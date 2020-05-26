module Parser where

import           Control.Applicative ((*>))

import           Data.Functor (($>), (<$))
import           Data.Set (size)

import           Text.Parsec
import qualified Text.Parsec.Expr as E
import           Text.Parsec.String
import           Text.Parsec.Char
import           Text.Parsec.Combinator

import           Expr
import           Lexer

table = [ [arith "^" Pow E.AssocLeft]
        , [arith "*" Mul E.AssocLeft , arith "/" Div E.AssocLeft, arith "%" Mod E.AssocLeft]
        , [arith "+" Add E.AssocLeft , arith "-" Min E.AssocLeft]
        , [arith "=" Eq E.AssocLeft, arith "<>" Df E.AssocLeft, 
           arith "<=" Leq E.AssocLeft, arith ">=" Geq E.AssocLeft,
           arith "<" Le E.AssocLeft, arith ">" Ge E.AssocLeft ]
        , [arith "&&" And E.AssocLeft, arith "||" Or E.AssocLeft]
        , [prefix "-" $ EAex Min (ECst 0)]
        , [binary "$" EApp E.AssocRight]
        , [binary "::" ECons E.AssocRight]
        ]

program :: Parser Program
program = do
    decls <- many uletP
    expr <- optionMaybe expr
    return (decls, expr)

expr :: Parser Expr
expr = whiteSpace *> E.buildExpressionParser table term
     <?> "Could not parse expression"

term :: Parser Expr
term = lexeme (
        try fncP
    <|> try fun
    <|> try letP
    <|> try ifP
    <|> try matchP
    <|> atoms
    <?> "Could not parse term")

patternP :: Parser Pattern
patternP = whiteSpace *> E.buildExpressionParser [
        [E.Infix (PCons <$ reservedOp "::") E.AssocRight]
    ] (lexeme (do
        xs <- identifier
        if xs == "_"
            then return PWdc 
            else return $ PVar xs)
    <|> try (parens patternP)
    <|> lexeme (PCpl <$> parens (commaSep1 patternP))
    <|> lexeme (PNil <$ string "[]"))
    <?> "Could not parse pattern"

atom :: Parser Expr
atom = lexeme (
        try (ECst  . fromInteger <$> (decimal <|> hexadecimal <|> octal))
    <|> ENil <$ reservedOp "[]"
    <|> EVar <$> identifier
    <|> braces expr
    <|> try (parens expr)
    <|> (ECpl <$> parens (commaSep1 expr))
    <|> (foldr ECons ENil <$> brackets (commaSep1 expr))
    <?> "Could not parse atom")

atoms :: Parser Expr
atoms = lexeme (
        foldl1 EApp <$> many1 atom
    <?> "Could not parse atoms")

fun :: Parser Expr
fun = lexeme (do
    lexeme $ reserved "fun"
    xs <- many1 $ lexeme patternP
    lexeme $ reserved "=>"
    e <- lexeme expr
    return $ foldr EFun e xs
    <?> "Could not parse fun") 

letP :: Parser Expr
letP = lexeme $ do
    lexeme $ reserved "let"
    x1 <- many1 $ lexeme patternP
    lexeme $ reserved "="
    e1 <- lexeme expr
    cs <- many (do
        lexeme $ reserved "and"
        xs <- many1 $ lexeme patternP
        lexeme $ reserved "="
        e <- lexeme expr
        foldFun xs e)
    lexeme $ reserved "in"
    e2 <- lexeme expr
    c@(x,e) <- foldFun x1 e1
    case cs of
        [] -> if varsOfPattern x `encounter` varsOfExpr e
            then return $ ELet x (EFix $ EFun x e) e2
            else return $ ELet x e e2
        _ -> return $ translate (c:cs) e2
    where foldFun [] _     = fail "letP: Empty let declaration"
          foldFun [x] e    = return (x, e)
          foldFun (x:xs) e = return (x, foldr EFun e xs)
          encounter x y    = any (`elem` y) x
          translate cs e2 = let (ps, es) = unzip cs in 
              if varsOfPattern (PCpl ps) `encounter` varsOfExpr (ECpl es)
                  then ELet (PCpl ps) (EFix $ EFun (PCpl ps) (ECpl es)) e2
                  else ELet (PCpl ps) (ECpl es) e2

uletP :: Parser (Pattern, Expr)
uletP = lexeme $ do
    lexeme $ reserved "let"
    x1 <- many1 $ lexeme patternP
    lexeme $ reserved "="
    e1 <- lexeme expr
    cs <- many (do
        lexeme $ reserved "and"
        xs <- many1 $ lexeme patternP
        lexeme $ reserved "="
        e <- lexeme expr
        foldFun xs e)
    lexeme $ reserved ";;"
    c@(x,e) <- foldFun x1 e1
    case cs of
        [] -> if varsOfPattern x `encounter` varsOfExpr e
            then return $ (x, EFix $ EFun x e)
            else return $ (x, e)
        _ -> return $ translate (c:cs)
    where foldFun [] _     = fail "letP: Empty let declaration"
          foldFun [x] e    = return (x, e)
          foldFun (x:xs) e = return (x, foldr EFun e xs)
          encounter x y    = any (`elem` y) x
          translate cs = let (ps, es) = unzip cs in 
              if varsOfPattern (PCpl ps) `encounter` varsOfExpr (ECpl es)
                  then (PCpl ps, EFix $ EFun (PCpl ps) (ECpl es))
                  else (PCpl ps, ECpl es)

ifP :: Parser Expr
ifP = lexeme $ do
    lexeme $ reserved "if"
    eb <- lexeme expr
    lexeme $ reserved "then"
    e1 <- lexeme expr
    lexeme $ reserved "else"
    e2 <- lexeme expr
    return $ EIte eb e1 e2

matchP :: Parser Expr
matchP = lexeme $ do
    lexeme $ reserved "match"
    e0 <- lexeme expr
    lexeme $ reserved "with"
    cs <- many1 $ lexeme $ do
        lexeme $ reservedOp "|"
        m <- patternP
        lexeme $ string "->"
        e <- expr
        return (m, e)
    return $ EMatch e0 cs

fncP :: Parser Expr
fncP = lexeme $ do
    lexeme $ reserved "function"
    cs <- many1 $ lexeme $ do
        lexeme $ reservedOp "|"
        m <- patternP
        lexeme $ string "->"
        e <- expr
        return (m, e)
    let n = sum . map (length . varsOfExpr . snd) $ cs
        v = "&" ++ show n
    return $ EFun (PVar v) (EMatch (EVar v) cs)

parseFromString :: String -> Either ParseError Expr
parseFromString = parse (expr <* eof) "<buffer>"

arith   name op       = E.Infix   ( EAex op <$ reservedOp name )
binary  name func     = E.Infix   ( func    <$ reservedOp name )
prefix  name func     = E.Prefix  ( func    <$ reservedOp name )
postfix name func     = E.Postfix ( func    <$ reservedOp name )