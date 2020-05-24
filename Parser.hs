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
        , [binary "$" EApp E.AssocLeft]
        ]

expr :: Parser Expr
expr = whiteSpace *> E.buildExpressionParser table term
     <?> "Could not parse expression"

term :: Parser Expr
term = lexeme (
        try fun
    <|> try letP
    <|> try ifP
    <|> atoms
    <?> "Could not parse term")

atom :: Parser Expr
atom = lexeme (
        try (ECst  . fromInteger <$> (decimal <|> hexadecimal <|> octal))
    <|> EVar <$> identifier
    <|> braces expr
    <|> try (parens expr)
    <?> "Could not parse atom")

atoms :: Parser Expr
atoms = lexeme (
        foldl1 EApp <$> many1 atom
    <?> "Could not parse atoms")

fun :: Parser Expr
fun = lexeme (do
    lexeme $ reserved "fun"
    xs <- many1 $ lexeme identifier
    lexeme $ reserved "=>"
    e <- lexeme expr
    return $ foldr EFun e xs
    <?> "Could not parse fun") 

letP :: Parser Expr
letP = lexeme $ do
    lexeme $ reserved "let"
    xs <- many1 $ lexeme identifier
    lexeme $ reserved "="
    e1 <- lexeme expr
    cs <- many $ do
        lexeme $ reserved "and"
        xs <- many1 $ lexeme identifier
        lexeme $ reserverd "="
        e1 <- lexeme expr
    lexeme $ reserved "in"
    e2 <- lexeme expr
    return $ case xs of
        []     -> e2
        [x]    -> ELet x e1 e2
        (x:xs) -> ELet x (foldr EFun e1 xs) e2

ifP :: Parser Expr
ifP = lexeme $ do
    lexeme $ reserved "if"
    eb <- lexeme expr
    lexeme $ reserved "then"
    e1 <- lexeme expr
    lexeme $ reserved "else"
    e2 <- lexeme expr
    return $ EIte eb e1 e2

parseFromString :: String -> Either ParseError Expr
parseFromString = parse (expr <* eof) "<buffer>"

arith   name op       = E.Infix   ( EAex op <$ reservedOp name )
binary  name func     = E.Infix   ( func    <$ reservedOp name )
prefix  name func     = E.Prefix  ( func    <$ reservedOp name )
postfix name func     = E.Postfix ( func    <$ reservedOp name )