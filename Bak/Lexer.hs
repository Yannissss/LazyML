module Lexer where

import           Text.Parsec
import           Text.Parsec.Char
import qualified Text.Parsec.Token as T

ravenDef :: T.LanguageDef st
ravenDef = T.LanguageDef
         { T.commentStart    = "{-"
         , T.commentEnd      = "-}"
         , T.commentLine     = "--"
         , T.nestedComments  = True
         , T.identStart      = lower <|> char '_'
         , T.identLetter     = alphaNum <|> oneOf "_'"
         , T.opStart         = T.opLetter ravenDef
         , T.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~;"
         , T.reservedOpNames = [":", "=>", "=", "\\", "->", "|"]
         , T.reservedNames   = [ "let", "in", "and"
                               , "fun"
                               , "if", "then", "else"
                               , "match", "with", "function"]
         , T.caseSensitive   = True
         }

lexer :: T.TokenParser st
lexer = T.makeTokenParser ravenDef

-- | This lexeme parser parses a legal identifier. Returns the identifier
-- string. This parser will fail on identifiers that are reserved
-- words. Legal identifier (start) characters and reserved words are
-- defined in the 'LanguageDef' that is passed to
-- 'makeTokenParser'. An @identifier@ is treated as
-- a single token using 'try'.
identifier       = T.identifier lexer

-- | The lexeme parser @reserved name@ parses @symbol
-- name@, but it also checks that the @name@ is not a prefix of a
-- valid identifier. A @reserved@ word is treated as a single token
-- using 'try'.
reserved         = T.reserved lexer

-- | This lexeme parser parses a legal operator. Returns the name of the
-- operator. This parser will fail on any operators that are reserved
-- operators. Legal operator (start) characters and reserved operators
-- are defined in the 'LanguageDef' that is passed to
-- makeTokenParser'. An @operator@ is treated as a
-- single token using 'try'.
operator         = T.operator lexer

-- |The lexeme parser @reservedOp name@ parses @symbol
-- name@, but it also checks that the @name@ is not a prefix of a
-- valid operator. A @reservedOp@ is treated as a single token using
-- 'try'.
reservedOp       = T.reservedOp lexer


-- | This lexeme parser parses a single literal character. Returns the
-- literal character value. This parsers deals correctly with escape
-- sequences. The literal character is parsed according to the grammar
-- rules defined in the Haskell report (which matches most programming
-- languages quite closely).
charLiteral      = T.charLiteral lexer

-- | This lexeme parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely).
stringLiteral    = T.stringLiteral lexer

-- | This lexeme parser parses a natural number (a positive whole
-- number). Returns the value of the number. The number can be
-- specified in 'decimal', 'hexadecimal' or
-- 'octal'. The number is parsed according to the grammar
-- rules in the Haskell report.
natural          = T.natural lexer

-- | This lexeme parser parses an integer (a whole number). This parser
-- is like 'natural' except that it can be prefixed with
-- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
-- number can be specified in 'decimal', 'hexadecimal'
-- or 'octal'. The number is parsed according
-- to the grammar rules in the Haskell report.
integer          = T.integer lexer

-- | This lexeme parser parses a floating point value. Returns the value
-- of the number. The number is parsed according to the grammar rules
-- defined in the Haskell report.
float            = T.float lexer

-- | This lexeme parser parses either 'natural' or a 'float'.
-- Returns the value of the number. This parsers deals with
-- any overlap in the grammar rules for naturals and floats. The number
-- is parsed according to the grammar rules defined in the Haskell report.
naturalOrFloat   = T.naturalOrFloat lexer

-- | Parses a non-negative whole number in the decimal system. Returns the
-- value of the number.
decimal          = T.decimal lexer

-- | Parses a non-negative whole number in the hexadecimal system. The
-- number should be prefixed with \"x\" or \"X\". Returns the value of the
-- number.
hexadecimal      = T.hexadecimal lexer

-- | Parses a non-negative whole number in the octal system. The number
-- should be prefixed with \"o\" or \"O\". Returns the value of the
-- number.
octal            = T.octal lexer

-- | Lexeme parser @symbol s@ parses 'string' @s@ and skips
-- trailing white space.
symbol           = T.symbol lexer

-- | @lexeme p@ first applies parser @p@ and then the 'whiteSpace'
-- parser, returning the value of @p@. Every lexical
-- token (lexeme) is defined using @lexeme@, this way every parse
-- starts at a point without white space. Parsers that use @lexeme@ are
-- called /lexeme/ parsers in this document.
--
-- The only point where the 'whiteSpace' parser should be
-- called explicitly is the start of the main parser in order to skip
-- any leading white space.
--
-- >    mainParser  = do{ whiteSpace
-- >                     ; ds <- many (lexeme digit)
-- >                     ; eof
-- >                     ; return (sum ds)
-- >                     }
lexeme           = T.lexeme lexer

-- | Parses any white space. White space consists of /zero/ or more
-- occurrences of a 'space', a line comment or a block (multi
-- line) comment. Block comments may be nested. How comments are
-- started and ended is defined in the 'LanguageDef'
-- that is passed to 'makeTokenParser'.
whiteSpace       = T.whiteSpace lexer

-- | Lexeme parser @parens p@ parses @p@ enclosed in parenthesis,
-- returning the value of @p@.
parens           = T.parens lexer

-- | Lexeme parser @braces p@ parses @p@ enclosed in braces (\'{\' and
-- \'}\'), returning the value of @p@.
braces           = T.braces lexer

-- | Lexeme parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
-- and \'>\'), returning the value of @p@.

angles           = T.angles lexer

-- | Lexeme parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
-- and \']\'), returning the value of @p@.
brackets         = T.brackets lexer

-- | DEPRECATED: Use 'brackets'.
squares          = T.squares lexer

-- | Lexeme parser |semi| parses the character \';\' and skips any
-- trailing white space. Returns the string \";\".
semi             = T.semi lexer

-- | Lexeme parser @comma@ parses the character \',\' and skips any
-- trailing white space. Returns the string \",\".
comma            = T.comma lexer

-- | Lexeme parser @colon@ parses the character \':\' and skips any
-- trailing white space. Returns the string \":\".
colon            = T.colon lexer

-- | Lexeme parser @dot@ parses the character \'.\' and skips any
-- trailing white space. Returns the string \".\".
dot              = T.dot lexer

-- | Lexeme parser @semiSep p@ parses /zero/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by
-- @p@.
semiSep          = T.semiSep lexer

-- | Lexeme parser @semiSep1 p@ parses /one/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by @p@.
semiSep1         = T.semiSep1 lexer

-- | Lexeme parser @commaSep p@ parses /zero/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@.

commaSep         = T.commaSep lexer

-- | Lexeme parser @commaSep1 p@ parses /one/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@.
commaSep1        = T.commaSep1 lexer