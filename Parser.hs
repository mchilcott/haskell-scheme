--module Parser
--(
--LispVal (..),
--parseExpr
--)
--where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char



-- Symbol Parser
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Parser to absorb spaces
spaces :: Parser ()
spaces = skipMany1 space 

-- Read an expression
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    -- Left value is error
    Left err -> "No match: " ++ show err
    -- Right value is LispVal parsed
    Right val -> "Found value " ++ showVal val

-- Different Classes of Scheme Data
data LispVal = Atom String                    -- Atom (e.g. identifier)
     | List [LispVal]                         -- Normal List
     | DottedList [LispVal] LispVal           -- (a b . c) - an improper list
     | Number Integer                         -- Numeric Type
     | String String                          -- String literal
     | Bool Bool                              -- Boolean Value
     | Character Char                         -- Character Literal


showVal :: LispVal -> String
showVal (String contents) = "String " ++ show contents
showVal (Atom name) ="Atom " ++ show name
showVal (Number contents) = "Number " ++ show contents
showVal (Bool True) = "Bool True"
showVal (Bool False) = "Bool False"
showVal (List contents) = "List [" ++ unwordsList contents ++ "]"
showVal (DottedList head tail) = "DottedList [" ++ unwordsList head ++ "] (" ++ showVal tail ++ ")"
showVal (Character c) = "Character " ++ show c

stitch :: String -> [String] -> String
stitch x [] = ""
stitch x [y] = y
stitch x (y:ys) = y ++ x ++ stitch x ys

unwordsList :: [LispVal] -> String
unwordsList x = stitch "," (map showVal x)


-- Parse and covert an escaped character in a string
-- This is incomplete, and replaces any unknown char
-- with a !
parseEscapeChar :: Parser Char
parseEscapeChar = do
  char '\\'
  x <- oneOf("\"\'\\nrt")
  return $ case x of
    '"' -> '"'
    '\'' -> '\''
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'
    '\\' -> '\\'

-- Parse a string literal
parseString :: Parser LispVal
parseString = do
                -- Expect string literal to start with "
                char '"'
                -- And to finish when it finds an unescaped "
                x <- manyTill
                  (try (parseEscapeChar) <|> anyChar)
                  (char '"')

                return $ String x

-- Parse an atom
parseAtom :: Parser LispVal
parseAtom = do
          
          first <- letter <|> symbol
          rest <- many (letter <|> digit <|> symbol)
          let atom = first:rest
          return $ case atom of 
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseNumberPlain
               <|> parseNumberBase

-- Plain Number Parser - This version uses function to Monad conversion
parseNumberPlain :: Parser LispVal
parseNumberPlain = liftM (Number . read) $ many1 digit

-- analogous to isDigit, isOctdigit, isHexDigit
isBinDigit :: Char -> Bool
isBinDigit c = (c == '0' || c == '1')

-- Number Parser for numbers prefixed with a base
parseNumberBase :: Parser LispVal
parseNumberBase = do
           char '#'
           x <- oneOf("bodx")
           let reader = case x of
                 'b' -> fst . head . (readInt 2 isBinDigit digitToInt)
                 'o' -> fst . head . readOct
                 'd' -> fst . head . readDec
                 'x' -> fst . head . readHex
           let parse = case x of
                 'b' -> oneOf("10")
                 'o' -> oneOf("01234567")
                 'd' -> digit
                 'x' -> digit <|> oneOf("ABCDEFabcdef")
           (liftM (Number . reader) $ many1 parse) -- No return - it's actually a monad thing
           
-- Parse a character literal
parseCharacter :: Parser LispVal
parseCharacter = do
    char '#'
    char '\\'
    first <- anyChar
    rest <- many (letter)
    let character = first:rest
    return $ case character of 
         "space"   -> Character ' '
         "newline" -> Character '\n'
         _         -> Character first

-- Parse a list
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- Parse a dotted list
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- Parse a quoted item. This includes unqoute and quasiquote
parseQuoted :: Parser LispVal
parseQuoted = do
    y <- oneOf("'`,")
    let quoteType = case y of
                    '\'' -> "quote"
                    '`'  -> "quasiquote"
                    ','  -> "unquote"
    x <- parseExpr
    return $ List [Atom quoteType, x]


-- Expression Parser - Top level parser
parseExpr :: Parser LispVal
parseExpr = (try parseCharacter)
          <|> (try parseNumber)
          <|> parseAtom
          <|> parseString
          <|> parseQuoted
          <|> do char '('
                 x <- try parseList <|> parseDottedList
                 char ')'
                 return x


-- Program Entry Point
main :: IO ()
main = do 
    (expr:_) <- getArgs
    putStrLn (expr)
    putStrLn (readExpr expr)

