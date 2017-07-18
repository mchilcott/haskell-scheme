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
    Right val -> "Found value " ++ show val

-- Different Classes of Scheme Data
data LispVal = Atom String                    -- Atom (e.g. identifier)
     | List [LispVal]                         -- Normal List
     | DottedList [LispVal] LispVal           -- (a b . c) - an improper list
     | Number Integer                         -- Numeric Type
     | String String                          -- String literal
     | Bool Bool                              -- Boolean Value
     | Character Char                         -- Character Literal
     deriving (Show)
                 
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
--               <|> parseNumberBase

-- Plain Number Parser - This version uses function to Monad conversion
parseNumberPlain :: Parser LispVal
parseNumberPlain = liftM (Number . read) $ many1 digit

-- analogous to isDigit, isOctdigit, isHexDigit
isBinDigit :: Char -> Bool
isBinDigit c = (c == '0' || c == '1')

-- Number Parser for numbers prefixed with a base
--parseNumberBase :: Parser LispVal
--parseNumberBase = do
--           char '#'
--           x <- oneOf("bodx")
--           y <- many1(digit)
--           return $ case x of
--              'b' -> fst (head (readInt 2 isBinDigit digitToInt y))
--              'o' -> fst (head (readOct y))
--              'd' -> fst (head (readDec y))
--              'x' -> fst (head (readHex y))

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
     

-- Expression Parser - Top level parser
parseExpr :: Parser LispVal
parseExpr = (try parseCharacter)
          <|> parseAtom
          <|> parseString
          <|> parseNumber


-- Program Entry Point
main :: IO ()
main = do 
    (expr:_) <- getArgs
    putStrLn (expr)
    putStrLn (readExpr expr)

