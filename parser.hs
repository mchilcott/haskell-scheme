import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

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
    Right val -> "Found value"

-- Different Classes of Scheme Data
data LispVal = Atom String                    -- Atom (e.g. identifier)
     | List [LispVal]                         -- Normal List
     | DottedList [LispVal] LispVal           -- (a b . c) - an improper list
     | Number Integer                         -- Numeric Type
     | String String                          -- String literal
     | Bool Bool                              -- Boolean Value
     | Character Char                         -- Character Literal

-- Parse a string literal
parseString :: Parser LispVal
parseString = do
                -- Expect string literal to start with "
                char '"'
                -- And to finish when it finds an unescaped "
                x <- manyTill anyChar (try (noneOf ("\\") >> char '\"'))

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

-- Number Parser - This version uses function to Monad conversion
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- Number Parser using do notation - Broken
--parseNumber :: Parser LispVal
--parseNumber = do
--            x <- many1 (digit)
--            [(a,_)] <- readDec (x)
--            return a

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
parseExpr = parseCharacter
          <|> parseAtom
          <|> parseString
          <|> parseNumber


-- Program Entry Point
main :: IO ()
main = do 
    (expr:_) <- getArgs
    putStrLn (expr)
    putStrLn (readExpr expr)

