import Parser
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric


-- A more lisp like show function for use with evaluator
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

-- Main evaluation
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val    -- Quoted

eval (List (Atom func : args)) = apply func $ map eval args  -- Other function


-- Apply a function atom
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

-- A list of primitive atoms
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("number?", isNumber),
              ("char?", isCharacter),
              ("string?", isString),
              ("symbol?", isAtom),
              ("boolean?", isBool)
              ]

isNumber :: [LispVal] -> LispVal
isNumber [Number _] = Bool True
isNumber _ = Bool False

isCharacter :: [LispVal] -> LispVal
isCharacter [Character _] = Bool True
isCharacter _ = Bool False

isAtom :: [LispVal] -> LispVal
isAtom [Atom _] = Bool True
isAtom _ = Bool False

isString :: [LispVal] -> LispVal
isString [String _] = Bool True
isString _ = Bool False

isBool :: [LispVal] -> LispVal
isBool [Bool _] = Bool True
isBool _ = Bool False

-- Apply a (Integer)  binary operator to a List
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

-- Unpack a number
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0


-- Evaluating readExpr
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val


-- Main entry point
main :: IO ()
main = getArgs >>= print . eval . readExpr . head