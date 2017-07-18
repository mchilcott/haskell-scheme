import Parser
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import Numeric

-- Error Handling
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

-- Error Show function
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

-- Error Types and management
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- Error helper function
trapError action = catchError action (return . show)

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
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val    -- Quoted

eval (List (Atom func : args)) = mapM eval args >>= apply func -- Other function

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm -- Catchall error

-- Apply a function atom
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args " func) ($ args) (lookup func primitives)

-- A list of primitive atoms
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return True
isNumber _ = return False

isCharacter :: [LispVal] -> ThrowsError LispVal
isCharacter [Character _] = return True
isCharacter _ = return False

isAtom :: [LispVal] -> ThrowsError LispVal
isAtom [Atom _] = return True
isAtom _ = return False

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return True
isString _ = return False

isBool :: [LispVal] -> ThrowsError LispVal
isBool [Bool _] = return True
isBool _ = return False

-- Apply a (Integer)  binary operator to a List
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

-- Unpack a number
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


-- Evaluating readExpr
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val


-- Main entry point
main :: IO ()
main = do
     args <- getArgs
     evaled <- return $liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $extractValue $ trapError evaled