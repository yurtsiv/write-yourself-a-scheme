module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Char (digitToInt)
import Data.List (foldl')

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr $ args !! 0)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]  
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Rational (Integer, Integer)
             | Complex (Double, Double)
             | Character Char
             | String String
             | Bool Bool
             deriving (Show)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> many1 (noneOf "\"\\")
                 char '"'
                 return $ String (concat x)

escapedChars :: Parser String
escapedChars = do char '\\'
                  c <- oneOf "\"\\nrt"
                  return $ case c of
                     '\\' -> "\\"
                     '\"' -> "\""
                     'n' -> "\n"
                     'r' -> "\r"
                     't' -> "\t"

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ Atom atom

parseBoolean :: Parser LispVal
parseBoolean = do char '#'
                  c <- oneOf "ft"
                  return $ case c of
                    't' -> Bool True
                    'f' -> Bool False
    
parseNumber :: Parser LispVal
parseNumber = try parseDecNum
          <|> try parseBinNum
          <|> try parseHexNum 
          <|> try parseOctNum

parseDecNum :: Parser LispVal
parseDecNum = try parseDecNum1 <|> try parseDecNum2

parseDecNum1 :: Parser LispVal
parseDecNum1 = do x <- many1 digit
                  return $ (Number . read) x

parseDecNum2 :: Parser LispVal
parseDecNum2 = string "#d" >> parseDecNum1
  
parseBinNum :: Parser LispVal
parseBinNum = do string "#b"
                 b <- many1 (oneOf "01")
                 return $ Number (readBin b)

parseOctNum :: Parser LispVal
parseOctNum = do string "#o"
                 x <- many1 octDigit
                 return $ Number (readOct' x)

parseHexNum :: Parser LispVal
parseHexNum = do string "#x"
                 x <- many1 hexDigit
                 return $ Number (readHex' x)

parseCharacter :: Parser LispVal
parseCharacter = do string "#\\"
                    c <- parseCharName <|> anyChar
                    return $ Character c

parseCharName :: Parser Char
parseCharName = do x <- try (string "space" <|> string "newline")
                   case x of
                     "space" -> do return ' '
                     "newline" -> do return '\n'

parseFloat :: Parser LispVal
parseFloat = do i <- many1 digit
                char '.'
                f <- many1 digit
                return $ Float $ readFloat' (i ++ "." ++ f)
  
parseRational :: Parser LispVal
parseRational = do e <- many1 digit
                   char '/'
                   d <- many1 digit
                   return $ Rational (read e, read d)

parseComplex :: Parser LispVal
parseComplex = do x <- try parseFloat <|> parseDecNum1
                  s <- oneOf "+-"
                  y <- try parseFloat <|> parseDecNum1
                  char 'i'
                  return $ case s of
                            '+' -> Complex (lispValToDouble x, lispValToDouble y)
                            '-' -> Complex (lispValToDouble x, - (lispValToDouble y))

parseExpr :: Parser LispVal
parseExpr = try parseAtom
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRational
        <|> try parseNumber
        <|> try parseString
        <|> try parseCharacter
        <|> try parseBoolean

lispValToDouble :: LispVal -> Double
lispValToDouble (Float x) = x
lispValToDouble (Number x) = fromIntegral x

readBin :: String -> Integer
readBin = toInteger . (foldl' (\acc x -> acc * 2 + (digitToInt x)) 0)

readOct' :: String -> Integer
readOct' = (fst . (!! 0) . readOct)

readHex' :: String -> Integer
readHex' = (fst . (!! 0) . readHex)

readFloat' :: String -> Double
readFloat' = (fst . (!! 0) . readFloat)