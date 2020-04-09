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
  Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]  
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Character Char
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ many1 (noneOf "\"\\") <|> escapedChars
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
parseNumber = parseDecNum <|> parseBinNum <|> parseHexNum <|> parseOctNum

parseDecNum :: Parser LispVal
parseDecNum = parseDecNum1 <|> parseDecNum2

parseDecNum1 :: Parser LispVal
parseDecNum1 = do x <- many1 digit
                  return $ (Number . read) x

parseDecNum2 :: Parser LispVal
parseDecNum2 = try $ (string "#d") >> parseDecNum1
  
parseBinNum :: Parser LispVal
parseBinNum = do try $ string "#b"
                 b <- many1 (oneOf "01")
                 return $ Number (readBin b)

parseOctNum :: Parser LispVal
parseOctNum = do try $ string "#o"
                 x <- many1 octDigit
                 return $ Number (readOct' x)

parseHexNum :: Parser LispVal
parseHexNum = do try $ string "#x"
                 x <- many1 hexDigit
                 return $ Number (readHex' x)

parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
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

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseFloat
        <|> parseNumber
        <|> parseCharacter
        <|> parseBoolean

readBin :: String -> Integer
readBin = toInteger . (foldl' (\acc x -> acc * 2 + (digitToInt x)) 0)

readOct' :: String -> Integer
readOct' = (fst . (!! 0) . readOct)

readHex' :: String -> Integer
readHex' = (fst . (!! 0) . readHex)

readFloat' :: String -> Double
readFloat' = (fst . (!! 0) . readFloat)