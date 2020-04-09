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
             | Character Char
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedSymbol <|> noneOf "\"")
                 char '"'
                 return $ String x

escapedSymbol :: Parser Char
escapedSymbol = do char '\\'
                   escapedChar <- char '\"'
                   return escapedChar

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

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseCharacter
        <|> parseBoolean

readBin :: String -> Integer
readBin = toInteger . (foldl' (\acc x -> acc * 2 + (digitToInt x)) 0)

readOct' :: String -> Integer
readOct' = (fst . (!! 0) . readOct)

readHex' :: String -> Integer
readHex' = (fst . (!! 0) . readHex)
