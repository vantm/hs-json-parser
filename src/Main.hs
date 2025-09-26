{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Applicative
import Data.Char (isDigit)
import Text.Pretty.Simple (pPrint)
import Type.Reflection (splitApps)

data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonNumber Double
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

data JsonError
    = JsonParseError String
    deriving (Show, Eq)

newtype Parser a = Parser
    { runParser :: String -> (Either JsonError a, String)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input ->
        let (res, rest) = p input
         in (fmap f res, rest)

instance Applicative Parser where
    pure x = Parser (pure x,)

    (Parser pf) <*> (Parser p) = Parser $ \input ->
        let (mf, rest) = pf input
            (mx, finalRest) = p rest
         in (mf <*> mx, finalRest)

instance Alternative Parser where
    empty = Parser (Left (JsonParseError "empty"),)
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        let (res1, rest1) = p1 input
         in case res1 of
                Right _ -> (res1, rest1)
                Left _ -> p2 input

instance Monad Parser where
    return = pure
    Parser p >>= f = Parser $ \input ->
        case p input of
            (Right x, rest) ->
                let Parser p' = f x
                 in p' rest
            (Left err, rest) -> (Left err, rest)

charP :: Char -> Parser ()
charP c = Parser $ \input ->
    case input of
        (x : xs) | x == c -> (Right (), xs)
        _ -> (Left (JsonParseError $ "Expected '" ++ [c] ++ "' but found '" ++ take 1 input ++ "'"), input)

matchP :: String -> Parser String
matchP str = Parser $ \input ->
    case splitAt (length str) input of
        (pre, rest)
            | pre == str -> (Right pre, rest)
            | otherwise -> (Left (JsonParseError $ "Not matched " ++ str), input)

digitP :: Parser String
digitP = Parser $ \input -> do
    let (digits, rest) = span isDigit input
    case digits of
        "" -> (Left (JsonParseError "Not a number"), input)
        _ -> (Right digits, rest)

doubleP :: Parser Double
doubleP = do
    a <- digitP
    isDbl <- charP '.' *> pure True <|> pure False
    b <- if isDbl then digitP else pure "0"
    let s = (a ++ "." ++ b)
    let n = read s :: Double
    return $ n

escapeP :: String -> (String, String)
escapeP input = case input of
    '\\' : c : xs -> (['\\', c], xs)
    _ -> ("", input)

splitP :: String -> Int -> (String, String)
splitP "" _ = ("", "")
splitP input i =
    if i >= length input
        then (input, "")
        else case input !! i of
            ('\\') -> splitP input (i + 2)
            ('"') -> splitAt i input
            _ -> splitP input (i + 1)

testSplitP :: String -> IO ()
testSplitP s = do
    let r = splitP s 0
    print s
    print r

testSplit :: IO ()
testSplit = testSplitP "ab\\\"c\"def\"sdf"

stringP :: Parser String
stringP = Parser $ \input -> do
    let (content, rest) = splitP input 0
    (Right content, rest)

lstrip :: [Char] -> [Char]
lstrip = dropWhile (`elem` " \t\n\r")

rstrip :: [Char] -> [Char]
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip

ws :: Parser ()
ws = Parser $ \input -> (Right (), strip input)

keyP :: Parser String
keyP = ws *> charP '"' *> stringP <* charP '"'

kvpP :: Parser (String, JsonValue)
kvpP = do
    _ <- ws
    key <- keyP
    _ <- ws
    _ <- charP ':'
    value <- parser
    return (key, value)

objectContentP :: [(String, JsonValue)] -> Parser [(String, JsonValue)]
objectContentP acc = do
    _ <- ws
    value <- kvpP
    _ <- ws
    next <- (charP ',' *> pure True) <|> pure False
    let arr = acc ++ [value]
    if next then objectContentP arr else return arr

arrayContentP :: [JsonValue] -> Parser [JsonValue]
arrayContentP acc = do
    _ <- ws
    value <- parser
    next <- charP ',' *> pure True <|> pure False
    let arr = acc ++ [value]
    if next then arrayContentP arr else return arr

emptyP :: Char -> Parser Bool
emptyP c = Parser $ \input -> case lstrip input of
    (x : xs) | x == c -> (Right True, xs)
    _ -> (Right False, input)

parseNull :: Parser JsonValue
parseNull = do
    _ <- ws
    _ <- matchP "null"
    return JsonNull

parseBool :: Parser JsonValue
parseBool = do
    _ <- ws
    value <- pure (JsonBool True) <* matchP "true" <|> pure (JsonBool False) <* matchP "false"
    return value

parseNumber :: Parser JsonValue
parseNumber = do
    _ <- ws
    value <- (JsonNumber <$> doubleP) <|> (charP '-' *> (JsonNumber . negate <$> doubleP))
    return value

parseString :: Parser JsonValue
parseString = do
    _ <- ws
    _ <- charP '"'
    isEmpty <- emptyP '"'
    if isEmpty
        then return $ JsonString ""
        else do
            value <- JsonString <$> stringP
            _ <- charP '"'
            return value

parseObject :: Parser JsonValue
parseObject = do
    _ <- ws
    _ <- charP '{'
    isEmpty <- emptyP '}'
    if isEmpty
        then return $ JsonObject []
        else do
            _ <- ws
            value <- objectContentP []
            _ <- ws
            _ <- charP '}'
            _ <- ws
            return $ JsonObject value

parseArray :: Parser JsonValue
parseArray = do
    _ <- ws
    _ <- charP '['
    isEmpty <- emptyP ']'
    if isEmpty
        then return $ JsonArray []
        else do
            _ <- ws
            value <- arrayContentP []
            _ <- charP ']'
            pure $ JsonArray value

parser :: Parser JsonValue
parser =
    ws
        *> parseNull
        <|> parseArray
        <|> parseObject
        <|> parseNumber
        <|> parseBool
        <|> parseString

testP :: (Show a) => Parser a -> String -> IO ()
testP p s = pPrint $ runParser p s

test :: IO ()
test = readFile "./sample.json" >>= testP parser

testF :: FilePath -> IO ()
testF f = readFile f >>= testP parser

testRawF :: FilePath -> IO ()
testRawF f = do
    json <- readFile f
    let _ = runParser parser json
    print "Done!"

main :: IO ()
main = print "Hello World!"
