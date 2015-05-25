module Parser where

import Model
import Control.Lens ((.~), (%~))
import Text.ParserCombinators.Parsec


csvFile :: GenParser Char st Carga
csvFile = 
    do result <- many line
       eof
       return $ mconcat result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st Carga
line = 
    do c <- ( prof <|> parseHor <|> precolor <|> parseRestr )
       eol                       -- end of line
       return c
       

quotedString :: GenParser Char st String
quotedString = do
    char '"'
    x <- (many $ noneOf "\"")
    char '"'
    many (oneOf " ")
    return x

number :: GenParser Char st Integer 
number = do
    x <- many (oneOf ['0'..'9'])
    many (oneOf " ")
    return (read x :: Integer)

identifier :: String -> GenParser Char st String 
identifier s = do
    r <- string s
    many (oneOf " ")
    return r

prof :: GenParser Char st Carga
prof = 
    do
       identifier "prof"
       n <- number
       ss <- many quotedString 
       return $ fases %~ ((Fase n, map Prof ss):) $  emptyCarga   

parseHor :: GenParser Char st Carga
parseHor = do
    identifier "hor"
    n <- number 
    s <- quotedString
    return $ hor %~ (Hor n (Just s):) $ emptyCarga

precolor :: GenParser Char st Carga
precolor = do
    identifier "color"
    number 
    number
    return emptyCarga

parseRestr :: GenParser Char st Carga
parseRestr = do
    identifier "restr"
    n1 <- number
    n2 <- number
    return $ restr %~ (Restr (Hor n1 Nothing, Hor n2 Nothing) :) $ emptyCarga


-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError Carga
parseCSV input = parse csvFile "(unknown)" input
