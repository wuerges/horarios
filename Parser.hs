module Parser where

import Model
import Control.Lens ((.~), (%~))
import Text.ParserCombinators.Parsec

import Debug.Trace


csvFile :: GenParser Char st Carga
csvFile = 
    do results <-  many line
       eof
       return $ foldl addResult emptyCarga results

data ResLine = P (Integer, Prof) | T (Integer, Turno)
    deriving Show

addResult :: Carga -> ResLine -> Carga
addResult c (P (i, p)) = addProf i p c 
addResult c (T (i, t)) = addTurno i t c

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st ResLine
line = 
    do 
       --c <- ( prof <|> parseHor <|> precolor <|> parseRestr )
       c <- ( prof <|> parseFase )
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

prof :: GenParser Char st ResLine
prof = 
    do
       identifier "prof"
       n <- number
       p <- quotedString
       return $ P (n, Prof p)

parseTurno :: GenParser Char st Turno
parseTurno =
    do 
       s <- string "Diurno" <|> string "Noturno"
       many (oneOf " ")
       case s of
           "Diurno" -> return Diurno
           "Noturno" -> return Noturno

parseFase :: GenParser Char st ResLine
parseFase =
    do 
       identifier "fase"
       n <- number
       t <- parseTurno
       return $ T (n, t)


{-parseHor :: GenParser Char st Carga
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
-}


-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError Carga
parseCSV input = parse csvFile "(unknown)" input
