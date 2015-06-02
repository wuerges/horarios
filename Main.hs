
import Model
import View
import Parser
import Algo
import System.Environment
import Control.Applicative
import Data.Either

main = 
    do 
       parse <- parseCSV <$> getContents
       case parse of 
           Left e -> putStrLn $ "Parse error: \n" ++ show e
           Right r -> case solve r of
                Left e -> putStr $ "Allocation Error: \n" ++ prettyPrintErr e
                Right s -> putStr $ prettyPrint s
