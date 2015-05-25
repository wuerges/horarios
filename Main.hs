
import Model
import Parser
import Algo
import System.Environment
import Control.Applicative
import Data.Either

main = 
    do 
       parse <- parseCSV <$> getContents
       case parse of 
           Left e -> putStrLn $ show e
           Right e -> putStrLn $ show $ solve e
