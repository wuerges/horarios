module View where

import Model
import Control.Lens

toCsv [] = []
toCsv (s:[]) = s
toCsv (s:ss) = s ++ ", " ++ toCsv ss

prettyPrintErr :: ([Disc], Quadro) -> String
prettyPrintErr (ps, q) = 
    unlines $ profErr:(lines $ prettyPrint q)
        where profErr = "unallocated: " ++ toCsv (map sp ps)
              sp p = "(" ++ p ^. nome ++ ", " ++ p ^. prof ++")"

prettyPrint (Quadro t) =  
    unlines $ header:content
        where header = "dia, hora, fase, professor"
              content = map (\(a, b, c) -> toCsv [show (a ^. dia), 
                                                  show (a ^. hora), 
                                                  show b, 
                                                  c ^. prof,
                                                  c ^. nome]) t
