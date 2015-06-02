module Algo where

import Data.Graph.Inductive
import Model
import Control.Lens
import Data.Maybe
import Data.Either
import Debug.Trace
import qualified Data.Map as H

import qualified Data.Set as S


uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

--cargaEdge :: LNode NL1 -> LNode NL2 -> Maybe Edge
--cargaEdge a@(n1, (f1, _)) b@(n2, (f2, h2))
--   | (f1 == f2) = Just (n1, n2)
--cargaEdge _ _ = Nothing

type G = Gr (Fase, Hor) ()

type PM = H.Map Int Prof
type TODO = [(Prof, [Int])]

color1n :: PM -> G -> Prof -> Int -> Maybe PM
color1n pm g c n = 
    if any (c==) (catMaybes $ map ((flip H.lookup) pm) $ neighbors g n) then Nothing
    else Just $ H.insert n c pm

color1ns :: PM -> G -> Prof -> [Int] -> Maybe PM
color1ns pm g c = 
    listToMaybe . catMaybes . map (color1n pm g c)

color :: PM -> G -> TODO -> Either (PM, Prof) PM
color pm g [] = Right pm
color pm g ((c, ns):cs) = case color1ns pm g c ns of 
    Just pm' -> color pm' g cs
    Nothing -> Left (pm, c)
    
genNodes :: Carga -> [LNode (Fase, Hor)]
genNodes c = zip [1..] (horariosFases $ map fst $ c ^. fases)

allEdges ns = [(e1, e2) | e1 <- ns, e2 <- ns]
filterEdge (a@(n1, (f1, h1)), b@(n2, (f2, h2))) = 
        (h1 == h2) || manhaSeguinte h1 h2 || (f1 == f2 && consecutivos h1 h2)


solve :: Carga -> Quadro
solve c = trace (show pm') $ Quadro []
    where ns = genNodes c
          es = filter filterEdge (allEdges ns)
          es' = map (\((n1, _), (n2, _)) -> (n1, n2, ())) es
          g = mkGraph ns es' :: G
          pm' = color H.empty g [] 
    --where ce = cargaEdges c
     --     sm = simpleMatching ce
      --    s = showMatching sm
          --as = [(n, p) | (n, (f, p)) <- genProfFase c]
          --bs = [(n, h, f) | (n, (h, f)) <- (genHorFase c)]
          --pp = show s ++ "\n" ++ show as ++ "\n" ++ show bs

          

