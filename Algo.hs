module Algo where

import Data.Graph.Inductive
import Model
import Control.Lens
import Data.Maybe
import Data.Either
import qualified Data.Map as H

import qualified Data.Set as S


uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

type G = Gr (Integer, Hor) ()

type PM = H.Map Int Disc
type TODO = [(Disc, [Int])]

breakP p = case break (=='&') p of
    (p1, "") -> [p1]
    (p1, _:ps) -> p1:(breakP ps)

profProib :: String -> String -> Bool
profProib p1s p2s = any (\(a, b) -> a == b) [(p1, p2) | p1 <- breakP p1s, p2 <- breakP p2s]
        
color1n :: PM -> G -> Disc -> Int -> Maybe PM
color1n pm g c n = 
    if free 
    then 
        if any (corProib c) (catMaybes $ map ((flip H.lookup) pm) $ neighbors g n) 
        then Nothing
        else Just $ H.insert n c pm
    else Nothing
  where corProib (Disc p1 d1) (Disc p2 d2) = profProib p1 p2 -- && (p1 == p2)
        free = case H.lookup n pm of 
                    Just _ -> False 
                    Nothing -> True

color1ns :: PM -> G -> Disc -> [Int] -> Maybe PM
color1ns pm g c = 
    listToMaybe . catMaybes . map (color1n pm g c)

color :: PM -> G -> TODO -> Either (TODO, PM) PM
color pm g [] = Right pm
color pm g todo@((c, ns):cs) = case color1ns pm g c ns of 
    Just pm' -> color pm' g cs
    Nothing -> Left (todo, pm)

color' :: TODO -> PM -> G -> TODO -> Either (TODO, PM) PM
color' errs pm g todo = case color pm g todo of 
    Left (t:ts, pm) -> color' (t:errs) pm g ts
    Right x -> case errs of 
        [] -> Right x
        errs -> Left (errs, x)
    
genNodes :: Carga -> [LNode (Integer, Hor)]
genNodes c = zip [1..] (horariosFases $ c ^. fases)

genTodoProf ns (p, f1) = (p, [n | (n, (f2, _)) <- ns, f1 == f2])

genTodo :: Carga -> [LNode (Integer, Hor)] -> TODO
genTodo c ns = map (genTodoProf ns) ( c ^. profs )

allEdges ns = [(e1, e2) | e1 <- ns, e2 <- ns]
filterEdge (a@(n1, (f1, h1)), b@(n2, (f2, h2))) = 
        (h1 == h2) || manhaSeguinte h1 h2 || (f1 == f2 && consecutivos h1 h2)

makeQuadro :: PM -> G -> Quadro
makeQuadro pm g = Quadro [(h n g, f n g, p) | (n, p) <- H.toList pm]
    where h n g = snd $ fromJust $ lab g n :: Hor
          f n g = fst $ fromJust $ lab g n :: Integer


solve :: Carga -> Either ([Disc], Quadro) Quadro
solve c = q
    where ns = genNodes c
          es = filter filterEdge (allEdges ns)
          es' = map (\((n1, _), (n2, _)) -> (n1, n2, ())) es
          g = mkGraph ns es' :: G
          td = genTodo c ns
          q = case color' [] H.empty g td of
            Left (td, pm') -> Left (map fst td, makeQuadro pm' g)
            Right pm' -> Right $ makeQuadro pm' g
