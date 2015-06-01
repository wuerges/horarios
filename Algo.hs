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

{-
type NL1 = (Hor, Prof)
type NL2 = (Fase, Hor, [Prof])

genPart1 :: Carga -> [LNode NL1]
genPart1 c = zip [1..] (do 
    a <- uniq (c ^. hor)
    b <- uniq (concat $ map snd $ c ^. fases)
    return (a, b))


genPart2 :: Carga -> [LNode NL2]
genPart2 c =  zip [1..] (do
    a <- uniq (map fst $ c ^. fases)
    b <- uniq (c ^. hor)
    return (a, b))

cargaEdge :: LNode NL1 -> LNode NL2 -> Maybe Edge
cargaEdge a@(n1, (f1, _)) b@(n2, (f2, h2))
    | (f1 == f2) = Just (n1, n2)
cargaEdge _ _ = Nothing


partEdges :: [LNode NL1] -> [LNode NL2] -> [Edge]
partEdges p1 p2 = catMaybes (do
    a <- p1
    b <- p2
    return $ cargaEdge a b)

cargaEdges :: Carga -> [Edge]
cargaEdges c = catMaybes (do
                    a <- genPart1 c
                    b <- genPart2 c
                    return $ cargaEdge a b)
                                    
data NL = P1 NL1
        | P2 NL2
        deriving Show

-}
type G = Gr Hor ()

{-
type Matching = (S.Set Node, S.Set Node, S.Set Edge)

directMatch :: Matching -> Edge -> Matching
directMatch m@(pa, pb, es) (n1, n2) = case (S.member n1 pa, S.member n2 pb) of
    (False, False) -> (S.insert n1 pa, S.insert n2 pb, S.insert (n1, n2) es)
    _ -> m


simpleMatching :: [Edge] -> Matching
simpleMatching l = foldl directMatch (S.empty, S.empty, S.empty) l


showMatching (_, _, es) = show $ S.toList es
-}


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
    

solve :: Carga -> Quadro
solve c = trace "" $ Quadro []
    --where ce = cargaEdges c
     --     sm = simpleMatching ce
      --    s = showMatching sm
          --as = [(n, p) | (n, (f, p)) <- genProfFase c]
          --bs = [(n, h, f) | (n, (h, f)) <- (genHorFase c)]
          --pp = show s ++ "\n" ++ show as ++ "\n" ++ show bs

          

