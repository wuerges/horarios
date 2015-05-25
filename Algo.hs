module Algo where

import Data.Graph.Inductive
import Model
import Control.Lens
import Data.Maybe
import Debug.Trace

import qualified Data.Set as S


uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

type NL1 = (Fase, [Prof])
type NL2 = (Fase, Hor)

genHorFase :: Carga -> [LNode NL2]
genHorFase c =  zip [1..] (do
    a <- uniq (map fst $ c ^. fases)
    b <- uniq (c ^. hor)
    return (a, b))


genProfFase :: Carga -> [LNode NL1]
genProfFase c = zip [1..] (c ^. fases)


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
                    a <- genProfFase c
                    b <- genHorFase c
                    return $ cargaEdge a b)
                                    
data NL = P1 NL1
        | P2 NL2
        deriving Show


type G = Gr NL ()


type Matching = (S.Set Node, S.Set Node, S.Set Edge)

directMatch :: Matching -> Edge -> Matching
directMatch m@(pa, pb, es) (n1, n2) = case (S.member n1 pa, S.member n2 pb) of
    (False, False) -> (S.insert n1 pa, S.insert n2 pb, S.insert (n1, n2) es)
    _ -> m


simpleMatching :: [Edge] -> Matching
simpleMatching l = foldl directMatch (S.empty, S.empty, S.empty) l


showMatching (_, _, es) = show $ S.toList es



solve :: Carga -> Quadro
solve c = traceShow (s) $ Quadro []
    where ce = cargaEdges c
          sm = simpleMatching ce
          s = showMatching sm
          a = genProfFase c
          b = genHorFase c

          

