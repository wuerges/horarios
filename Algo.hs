module Algo where

import Data.Graph.Inductive
import Model
import Control.Lens
import Data.Maybe
import Debug.Trace

import qualified Data.Set as S

solve :: Carga -> Quadro
solve c = traceShow (cargaEdges c) $ Quadro []


uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

type NL1 = (Integer, [Prof], Fase, Hor)
type NL2 = (Fase, Hor)

genHorFase :: Carga -> [NL2]
genHorFase c =  do
    a <- uniq (map fst $ c ^. fases)
    b <- uniq (c ^. hor)
    return (a, b)


genProfHorFase :: Carga -> [NL1]
genProfHorFase c = do
    (n, (f, p)) <- zip [1..] $ c ^. fases
    h <- c ^. hor
    return (n, p, f, h)


cargaEdge :: NL1 -> NL2 -> Maybe (NL, NL)
cargaEdge a@(_, _, f1, h1) b@(f2, h2) 
    | (f1 == f2) && (h1 ^. codhor == h2 ^. codhor) = Just (P1 a, P2 b)
cargaEdge _ _ = Nothing

cargaEdges :: Carga -> [(NL, NL)]
cargaEdges c = catMaybes (do
                    a <- genProfHorFase c
                    b <- genHorFase c
                    return $ cargaEdge a b)
                                    

data NL = P1 NL1
        | P2 NL2
        deriving Show


type G = Gr NL ()

cargaGraph :: Carga -> G
cargaGraph = undefined

