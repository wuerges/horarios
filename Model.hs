{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Lens
import Text.ParserCombinators.Parsec
import Data.Maybe
import qualified Data.Map as H

newtype Prof = Prof { _name :: String }
    deriving (Ord, Eq, Show)

data Hor = Hor { _dia :: Integer, _hora :: Integer }
    deriving (Ord, Eq, Show)

data Turno = Diurno | Noturno
    deriving (Ord, Eq, Show)
    
type FaseMap = H.Map Integer (Turno, [Prof])

newtype Restr = Restr (Hor, Hor)
    deriving Show

data Carga = Carga { _restr ::[Restr], _fases :: FaseMap }
    deriving Show 

emptyCarga = Carga [] H.empty

data Quadro = Quadro [(Hor, Integer, Prof)]
    deriving Show 

$(makeLenses ''Prof)
$(makeLenses ''Hor)
$(makeLenses ''Restr)
$(makeLenses ''Carga)
$(makeLenses ''Quadro)

dias = [1..5]
horsTurno Diurno  = [Hor d h | d <- dias, h <- [7, 10]]
horsTurno Noturno = [Hor d h | d <- dias, h <- [19, 21]]

horariosFases fm = concat [[(c, h) | h <- horsTurno t] | (c, (t, _)) <- H.toList fm]

manhaSeguinte (Hor d1 h1) (Hor d2 h2) = d2 == d1 + 1 && h1 == 21 && h2 == 7

consecutivos (Hor d1 h1) (Hor d2 h2) = d1 == d2 && ((h1 == 7 && h2 == 10) || (h1 == 19 && h2 == 21))

addProf :: Integer -> Prof -> Carga -> Carga
addProf f p c = fases %~ H.update (\(t, ps) -> Just (t, (p:ps))) f  $ c

addTurno :: Integer -> Turno -> Carga -> Carga
addTurno f t c = fases %~ H.update (\(_, ps) -> Just (t, ps)) f  $ c


getProfs :: Integer -> Carga -> [Prof]
getProfs f c = snd $ fromMaybe (Diurno,[]) $ H.lookup f (c ^. fases)

--instance Monoid Carga where  
--    mempty = emptyCarga

