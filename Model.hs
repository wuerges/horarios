{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Lens
import Text.ParserCombinators.Parsec
import Data.Maybe

newtype Prof = Prof { _name :: String }
    deriving (Ord, Eq, Show)

data Hor = Hor { _dia :: Integer, _hora :: Integer }
    deriving (Ord, Eq, Show)


data Turno = Diurno | Noturno
    deriving (Ord, Eq, Show)

data Fase = Fase { _codfase :: Integer, _turno :: Turno }
    deriving (Ord, Eq, Show)

newtype Restr = Restr (Hor, Hor)
    deriving Show

data Carga = Carga { _restr ::[Restr], _fases :: [(Fase, [Prof])] }
    deriving Show 

emptyCarga = Carga [] []

data Quadro = Quadro [(Hor, Fase, Prof)]
    deriving Show 

$(makeLenses ''Prof)
$(makeLenses ''Hor)
$(makeLenses ''Fase)
$(makeLenses ''Restr)
$(makeLenses ''Carga)
$(makeLenses ''Quadro)

dias = [1..5]
horsTurno Diurno  = [Hor d h | d <- dias, h <- [7, 10]]
horsTurno Noturno = [Hor d h | d <- dias, h <- [19, 21]]

horariosFase f = [(f, h) | h <- horsTurno $ f ^. turno]
horariosFases = concat . map horariosFase

manhaSeguinte (Hor d1 h1) (Hor d2 h2) = d2 == d1 + 1 && h1 == 21 && h2 == 7

consecutivos (Hor d1 h1) (Hor d2 h2) = d1 == d2 && ((h1 == 7 && h2 == 10) || (h1 == 19 && h2 == 21))


--instance Monoid Carga where  
--    mempty = emptyCarga

