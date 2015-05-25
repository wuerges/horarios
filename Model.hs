{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Lens
import Text.ParserCombinators.Parsec
import Data.Maybe

newtype Prof = Prof { _name :: String }
    deriving (Ord, Eq, Show)
data Hor = Hor { _codhor :: Integer , _descr :: Maybe String }
    deriving (Ord, Eq, Show)
newtype Fase = Fase { _codfase :: Integer }
    deriving (Ord, Eq, Show)

newtype Restr = Restr (Hor, Hor)
    deriving Show

data Carga = Carga { _fases ::[(Fase, [Prof])], _hor :: [Hor],  _restr ::[Restr] }
    deriving Show 


emptyCarga = Carga [] [] []



data Quadro = Quadro [(Hor, Fase, Prof)]
    deriving Show 



$(makeLenses ''Prof)
$(makeLenses ''Hor)
$(makeLenses ''Fase)
$(makeLenses ''Restr)
$(makeLenses ''Carga)
$(makeLenses ''Quadro)


instance Monoid Carga where  
    mempty = emptyCarga
    mappend (Carga fs1 hs1 rs1) (Carga fs2 hs2 rs2) = Carga (fs1 ++ fs2) (hs1 ++ hs2) (rs1 ++ rs2)
