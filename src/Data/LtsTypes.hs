{-# LANGUAGE NoMonomorphismRestriction,TemplateHaskell #-}
module Data.LtsTypes where

import           Control.Applicative hiding (empty)
import           Control.Lens        hiding (Action,Choice)
import           Control.Monad
import           Data.Map            (Map, (!))
import qualified Data.Map            as M
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as S

-- Lts types

type Variable = String
data Item = ItemInt Int | ItemStr String deriving (Eq,Ord,Read,Show)
type Collection = [Item]
data Binding = Binding { _varName :: Variable
                       , _varValue :: Collection } deriving (Eq,Ord,Read,Show)
data Decoration = DecVar String
                | DecValue Int
                | DecColl Collection
                | DecBind Binding
                  deriving (Eq,Ord,Read,Show)
data Name = Name { _name :: String
                 , _annotation  :: [Decoration]
                 , _indices :: [Decoration] } deriving (Eq,Ord,Read,Show)
type Action = Name
data PName = PSingle Name | PChoice [PName] | PAct [Action] PName deriving (Eq,Ord,Read,Show)
type Process = Set PName
type Epsilon = (Process, Process)
type Colour = Set Process
type Colouring = Set Colour

data Arc = Arc { _destination :: Process
               , _label :: Action }
           deriving (Eq,Ord,Read,Show)

newtype Lts = Lts { _lts :: Map Process (Set Arc) }
    deriving (Eq,Ord,Read,Show)

--Parsing types

data Expr = Nil
          | Bracket Choice
          | Act Action Expr
          | Var PName deriving (Eq,Ord,Read,Show)
data Rule = Rule PName Choice deriving (Eq,Ord,Read,Show)
type Choice = [Expr]

makePrisms ''Expr
makePrisms ''Rule


makePrisms ''Item
makeLenses ''Binding
makePrisms ''Decoration
makeLenses ''Name
makePrisms ''PName
makeLenses ''Arc
makeIso ''Lts

instance Monoid Lts where
  mempty = Lts M.empty
  mappend (Lts a) (Lts b) = Lts $ M.unionWith S.union a b

type Info = (Set Process, Lts, Set Epsilon)

--Hennessy-Milner Logic

data HML = Diamond (Set Action) HML
         | Box (Set Action) HML
         | Or HML HML
         | And HML HML
         | Neg HML
         | Atom Bool deriving (Eq,Ord,Read,Show)
