{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Lts where

import           Control.Applicative hiding (empty)
import           Control.Monad
import           Data.Function       (on)
import           Data.List           (foldl', foldl1', groupBy, intercalate, reverse, sortBy)
import           Data.Map            (Map, (!))
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Text.Parsec         hiding (choice, many, optional, (<|>))
import           Text.Parsec.String  (Parser)

-- Lts types

type Variable = String
type Action = String
type ProcessName = String
type Process = Set ProcessName
type Epsilon = (Process, Process)
type Colour = Set Process
type Colouring = Set Colour

newtype Lts = Lts {lts :: Map Process (Set (Process, Action))}
    deriving (Eq,Ord,Read,Show)
instance Monoid Lts where
  mempty = Lts M.empty
  mappend (Lts a) (Lts b) = Lts $ M.unionWith S.union a b

type Info = (Set Process, Lts, Set Epsilon)

--Parsing types

data Expr = Nil
          | Bracket Choice
          | Act Action Expr
          | Var Variable deriving (Eq,Ord,Read)
data Rule = Rule Variable Choice deriving (Eq,Ord,Show,Read)
type Choice = [Expr]

instance Show Expr where
  show Nil = "0"
  show (Bracket e) = "(" ++ showChoice e ++ ")"
  show (Act l e) = l ++ ('.' : show e)
  show (Var v) = v

showChoice = intercalate "+" . map show

--

parseLts :: SourceName -> String -> Either ParseError [Rule]
parseLts = parse rules

convert :: [Rule] -> Lts
convert = addZeros . satisfyEpsilon . info

addZeros :: Info -> Lts
addZeros (s,Lts l,_) = Lts $ l <> M.fromSet (const mempty) s
-- fromSet s
--   where fromSet s = M.fromList . S.toList . S.map (\k -> (k, S.empty)) $ s

info :: [Rule] -> Info
info = mconcat . map infoRule

infoRule :: Rule -> Info
infoRule (Rule v e) =
  mconcat $ (S.singleton (process v), mempty, mempty) : map (infoExpr (process v) (Just $ process v)) e

infoExpr :: Process -> Maybe Process -> Expr -> Info
infoExpr _ _ Nil = (S.singleton (process "0"), mempty, mempty)
infoExpr v mv (Bracket e) =
  let mv' = Just $ fromMaybe (process $ showChoice e) mv
  in mconcat $ map (infoExpr v mv') e
infoExpr v _ (Act l e) =
  let v' = process $ case e of
             (Bracket e') -> showChoice e'
             _ -> show e
  in (S.singleton v', Lts $ M.singleton v (S.singleton (v',l)), mempty)
         <> infoExpr v' Nothing e
infoExpr _ Nothing (Var v') = (S.singleton (process v'), mempty, mempty)
infoExpr _ (Just v) (Var v') = (S.singleton (process v')
                               , mempty
                               , S.singleton (v, process v'))

satisfyEpsilon :: Info -> Info
satisfyEpsilon i@(ps, a, es) =
  let a' = S.fold copy a es
      copy (p,p') a@(Lts m) = a <> (Lts . maybe M.empty
                                                (M.singleton p))
                                         (M.lookup p' m)
  in if a == a' then i else satisfyEpsilon (ps, a', es)

process :: ProcessName -> Process
process = S.singleton

-- Parsing using applicative style

variable :: Parser Variable
variable = (:) <$> upper <*> many alphaNum <* spaces

action :: Parser Action
action = (:) <$> lower <*> many alphaNum <* spaces

plus,open,close,ruleEnd,eqn :: Parser Char
plus = char '+' <* spaces
open = char '(' <* spaces
close = char ')' <* spaces
ruleEnd = char ';' <* spaces
eqn = char '=' <* spaces

choice,bracketedChoice,choice' :: Parser Choice
choice = bracketedChoice <|> choice'
bracketedChoice = between open close choice'
choice' = expr `sepBy` plus

expr,inBrackets,constant,actExpr,nilExpr :: Parser Expr
expr = inBrackets <|> nilExpr <|> actExpr <|> constant
inBrackets = Bracket <$> bracketedChoice
constant = Var <$> variable <* spaces
actExpr = Act <$> action <* char '.' <* spaces <*> expr
nilExpr = Nil <$ char '0' <* spaces

rule :: Parser Rule
rule = Rule <$> (spaces *> variable) <* eqn <*> choice

rules :: Parser [Rule]
rules = rule `sepEndBy1` ruleEnd

-- Lts manipulation

alphabet :: Lts -> Set Action
alphabet (Lts l) = S.map snd . foldl1' S.union $ M.elems l

processes :: Lts -> Set Process
processes (Lts l) = M.keysSet l

successors :: Lts -> Process -> Action -> Set Process
successors l p a = S.map fst . S.filter (\(_,a') -> a' == a) . M.findWithDefault S.empty p $ lts l

-- Cell of partition. A colouring is a partition of the processes.
cell :: Ord a => Set (Set a) -> a -> Set a
cell p e = S.foldr (\c s -> if e `S.member` c then c else s) S.empty p

inverseMap :: (Ord a, Ord k) => Map k a -> Map a (Set k)
inverseMap = M.foldrWithKey (\ k a m -> M.insertWith S.union a (S.singleton k) m) M.empty

colours :: Colouring -> Set Process -> Set Colour
colours c = S.map (cell c)

setUnion :: Ord a => Set (Set a) -> Set a
setUnion = S.fold S.union S.empty

smash :: Set Colour -> Set Process
smash = S.map setUnion

{-- Does one step of the lts minimisation. Given a colouring encoded as a
partition of `Process`es this computes a new colouring that separates any
`Process`es that can be differentiated by one action step.  -}

minStep :: Lts -> Colouring -> Colouring
minStep l cs =
  let colourMap p = M.fromSet (setUnion . colours cs . successors l p) $ alphabet l
      a = M.fromSet colourMap $ processes l
  in S.fromList . M.elems . inverseMap $ a

minimiseLts :: Lts -> Lts
minimiseLts l =
  let as = alphabet l
      cs = iterate (minStep l) (S.singleton $ processes l)
      c = fst . head . dropWhile (\(a,b) -> a /= b) $ zip cs (tail cs)
      f :: Process -> Action -> Set (Process, Action)
      f p a = S.map (flip (,) a) . smash . colours c $ successors l (S.singleton $ S.findMin p) a
  in Lts . M.fromSet (\p -> setUnion $ S.map (f p) as) $ smash c
