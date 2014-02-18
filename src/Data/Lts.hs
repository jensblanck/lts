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

type Variable = String
type Action = String
type ProcessName = String
data Process = Single ProcessName
               | Multi (Set ProcessName)
    deriving (Eq,Ord,Read,Show)
type Choice = [Expr]
newtype Lts = Lts {lts :: Map Process (Set (Process, Action))}
    deriving (Eq,Ord,Read,Show)
type Epsilon = (Process, Process)

instance Monoid Lts where
  mempty = Lts M.empty
  mappend (Lts a) (Lts b) = Lts $ M.unionWith S.union a b
type Info = (Set Process, Lts, Set Epsilon)

data Expr = Nil
          | Bracket Choice
          | Act Action Expr
          | Var Variable deriving (Eq,Ord,Read)
data Rule = Rule Variable Choice deriving (Eq,Ord,Show,Read)

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
addZeros (s,Lts l,_) = Lts $ l <> fromSet s
  -- M.fromSet (const mempty) s
  where fromSet s = M.fromList . S.toList . S.map (\k -> (k, S.empty)) $ s

info :: [Rule] -> Info
info = mconcat . map infoRule

infoRule :: Rule -> Info
infoRule (Rule v e) =
    mconcat $ (S.singleton (Single v), mempty, mempty) : map (infoExpr (Single v) (Just $ Single v)) e

infoExpr :: Process -> Maybe Process -> Expr -> Info
infoExpr _ _ Nil = (S.singleton (Single "0"), mempty, mempty)
infoExpr v mv (Bracket e) =
  let mv' = Just $ fromMaybe (Single $ showChoice e) mv
  in mconcat $ map (infoExpr v mv') e
infoExpr v _ (Act l e) =
  let v' = Single $ case e of
             (Bracket e') -> showChoice e'
             _ -> show e
  in (S.singleton v', Lts $ M.singleton v (S.singleton (v',l)), mempty)
         <> infoExpr v' Nothing e
infoExpr _ Nothing (Var v') = (S.singleton (Single v'), mempty, mempty)
infoExpr _ (Just v) (Var v') = (S.singleton (Single v')
                               , mempty
                               , S.singleton (v, Single v'))

satisfyEpsilon :: Info -> Info
satisfyEpsilon i@(ps, a, es) =
  let a' = S.fold copy a es
      copy (p,p') a@(Lts m) = a <> (Lts . maybe M.empty
                                                (M.singleton p))
                                         (M.lookup p' m)
  in if a == a' then i else satisfyEpsilon (ps, a', es)

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

{-- Does one step of the lts minimisation. Given a colouring encoded as a partition of `Process`es this computes a new colouring that separates any `Process`es that can be differentiated by one action step.
-}
minStep :: Lts -> Set (Set Process) -> Set (Set Process)
minStep l s =
    let ps = processes l
        as = alphabet l
        -- findIn looks up all colours represented by a set of `Process`es.
        findIn s ns = S.fold S.union S.empty $ S.map (\p -> S.fold S.union S.empty $ S.map (\s' -> if S.member p s' then s' else S.empty) s) ns
        -- findDest computes a pair of (a set of pairs of an action and all colours reachable via that action) and a `Process`.
        findDest p = (S.map (\a -> (a, findIn s $ successors l p a)) as, p)
        a = S.map findDest ps
        -- Groups together `Process`es that still are indistinguishable by what colours are reachable.
        b = groupBy (\a b -> fst a == fst b) . sortBy (compare `on` fst) . S.toList $ a
        -- Extracts the colouring by forgetting non-relevant information.
        c = map (map snd) b
    in S.fromList . map S.fromList $ c

minimiseLts :: Lts -> Lts
minimiseLts l =
    let cs = iterate (minStep l) (S.singleton $ processes l)
        c = fst . head . dropWhile (\(a,b) -> a /= b) $ zip cs (tail cs)
        trans = M.fromList [(p, Multi . S.fold S.union S.empty $ S.map (\c' -> if S.member p c' then c' else S.empty) c) | p <- S.toList $ processes l ]
        buildLts cs = undefined
    in undefined

--minimiseStep :: Lts -> Lts
minimiseStep l = M.fromList . map swap . M.assocs . M.fromListWith (\_ a -> a) . map swap . M.assocs $ lts l
  where swap (a,b) = (b,a)
