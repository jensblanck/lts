{-# LANGUAGE NoMonomorphismRestriction,TemplateHaskell #-}
module Data.Lts where

import           Control.Applicative hiding (empty)
import           Control.Lens        hiding (Action,Choice)
import           Control.Monad
import           Data.Function       (on)
import           Data.List           (foldl', foldl1', groupBy, intercalate, partition, reverse, sortBy)
import           Data.Map            (Map, (!))
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Text.Parsec         hiding (choice, label, many, optional, (<|>))
import           Text.Parsec.String  (Parser)
import           Text.PrettyPrint    (render)

import Data.LtsTypes
import Data.LtsPretty

--Parsing types

data Expr = Nil
          | Bracket Choice
          | Act Action Expr
          | Var PName deriving (Eq,Ord,Read)

instance Show Expr where
  show Nil = "0"
  show (Bracket e) = "(" ++ showChoice e ++ ")"
  show (Act l e) = (render $ prettyAction l) ++ ('.' : show e)
  show (Var v) = render $ prettyPName v

showChoice = intercalate "+" . map show

data Rule = Rule PName Choice deriving (Eq,Ord,Read,Show)
type Choice = [Expr]

makePrisms ''Expr
makePrisms ''Rule

-- help to define processes

process :: PName -> Process
process = S.singleton

process' :: Variable -> Process
process' v = process . PSingle $ Name v [] []

nilProcess :: Process
nilProcess = process' "0"

-- Parsing lts equations

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
infoExpr _ _ Nil = (S.singleton nilProcess, mempty, mempty)
infoExpr v mv (Bracket e) =
  let mv' = Just $ fromMaybe (process' $ showChoice e) mv
  in mconcat $ map (infoExpr v mv') e
infoExpr v _ (Act l e) =
  let v' = process' $ case e of
             (Bracket e') -> showChoice e'
             _ -> show e
  in (S.singleton v', Lts $ M.singleton v (S.singleton (Arc v' l)), mempty)
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

-- Parsing using applicative style

variable :: Parser Char -> Parser Variable
variable p = (:) <$> p <*> many alphaNum <* spaces

varUpper,varLower,varLetter :: Parser Variable
varUpper = variable upper
varLower = variable lower
varLetter = variable letter

natural :: Parser String
natural = many1 digit

item :: Parser Item
item = ItemInt . read <$> natural <* notFollowedBy alphaNum
       <|> ItemStr <$> many1 alphaNum

intRange :: Parser Collection
intRange =
    let f m n = map ItemInt [read m..read n]
    in f <$> natural <* char '.' <* char '.' <*> natural

setNotation :: Parser Collection
setNotation = char '{' *> item `sepBy1` (char ',' <* spaces) <* char '}'

collection :: Parser Collection
collection = setNotation <|> intRange

binding :: Parser Binding
binding = Binding <$> varLetter <* char ':' <*> collection

decoration :: Parser Decoration
decoration = try (DecBind <$> binding)
             <|> try (DecColl <$> collection)
             <|> try (DecValue . read <$> natural)
             <|> DecVar <$> varLetter

decorations :: Parser ([Decoration], [Decoration])
decorations = f <$> many ((,) <$> char '^' <*> decoration
                          <|> (,) <$> char '_' <*> decoration)
    where f = g . partition ((== '^') . fst)
          g (as,bs) = (map snd as, map snd bs)

decName :: Parser Variable -> Parser Name
decName p = (\n (a,i) -> Name n a i) <$> p <*> decorations

decNameUpper,decNameLower,decNameLetter :: Parser Name
decNameUpper = decName varUpper
decNameLower = decName varLower
decNameLetter = decName varLetter

action :: Parser Action
action = decNameLower

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
constant = Var . PSingle <$> decNameUpper <* spaces
actExpr = Act <$> action <* char '.' <* spaces <*> expr
nilExpr = Nil <$ char '0' <* spaces

rule :: Parser Rule
rule = Rule <$> (PSingle <$> (spaces *> decNameUpper <* spaces)) <* eqn <*> choice

rules :: Parser [Rule]
rules = rule `sepEndBy1` ruleEnd

-- Lts manipulation
{-
defaultName :: Process -> PName
defaultName = head . S.toList

alphabet :: Lts -> Set Action
alphabet (Lts l) = S.map (^. label) . foldl1' S.union $ M.elems l
--alphabet = traverse . from lts

processes :: Lts -> Set Process
processes (Lts l) = M.keysSet l

nodes :: Lts -> [(Name, [Name])]
nodes = map ((\n -> (head n, n)) . S.toList) . S.toList . processes

arcs :: Lts -> [(Name, Name, Action)]
arcs (Lts l) =
  let as = M.assocs l
      f (p, es) = map (\(Arc p' a) -> (defaultName p, defaultName p', a)) $ S.toList es
  in concatMap f as

successors :: Lts -> Process -> Action -> Set Process
successors l p a = S.map _destination
                   . S.filter (\(Arc _ a') -> a' == a)
                   . M.findWithDefault S.empty p
                   $ l ^. from lts

successorSet :: Lts -> Process -> Set Action -> Set Process
successorSet l p as = S.foldl S.union S.empty $ S.map (successors l p) as

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
      c = fst . head . dropWhile (uncurry (/=)) $ zip cs (tail cs)
      f :: Process -> Action -> Set Arc
      f p a = S.map (`Arc` a)
              . smash
              . colours c
              $ successors l (S.singleton $ p ^?! folded) a
  in Lts . M.fromSet (\p -> setUnion $ S.map (f p) as) $ smash c

-- Hennessy-Milner Logic

valid :: Lts -> Process -> HML -> Bool
valid _ _ (Atom b) = b
valid l p (Box as hml) =
  let ps = successorSet l p as
  in S.foldl (&&) True (S.map (flip (valid l) hml) ps)
valid l p (Diamond as hml) =
  let ps = successorSet l p as
  in S.foldl (||) False (S.map (flip (valid l) hml) ps)
valid l p (Or h h') = valid l p h || valid l p h'
valid l p (And h h') = valid l p h && valid l p h'
valid l p (Neg h) = not $ valid l p h
-}