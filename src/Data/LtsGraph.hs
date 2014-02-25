module Data.LtsGraph where

import           Control.Applicative hiding (empty)
import           Data.Map            (Map, (!))
import qualified Data.Map            as M
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.GraphViz

import           Data.Lts

main = do
  f <- readFile "test.lts"
  let g = fmap convert $ parseLts "" f
  let l = either undefined id g
  let gr = graphElemsToDot defaultParams
                           (nodes l)
                           (arcs l)
  return ()