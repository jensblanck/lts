module Data.LtsPretty where

import           Control.Applicative hiding (empty)
import           Data.Map            (Map, (!))
import qualified Data.Map            as M
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Text.PrettyPrint

import           Data.Lts

-- Lts pretty print

prettyAction :: Action -> Doc
prettyAction = text

prettyProcessName :: ProcessName -> Doc
prettyProcessName = text

prettyProcess :: Process -> Doc
prettyProcess p = (if S.size p == 1 then id else braces) 
                . hcat 
                . punctuate comma
                . map prettyProcessName
                . S.toList $ p

prettyLts :: Lts -> Doc
prettyLts (Lts l) =
  let items = M.assocs l
      item (p, s) = prettyProcess p <> colon $$ nest 8 (vcat . map arrow $ S.toList s)
      arrow (p, a) = prettyAction a <> text " -> " <> prettyProcess p
  in text "Lts" $$ nest 4 (vcat . map item $ items)

renderLts :: Lts -> String
renderLts = render . prettyLts
