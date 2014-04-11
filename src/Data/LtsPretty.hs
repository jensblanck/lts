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
prettyAction = prettyName

prettyName :: Name -> Doc
prettyName (Name n a i) = text n <> prettyDecorations '^' a <> prettyDecorations '_' i

prettyDecorations :: Char -> [Decoration] -> Doc
prettyDecorations _ [] = empty
prettyDecorations c ds = char c <> hcat (punctuate comma $ map f ds)
    where f (DecVar s) = text s
          f (DecValue n) = int n
          f (DecColl c) = hcat $ map prettyItem c
          f (DecBind (Binding v c)) = text v <> colon <> hcat (map prettyItem c)
          prettyItem (ItemInt n) = int n
          prettyItem (ItemStr s) = text s

prettyProcess :: Process -> Doc
prettyProcess p = (if S.size p == 1 then id else braces) 
                . hcat 
                . punctuate comma
                . map prettyName
                . S.toList $ p

prettyLts :: Lts -> Doc
prettyLts (Lts l) =
  let items = M.assocs l
      item (p, s) = prettyProcess p <> colon $$ nest 8 (vcat . map arrow $ S.toList s)
      arrow (Arc p a) = prettyAction a <> text " -> " <> prettyProcess p
  in text "Lts" $$ nest 4 (vcat . map item $ items)

renderLts :: Lts -> String
renderLts = render . prettyLts
