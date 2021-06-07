{-# LANGUAGE TypeOperators, FlexibleInstances #-}
module VL.Language.Pretty
    ( Pretty
    , pp
    , pprint
    , sepMap
    , ppList
    , ppPair
    , internal
    , dot, newline
    , module Text.PrettyPrint
    )
    where

import VL.Language.Common

import Text.PrettyPrint hiding ((<>))

class Pretty a where
    pp :: a -> Doc

pprint :: Pretty a => a -> String
pprint = render . pp

instance Pretty Name where
    pp = text

-- General purpose combinators
sepMap :: (a -> Doc) -> [a] -> Doc
sepMap f = sep . map f

ppList :: [Doc] -> Doc
ppList = parens . sep

ppPair :: (Pretty a, Pretty b) => a -> b -> Doc
ppPair x y = ppList [pp x, dot, pp y]

internal :: String -> Doc -> Doc
internal name contents = text "#[" <> sep [text name, contents] <> char ']'

dot, newline :: Doc
dot     = char '.'
newline = char '\n'
