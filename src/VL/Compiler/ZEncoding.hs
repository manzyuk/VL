module VL.Compiler.ZEncoding (zencode) where

-- Z-encoding.  See http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/SymbolNames.

import Data.Maybe

import qualified Data.Map as Map

import Control.Arrow

-- We could use ordinary lists and concat, but that would be slow; we
-- could write 'zencode' in directly recursive style (i.e., something
-- like zencode ('z':cs) = 'z' : 'z' : zencode cs), but that would be
-- tedious.  With difference lists we can be efficient and write in a
-- more natural style.
-- See http://book.realworldhaskell.org/read/data-structures.html#data.dlist
newtype DList a = DL ([a] -> [a])

instance Semigroup (DList a) where
    DL f <> DL g = DL (f . g)

instance Monoid (DList a) where
    mempty = DL id

fromList :: [a] -> DList a
fromList xs = DL (xs ++)

toList :: DList a -> [a]
toList (DL f) = f []

zencode :: String -> String
zencode = toList . mconcat . map zencodeChar

zencodeChar :: Char -> DList Char
zencodeChar c = fromMaybe (error msg) (Map.lookup c table)
    where
      msg = "encodeChar: Invalid character " ++ show c
      alphanumeric = Map.fromList . map dup $ ['a'..'y'] ++ ['A'..'Y'] ++ ['0'..'9']
      dup x = (x, fromList [x])
      table = alphanumeric `Map.union` other
      other = Map.fromList . map (second fromList) $
              -- standard z-encoding extended with encodings of ?, ~, and @
              [ ('z', "zz")
              , ('Z', "ZZ")
              , ('!', "zn")
              , ('$', "zd")
              , ('%', "zv")
              , ('&', "za")
              , ('*', "zt")
              , ('/', "zs")
              , (':', "ZC")
              , ('<', "zl")
              , ('=', "ze")
              , ('>', "zg")
              , ('?', "zQ") -- /Q/uestion mark
              , ('^', "zc")
              , ('_', "zu")
              , ('~', "zW") -- /W/ave
              , ('+', "zp")
              , ('-', "zm")
              , ('.', "zi")
              , ('@', "zA") -- /A/t sign
              , ('#', "zh")
              ]
