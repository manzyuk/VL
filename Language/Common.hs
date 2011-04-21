module VL.Language.Common
    ( Name
    , nil, true, false
    , module Data.Maybe
    )
    where

import Data.Maybe

type Name = String

nil, true, false :: Name
nil   = "#:nil"
true  = "#:true"
false = "#:false"
