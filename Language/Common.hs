module VL.Language.Common
    ( fromMaybe
    , Name
    , nil, true, false
    )
    where

import Data.Maybe (fromMaybe)

type Name = String

nil, true, false :: Name
nil   = "#:nil"
true  = "#:true"
false = "#:false"
