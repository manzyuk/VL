module VL.Compiler.Monad where

import VL.Language.Common

import VL.Abstract.Value

import VL.Abstract.Analysis (AbstractAnalysis)

import VL.Compiler.ZEncoding

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader

data Table = Table { strName :: Map AbstractValue (Name, Int)
                   , conName :: Map AbstractValue Name
                   , appName :: Map (AbstractValue, AbstractValue) Name
                   , counter :: Int
                   }

-- Code generation monad
type CG = ReaderT AbstractAnalysis (State Table)

runCG :: CG a -> AbstractAnalysis -> a
runCG = (flip evalState emptyTable .). runReaderT
    where
      emptyTable = Table { strName = Map.empty
                         , conName = Map.empty
                         , appName = Map.empty
                         , counter = 0
                         }

analysis :: CG AbstractAnalysis
analysis = ask

getStrNameAndIndex :: AbstractValue -> CG (Name, Int)
getStrNameAndIndex v
    = do table <- get
         case Map.lookup v (strName table) of
           Just (n, i) -> return (n, i)
           Nothing     -> let i = counter table
                              n = zencode $ "#:str-" ++ show i
                          in do put table { strName = Map.insert v (n, i) (strName table)
                                          , counter = succ i
                                          }
                                return (n, i)

getConName :: AbstractValue -> CG Name
getConName v
    = do table <- get
         case Map.lookup v (conName table) of
           Just n  -> return n
           Nothing -> let i = counter table
                          n = zencode $ "#:con-" ++ show i
                      in do put table { conName = Map.insert v n (conName table)
                                      , counter = succ i
                                      }
                            return n

getAppName :: AbstractValue -> AbstractValue -> CG Name
getAppName v1 v2
    = do table <- get
         case Map.lookup (v1, v2) (appName table) of
           Just n  -> return n
           Nothing -> let i = counter table
                          n = zencode $ "#:app-" ++ show i
                      in do put table { appName = Map.insert (v1, v2) n (appName table)
                                      , counter = succ i
                                      }
                            return n
