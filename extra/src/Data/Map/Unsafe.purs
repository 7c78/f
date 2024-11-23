module Data.Map.Unsafe where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Map (Map)
import Effect.Exception.Unsafe (unsafeThrow)

lookup :: forall k v. Ord k => k -> Map k v -> v
lookup k m =
    case Map.lookup k m of
        Nothing -> unsafeThrow "lookup: key missisng"
        Just v  -> v
