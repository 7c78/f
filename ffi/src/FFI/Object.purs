module FFI.Object
    ( (!), unsafeGetProperty
    , (!?), getProperty
    , (..), applyMethod
    , HArray, args0, args1, args2, args3, args4, args5
    , typeof
    , instanceof
    , cast
    ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, toMaybe)
import Data.Function.Uncurried
    ( Fn1, runFn1
    , Fn2, runFn2
    , Fn3, runFn3
    , Fn4, runFn4
    , Fn5, runFn5 )
import Unsafe.Coerce (unsafeCoerce)

infixl 9 unsafeGetProperty as !
foreign import unsafeGetProperty :: forall a o. o -> String -> a

infixl 9 getProperty as !?
getProperty :: forall a o. o -> String -> Maybe a
getProperty obj = toMaybe <<< notNull <<< unsafeGetProperty obj

foreign import data HArray :: Type

infix 9 applyMethod as ..
foreign import applyMethod :: forall a o. o -> String -> HArray -> a

foreign import args0 :: HArray
foreign import _args1 :: forall a1. Fn1 a1 HArray
foreign import _args2 :: forall a1 a2. Fn2 a1 a2 HArray
foreign import _args3 :: forall a1 a2 a3. Fn3 a1 a2 a3 HArray
foreign import _args4 :: forall a1 a2 a3 a4. Fn4 a1 a2 a3 a4 HArray
foreign import _args5 :: forall a1 a2 a3 a4 a5. Fn5 a1 a2 a3 a4 a5 HArray

args1 :: forall a1. a1 -> HArray
args1 = runFn1 _args1

args2 :: forall a1 a2. a1 -> a2 -> HArray
args2 = runFn2 _args2

args3 :: forall a1 a2 a3. a1 -> a2 -> a3 -> HArray
args3 = runFn3 _args3

args4 :: forall a1 a2 a3 a4. a1 -> a2 -> a3 -> a4 -> HArray
args4 = runFn4 _args4

args5 :: forall a1 a2 a3 a4 a5. a1 -> a2 -> a3 -> a4 -> a5 -> HArray
args5 = runFn5 _args5

foreign import typeof :: forall a. a -> String
foreign import instanceof :: forall a b. a -> b -> Boolean

cast :: forall ctor from to. ctor -> from -> Maybe to
cast ctor from =
    if from `instanceof` ctor
       then Just (unsafeCoerce from)
       else Nothing
