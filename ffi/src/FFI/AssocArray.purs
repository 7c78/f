module FFI.AssocArray
    ( AssocArray
    , empty
    , keys
    , elems
    , member
    , notMember
    , size
    , null
    , insert
    , delete
    , (:=), singleton
    , (!?), lookup
    , (!), unsafeLookup
    , fold, foldM
    , union
    , STAssocArray
    , newSTAssocArray
    , insertSTAssocArray
    , deleteSTAssocArray
    , thawSTAssocArray
    , freezeSTAssocArray
    ) where

import Prelude
import Control.Monad.ST (ST, Region)
import Data.Maybe (Maybe)
import Data.Nullable (notNull, toMaybe)
import Data.Array (length)

foreign import data AssocArray :: Type -> Type
type role AssocArray representational

foreign import data STAssocArray :: Region -> Type -> Type
type role STAssocArray nominal representational

foreign import newSTAssocArray    :: forall a s. ST s (STAssocArray s a)
foreign import insertSTAssocArray :: forall a s. String -> a -> STAssocArray s a -> ST s (STAssocArray s a)
foreign import deleteSTAssocArray :: forall a s. String -> STAssocArray s a -> ST s (STAssocArray s a)

foreign import _copyST :: forall a b s. a -> ST s b

-- | Convert an immutable AssocArray into a mutable AssocArray
thawSTAssocArray :: forall a s. AssocArray a -> ST s (STAssocArray s a)
thawSTAssocArray = _copyST

-- | Convert a mutable AssocArray into an immutable AssocArray
freezeSTAssocArray :: forall a s. STAssocArray s a -> ST s (AssocArray a)
freezeSTAssocArray = _copyST

foreign import runST :: forall a. (forall s. ST s (STAssocArray s a)) -> AssocArray a

mutate :: forall a b. (forall s. STAssocArray s a -> ST s b) -> AssocArray a -> AssocArray a
mutate f x = runST do
    mx <- thawSTAssocArray x
    void $ f mx
    pure mx

foreign import _equal :: forall a. AssocArray a -> AssocArray a -> Boolean

instance Eq a => Eq (AssocArray a) where
    eq = _equal

foreign import _foldM
    :: forall a z m.
       (m -> (z -> m) -> m)
    -> (z -> String -> a -> m)
    -> m
    -> AssocArray a
    -> m

foldM :: forall a z m. Monad m => (z -> String -> a -> m z) -> z -> AssocArray a -> m z
foldM f z = _foldM bind f (pure z)

fold :: forall a z. (z -> String -> a -> z) -> z -> AssocArray a -> z
fold = _foldM (#)

-- | Compute the union of two maps, preferring the first map in the case of
-- | duplicate keys.
union :: forall a. AssocArray a -> AssocArray a -> AssocArray a
union x = mutate (\my -> foldM (\my' k v -> insertSTAssocArray k v my') my x)

instance Semigroup (AssocArray a) where
    append = union

instance Monoid (AssocArray a) where
    mempty = empty

foreign import empty :: forall a. AssocArray a

insert :: forall a. String -> a -> AssocArray a -> AssocArray a
insert k v = mutate (insertSTAssocArray k v)

infix 9 singleton as :=
singleton :: forall a. String -> a -> AssocArray a
singleton k v = runST (newSTAssocArray >>= insertSTAssocArray k v)

delete :: forall a. String -> AssocArray a -> AssocArray a
delete k = mutate (deleteSTAssocArray k)

infix 9 unsafeLookup as !
foreign import unsafeLookup :: forall a. AssocArray a -> String -> a

infix 9 lookup as !?
lookup :: forall a. AssocArray a -> String -> Maybe a
lookup obj = toMaybe <<< notNull <<< unsafeLookup obj

foreign import keys :: forall a. AssocArray a -> Array String

foreign import elems :: forall a. AssocArray a -> Array a

foreign import member :: forall a. String -> AssocArray a -> Boolean

notMember :: forall a. String -> AssocArray a -> Boolean
notMember = not <<< member

size :: forall a. AssocArray a -> Int
size = length <<< keys

null :: forall a. AssocArray a -> Boolean
null = (_ == 0) <<< size
