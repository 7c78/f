module Data.Array.Unsafe where

import Prelude
import Data.Maybe (Maybe(..))
import Data.TraversableWithIndex (scanlWithIndex, scanrWithIndex) as Safe
import Data.Array (cons, deleteAt, head, index, init, insertAt, last, length, modifyAt, scanl, scanr, snoc, tail, uncons, updateAt) as Safe
import Data.Tuple.Unsafe (type (&), (&))
import Effect.Exception.Unsafe (unsafeThrow)

updateAt :: forall a. Int -> a -> Array a -> Array a
updateAt i y xs =
    case Safe.updateAt i y xs of
        Nothing  -> unsafeThrow "updateAt: index out of bound"
        Just xs' -> xs'

modifyAt :: forall a. Int -> (a -> a) -> Array a -> Array a
modifyAt i f xs =
    case Safe.modifyAt i f xs of
        Nothing  -> unsafeThrow "modifyAt: index out of bound"
        Just xs' -> xs'

modifyLast :: forall a. (a -> a) -> Array a -> Array a
modifyLast f xs =
    modifyAt (Safe.length xs - 1) f xs

modifyHead :: forall a. (a -> a) -> Array a -> Array a
modifyHead =
    modifyAt 0

insertAt :: forall a. Int -> a -> Array a -> Array a
insertAt i y xs =
    case Safe.insertAt i y xs of
        Nothing -> unsafeThrow "insertAt: index out of bound"
        Just xs -> xs

deleteAt :: forall a. Int -> Array a -> Array a
deleteAt i xs =
    case Safe.deleteAt i xs of
        Nothing -> unsafeThrow "deleteAt: index out of bound"
        Just xs -> xs

scanl :: forall a b. (b -> a -> b) -> b -> Array a -> Array b
scanl f z xs =
    z `Safe.cons` Safe.scanl f z xs

scanr :: forall a b. (a -> b -> b) -> b -> Array a -> Array b
scanr f z xs =
    Safe.scanr f z xs `Safe.snoc` z

scanlWithIndex :: forall a b. (Int -> b -> a -> b) -> b -> Array a -> Array b
scanlWithIndex f z xs =
    z `Safe.cons` Safe.scanlWithIndex f z xs

scanrWithIndex :: forall a b. (Int -> a -> b -> b) -> b -> Array a -> Array b
scanrWithIndex f z xs =
    Safe.scanrWithIndex f z xs `Safe.snoc` z

infixl 8 index as !
index :: forall a. Array a -> Int -> a
index xs i =
    case Safe.index xs i of
        Nothing -> unsafeThrow "index: empty array"
        Just x  -> x

head :: forall a. Array a -> a
head xs =
    case Safe.head xs of
        Nothing -> unsafeThrow "head: empty array"
        Just x  -> x

last :: forall a. Array a -> a
last xs =
    case Safe.last xs of
        Nothing -> unsafeThrow "last: empty array"
        Just x  -> x

tail :: forall a. Array a -> Array a
tail xs =
    case Safe.tail xs of
        Nothing  -> unsafeThrow "tail: empty array"
        Just xs' -> xs'

init :: forall a. Array a -> Array a
init xs =
    case Safe.init xs of
        Nothing  -> unsafeThrow "init: empty array"
        Just xs' -> xs'

uncons :: forall a. Array a -> a & Array a
uncons xs =
    case Safe.uncons xs of
        Nothing ->
            unsafeThrow "uncons: empty array"
        Just {head: x, tail: xs} ->
            x & xs
