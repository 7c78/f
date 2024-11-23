module Data.List.Unsafe where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:), snoc)
import Data.List as Safe
import Data.Tuple.Unsafe (type (&), (&))
import Effect.Exception.Unsafe (unsafeThrow)

arrayList :: forall a. Array a -> List a
arrayList = Safe.fromFoldable

head :: forall a. List a -> a
head xs =
    case Safe.head xs of
        Nothing -> unsafeThrow "Data.List.Unsafe.head: empty list"
        Just x  -> x

last :: forall a. List a -> a
last xs =
    case Safe.last xs of
        Nothing -> unsafeThrow "Data.List.Unsafe.last: empty list"
        Just x  -> x

init :: forall a. List a -> List a
init xs =
    case Safe.init xs of
        Nothing -> unsafeThrow "Data.List.Unsafe.init: empty list"
        Just ys -> ys

tail :: forall a. List a -> List a
tail xs =
    case Safe.tail xs of
        Nothing -> unsafeThrow "Data.List.Unsafe.tail: empty list"
        Just ys -> ys

uncons :: forall a. List a -> (a & List a)
uncons xs =
    case Safe.uncons xs of
        Nothing           -> unsafeThrow "Data.List.Unsafe.uncons: empty list"
        Just {head, tail} -> (head & tail)

unsnoc :: forall a. List a -> (List a & a)
unsnoc xs =
    case Safe.unsnoc xs of
        Nothing           -> unsafeThrow "Data.List.Unsafe.unsnoc: empty list"
        Just {init, last} -> (init & last)

splitAt :: forall a. Int -> List a -> (List a & List a)
splitAt n xs | n <= 0 =
    (Nil & xs)
splitAt _ Nil    = (Nil & Nil)
splitAt 1 (x:xs) = (Safe.singleton x & xs)
splitAt n (x:xs) =
    let (ys & zs) = splitAt (n - 1) xs
     in ((x:ys) & zs)

modifyAt :: forall a. Int -> (a -> a) -> List a -> List a
modifyAt i f xs =
    case Safe.modifyAt i f xs of
        Nothing -> unsafeThrow "Data.List.Unsafe.modifyAt: index out of bound"
        Just ys -> ys

scanl :: forall a b. (b -> a -> b) -> b -> List a -> List b
scanl f z xs =
    z : Safe.scanl f z xs

scanr :: forall a b. (a -> b -> b) -> b -> List a -> List b
scanr f z xs =
    Safe.scanr f z xs `snoc` z
