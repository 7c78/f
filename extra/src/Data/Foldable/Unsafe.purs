module Data.Foldable.Unsafe where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Foldable as Safe
import Data.Foldable (class Foldable, foldr, foldl)
import Effect.Exception.Unsafe (unsafeThrow)

minimum :: forall a t. Ord a => Foldable t => t a -> a
minimum xs =
    case Safe.minimum xs of
        Nothing -> unsafeThrow "minimum: empty"
        Just x  -> x

minimumOn :: forall a b t. Ord b => Foldable t => (a -> b) -> t a -> a
minimumOn f xs =
    case Safe.minimumBy (\x y -> f x `compare` f y) xs of
         Nothing -> unsafeThrow "minimumOn: empty"
         Just x  -> x

maximum :: forall a t. Ord a => Foldable t => t a -> a
maximum xs =
    case Safe.maximum xs of
        Nothing -> unsafeThrow "maximum: empty"
        Just x  -> x

maximumOn :: forall a b t. Ord b => Foldable t => (a -> b) -> t a -> a
maximumOn f xs =
    case Safe.maximumBy (\x y -> f x `compare` f y) xs of
        Nothing -> unsafeThrow "maximumOn: empty"
        Just x  -> x

foldr1 :: forall a t. Foldable t => (a -> a -> a) -> t a -> a
foldr1 f xs =
    case foldr mf Nothing xs of
        Nothing -> unsafeThrow "foldr1: empty"
        Just z  -> z
    where mf x mz =
            Just $ case mz of
                    Nothing -> x
                    Just z  -> f x z

foldl1 :: forall a t. Foldable t => (a -> a -> a) -> t a -> a
foldl1 f xs =
    case foldl mf Nothing xs of
        Nothing -> unsafeThrow "foldl1: empty"
        Just z  -> z
    where mf mz x =
            Just $ case mz of
                    Nothing -> x
                    Just z  -> f z x
