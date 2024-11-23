module Test.FFI.AssocArray where

import Prelude
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Assertions (shouldEqual)
import FFI.AssocArray as A
import FFI.AssocArray (AssocArray, (!), (!?))

number :: Int -> Int
number = identity

spec :: Spec Unit
spec = do
    describe "FFI.AssocArray" do
        describe "insert" do
            it "insert into the empty object" do
                quickCheck $ \k v ->
                    (A.insert k v A.empty) ! k
                    == (number v)

            it "insert two values with the same key" do
                quickCheck $ \k v1 v2 ->
                    (A.insert k v2 (A.insert k v1 A.empty)) ! k
                     == number v2

            it "insert -> delete" do
                quickCheck $ \k v ->
                    A.null (A.delete k (A.insert k (number v) A.empty))

            it "insert two -> lookup second" do
                quickCheck $ \k1 v1 k2 v2 ->
                    A.insert k2 (number v2) (A.insert k1 (number v1) A.empty) ! k2
                    == number v2

            it "insert two -> delete first" do
                quickCheck $ \k1 v1 k2 v2 ->
                    (k1 == k2)
                 || ((A.delete k1 (A.insert k2 (number v2) (A.insert k1 (number v1) A.empty))) ! k2
                        == v2)

        describe "lookup" do
            it "lookup from empty" do
                quickCheck $ \k ->
                    (A.empty :: AssocArray Int) !? k
                    == Nothing

            it "lookup from singleton" do
                quickCheck $ \k v ->
                    A.singleton k (number v) ! k
                    == v

        describe "union" do
            it "prefer the first object" do
                let x = A.singleton "k" "x"
                    y = A.singleton "k" "y"
                    z = x `A.union` y
                (z ! "k") `shouldEqual` "x"
