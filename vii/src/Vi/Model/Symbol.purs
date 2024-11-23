module Vi.Model.Symbol
    ( ViSymbol(..)
    , LetterSymbol(..)
    , MarkSymbol(..)
    , ConsonantMarkSymbol(..)
    , VowelMarkSymbol(..)
    , ToneMarkSymbol(..)
    , consonantSymbols
    , vowelSymbols
    , viSymbols
    , readVowel
    , readConsonant
    , readLetter
    ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set

data ViSymbol
    = Letter LetterSymbol
    | Mark MarkSymbol

data LetterSymbol
    = Consonant String
    | Vowel String

derive instance Generic LetterSymbol _
instance Show LetterSymbol where show = genericShow

data MarkSymbol
    = ConsonantMark ConsonantMarkSymbol
    | VowelMark VowelMarkSymbol
    | ToneMark ToneMarkSymbol

data ConsonantMarkSymbol
    = Dbar

derive instance Eq ConsonantMarkSymbol
derive instance Ord ConsonantMarkSymbol
derive instance Generic ConsonantMarkSymbol _
instance Show ConsonantMarkSymbol where show = genericShow

data VowelMarkSymbol
    = Abreve
    | Acircumflex
    | Ecircumflex
    | Ocircumflex
    | Ohorn
    | Uhorn

derive instance Eq VowelMarkSymbol
derive instance Ord VowelMarkSymbol
derive instance Generic VowelMarkSymbol _
instance Show VowelMarkSymbol where show = genericShow

data ToneMarkSymbol
    = Level
    | Grave
    | Acute
    | Hook
    | Tilde
    | Dot

derive instance Eq ToneMarkSymbol
derive instance Ord ToneMarkSymbol
derive instance Generic ToneMarkSymbol _
instance Show ToneMarkSymbol where show = genericShow

consonantSymbols :: Set String
consonantSymbols = Set.fromFoldable
    [ "b", "c", "d", "g", "h", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "x" ]

vowelSymbols :: Set String
vowelSymbols = Set.fromFoldable
    [ "a", "e", "i", "o", "u", "y" ]

viSymbols :: Set String
viSymbols = vowelSymbols `Set.union` consonantSymbols

read :: String -> (String -> LetterSymbol) -> Set String -> Maybe LetterSymbol
read x cons xs
    | x `Set.member` xs = Just (cons x)
    | otherwise         = Nothing

readVowel :: String -> Maybe LetterSymbol
readVowel x = read x Vowel vowelSymbols

readConsonant :: String -> Maybe LetterSymbol
readConsonant x = read x Consonant consonantSymbols

readLetter :: String -> Maybe LetterSymbol
readLetter x = readVowel x <|> readConsonant x
