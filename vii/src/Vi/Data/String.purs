module Vi.Data.String where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String (Pattern(..), Replacement(..))
import Effect.Exception.Unsafe (unsafeThrow)

words :: String -> Array String
words = String.split (Pattern " ")

unwords :: Array String -> String
unwords = String.joinWith " "

lines :: String -> Array String
lines = String.split $ Pattern "\n"

unlines :: Array String -> String
unlines = String.joinWith "\n"

replace :: String -> String -> String -> String
replace x y =
    String.replace (Pattern x) (Replacement y)

infixl 8 index as !
index :: String -> Int -> String
index xs i =
    case String.codePointAt i xs of
        Nothing -> unsafeThrow "String.index: out of bounds"
        Just x  -> String.singleton x

notNull :: String -> Boolean
notNull = not <<< String.null

last :: String -> String
last xs = xs ! (String.length xs - 1)

chars :: String -> Array String
chars = map String.singleton <<< String.toCodePointArray
