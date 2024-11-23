module Test.Vi.Model.Word where

import Prelude
import Data.Maybe (Maybe(..))
import Vi.Data.String (lines, unlines, unwords, words)
import Vi.Model.Word (toString)
import Vi.Model.InputMethod (InputMethod, readWord)

word :: InputMethod -> String -> String
word method xs =
    case readWord method xs of
        Nothing -> ""
        Just vi -> toString vi

sentence :: InputMethod -> String -> String
sentence method =
    unwords <<< map (word method) <<< words

paragraph :: InputMethod -> String -> String
paragraph method =
    unlines <<< map (sentence method) <<< lines
