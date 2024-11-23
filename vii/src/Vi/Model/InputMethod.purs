module Vi.Model.InputMethod where

import Prelude
import Control.Alternative ((<|>))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Vi.Data.String as String
import Vi.Model.Symbol (ConsonantMarkSymbol(..), MarkSymbol(..), ToneMarkSymbol(..), VowelMarkSymbol(..), readLetter)
import Vi.Model.Word (ViWord)
import Vi.Model.Word as ViWord

type InputMethod = String -> ViWord -> Array MarkSymbol

telex :: InputMethod
telex "d" _ = [ConsonantMark Dbar]
telex "a" _ = [VowelMark Acircumflex]
telex "e" _ = [VowelMark Ecircumflex]
telex "o" _ = [VowelMark Ocircumflex]
telex "w" { vowel } =
  case vowel of
    "a"    -> [VowelMark Abreve]
    "o"    -> [VowelMark Ohorn]
    "u"    -> [VowelMark Uhorn]
    "ua"   -> [VowelMark Uhorn]
    "uo"   -> [VowelMark Uhorn, VowelMark Ohorn]
    "ưo"   -> [VowelMark Ohorn]
    _      -> []
telex "z" _ = [ToneMark Level]
telex "f" _ = [ToneMark Grave]
telex "s" _ = [ToneMark Acute]
telex "r" _ = [ToneMark Hook]
telex "x" _ = [ToneMark Tilde]
telex "j" _ = [ToneMark Dot]
telex _   _ = []

readWord :: InputMethod -> String -> Maybe ViWord
readWord method =
    foldM (flip $ snocChar method) ViWord.nil <<< String.chars

snocChar :: InputMethod -> String -> ViWord -> Maybe ViWord
snocChar method c vi =
    let marks = method c vi
        letter = readLetter c
        vi' = case marks of
                [] -> Nothing
                _  -> foldM ViWord.mark vi marks
        vii = ViWord.snoc vi =<< letter
     in vi' <|> vii
