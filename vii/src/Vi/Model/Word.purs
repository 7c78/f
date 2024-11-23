module Vi.Model.Word where

import Prelude
import Control.Alternative ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (null) as String
import Vi.Data.String (last, notNull) as String
import Vi.Model.Symbol (ConsonantMarkSymbol, LetterSymbol(..), MarkSymbol(..), ToneMarkSymbol(..), VowelMarkSymbol)
import Vi.Model.Word.Internal (finalConsonants, initialConsonants, vowels, markConsonantS, markS, toneS)

type ViWord =
    { initial  :: String
    , glide    :: String
    , vowel    :: String
    , final    :: String
    , toneMark :: ToneMarkSymbol
    }

nil :: ViWord
nil = { initial:  ""
      , glide:    ""
      , vowel:    ""
      , final:    ""
      , toneMark: Level
      }

mark :: ViWord -> MarkSymbol -> Maybe ViWord
mark w (ConsonantMark m) = viMarkInitial w m
mark w (VowelMark m)     = viMark w m
mark w (ToneMark m)      = viTone w m

snoc :: ViWord -> LetterSymbol -> Maybe ViWord
snoc w@{vowel} (Consonant c)
    | String.null vowel    = viInitial w c
    | otherwise            = viFinal w c
snoc w@{vowel, final} (Vowel c)
    | String.notNull final = Nothing
    | String.null vowel    =   viInitialIU w c
                           <|> viMonophthong w c
    | otherwise            =   viGlide w c
                           <|> viDiphthong w c
                           <|> viSemivowelFinal w c

viMarkInitial :: ViWord -> ConsonantMarkSymbol -> Maybe ViWord
viMarkInitial w@{initial} m = do
    initial' <- markConsonantS m initial
    pure w{initial = initial'}

viMark :: ViWord -> VowelMarkSymbol -> Maybe ViWord
viMark w@{vowel} m = do
    vowel' <- markS m vowel
    pure w{vowel = vowel'}

viTone :: ViWord -> ToneMarkSymbol -> Maybe ViWord
viTone w@{toneMark, vowel} m =
    case toneMark, m of
      Level, Level -> Nothing
      _,     Level -> Just w{toneMark = m}
      _,     _     ->
        case toneS m vowel of
          Nothing  -> Nothing
          Just _   -> Just w{toneMark = m}

viFinal :: ViWord -> String -> Maybe ViWord
viFinal w@{final} c =
    let final' = final <> c
    in if final' `Set.member` finalConsonants
          then Just w{final = final'}
          else Nothing

viInitial :: ViWord -> String -> Maybe ViWord
viInitial w@{initial} c =
    let initial' = initial <> c
    in if initial' `Set.member` initialConsonants
          then Just w{initial = initial'}
          else Nothing

viInitialIU :: ViWord -> String -> Maybe ViWord
viInitialIU w@{initial} c =
    case initial, c of
        "g", "i" -> Just w{initial = "gi"}
        "q", "u" -> Just w{initial = "qu"}
        _,   _   -> Nothing

viMonophthong :: ViWord -> String -> Maybe ViWord
viMonophthong w c =
    Just w{vowel = c}

viGlide :: ViWord -> String -> Maybe ViWord
viGlide w@{initial, vowel} c =
    case vowel of
        "o" | c `Set.member` ae ->
            just "o"
        "u" | c == "o" && initial `Set.member` hqt ->
            just "u"
        "u" | c `Set.member` ey ->
            just "u"
        _otherwise ->
            Nothing
    where ae = Set.fromFoldable ["a", "e"]
          ey = Set.fromFoldable ["e", "y"]
          hqt = Set.fromFoldable ["h", "q", "th"]
          just x = Just w{glide = x, vowel = c}

viDiphthong :: ViWord -> String -> Maybe ViWord
viDiphthong w@{vowel} c =
    let vowel' = vowel <> c
    in if vowel' `Set.member` vowels
          then Just w{vowel = vowel'}
          else Nothing

viSemivowelFinal :: ViWord -> String -> Maybe ViWord
viSemivowelFinal w@{vowel} c =
    let n = String.last vowel
    in case c of
        "o" | n `Set.member` ae    -> just "o"
        "u" | n `Set.member` aeiou -> just "u"
        "i" | n `Set.member` aou   -> just "i"
        "y" | n `Set.member` aou   -> just "y"
        _otherwise                 -> Nothing
    where ae = Set.fromFoldable ["a", "e"]
          aeiou = Set.fromFoldable
            [ "a", "â"
            , "e", "ê"
            , "i"
            , "o", "ơ"
            , "u", "ư"
            ]
          aou = Set.fromFoldable
            [ "a", "ă", "â"
            , "o", "ô", "ơ"
            , "u", "ư"
            ]
          just x = Just w{final = x}

toString :: ViWord -> String
toString {initial, glide, vowel, final, toneMark} =
    initial <> glide <> vowel' <> final
  where
    vowel' =
      case toneMark of
        Level  -> vowel
        _other -> fromMaybe vowel (toneS toneMark vowel)
