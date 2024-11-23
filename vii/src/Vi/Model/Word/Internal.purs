module Vi.Model.Word.Internal where

import Prelude
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.Tuple.Unsafe (type (&), (&))
import Effect.Exception.Unsafe (unsafeThrow)
import Vi.Data.String ((!), replace)
import Vi.Model.Symbol (ConsonantMarkSymbol(..), ToneMarkSymbol(..), VowelMarkSymbol(..))

initialConsonants :: Set String
initialConsonants = Set.fromFoldable $ Array.concat
    [ [ "b", "c", "d", "đ", "g", "h", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "x" ]
    , [ "ch", "gh", "gi", "kh", "ng", "nh", "ph", "qu", "th", "tr" ]
    , [ "ngh" ]
    ]

finalConsonants :: Set String
finalConsonants = Set.fromFoldable
    [ "c", "ch", "m", "n", "ng", "nh", "p", "t" ]

vowels :: Set String
vowels = Set.fromFoldable $ Array.concat
    [ [ "a", "ă", "â", "e", "ê", "i", "o", "ô", "ơ", "u", "ư", "y" ]
    , Array.concat
        [ -- | iê
          "i" `di` [ "a", "e", "ê" ]
        , "y" `di` [ "a", "e", "ê" ]
          -- | ươ
        , "ư" `di` [ "a", "o", "ơ" ]
          -- | uô
        , "u" `di` [ "a", "o", "ô" ]
        ]
    ]
    where di x = map (\y -> x <> y)

semivowelsJ :: Set String
semivowelsJ = Set.fromFoldable [ "i", "y" ]

semivowelsW :: Set String
semivowelsW = Set.fromFoldable [ "o", "u" ]

glides :: Set String
glides = semivowelsW

finalSemivowels :: Set String
finalSemivowels = semivowelsJ `Set.union` semivowelsW

markedConsonants :: Map (String & ConsonantMarkSymbol) String
markedConsonants = Map.fromFoldable
    [ ("d" & Dbar) & "đ" ]

markConsonantS :: ConsonantMarkSymbol -> String -> Maybe String
markConsonantS m x = Map.lookup (x & m) markedConsonants

markedVowels :: Map (String & VowelMarkSymbol) String
markedVowels = Map.fromFoldable
    [ "a"  # (mark Abreve)      "ă"
    , "a"  # (mark Acircumflex) "â"

    , "e"  # (mark Ecircumflex) "ê"
    , "ie" # (mark Ecircumflex) "iê"
    , "ye" # (mark Ecircumflex) "yê"

    , "o"  # (mark Ocircumflex) "ô"
    , "uo" # (mark Ocircumflex) "uô"

    , "o"  # (mark Ohorn)       "ơ"
    , "uo" # (mark Ohorn)       "uơ"
    , "ưo" # (mark Ohorn)       "ươ"

    , "u"  # (mark Uhorn)       "ư"
    , "ua" # (mark Uhorn)       "ưa"
    , "uo" # (mark Uhorn)       "ưo"
    ]
    where mark m y x = (x & m) & y

markS :: VowelMarkSymbol -> String -> Maybe String
markS m xs = Map.lookup (xs & m) markedVowels

tonedVowels1 :: Map (String & ToneMarkSymbol) String
tonedVowels1 = Map.fromFoldable $ Array.concat
    [ "a" `tone` [ "à", "á", "ả", "ã", "ạ" ]
    , "ă" `tone` [ "ằ", "ắ", "ẳ", "ẵ", "ặ" ]
    , "â" `tone` [ "ầ", "ấ", "ẩ", "ẫ", "ậ" ]
    , "e" `tone` [ "è", "é", "ẻ", "ẽ", "ẹ" ]
    , "ê" `tone` [ "ề", "ế", "ể", "ễ", "ệ" ]
    , "i" `tone` [ "ì", "í", "ỉ", "ĩ", "ị" ]
    , "o" `tone` [ "ò", "ó", "ỏ", "õ", "ọ" ]
    , "ô" `tone` [ "ồ", "ố", "ổ", "ỗ", "ộ" ]
    , "ơ" `tone` [ "ờ", "ớ", "ở", "ỡ", "ợ" ]
    , "u" `tone` [ "ù", "ú", "ủ", "ũ", "ụ" ]
    , "ư" `tone` [ "ừ", "ứ", "ử", "ữ", "ự" ]
    ]
  where
    tone level [grave, acute, hook, tilde, dot] =
        [ (level & Grave) & grave
        , (level & Acute) & acute
        , (level & Hook)  & hook
        , (level & Tilde) & tilde
        , (level & Dot)   & dot
        ]
    tone _ _ = unsafeThrow "tonedVowels1"

tonedVowels2 :: Map (String & ToneMarkSymbol) String
tonedVowels2 = Map.fromFoldable $ Array.concat
    [ "ia" `tone` 0
    , "ua" `tone` 0
    , "ưa" `tone` 0

    , "ie" `tone` 1
    , "iê" `tone` 1
    , "ye" `tone` 1
    , "yê" `tone` 1
    , "uo" `tone` 1
    , "uô" `tone` 1
    , "ưo" `tone` 1
    , "ươ" `tone` 1
    ]
  where
    toned1 x = Map.toUnfoldable
             $ Map.filterWithKey (\(src & _) _ -> src == x)
             $ tonedVowels1

    tone xs i =
        let x = xs!i
        in map (entry xs) (toned1 x)

    entry xs ((x & m) & y) =
        let ys = replace x y xs
        in (xs & m) & ys

tonedVowels :: Map (String & ToneMarkSymbol) String
tonedVowels = Map.union tonedVowels1 tonedVowels2

toneS :: ToneMarkSymbol -> String -> Maybe String
toneS m xs = Map.lookup (xs & m) tonedVowels
