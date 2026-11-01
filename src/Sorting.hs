module Sorting (Entry (..), countLetter, sortByLetter) where

import Prelude hiding (Word)

import Data.Char (toLower)
import Data.List (maximumBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

import Alphabet (mergeKey, sortKey)
import Types

data Entry = Entry
  { entryWord :: Word
  , entryLetters :: Int -- скільки разів у слові трапляється літера
  , entryFreq :: Int -- скільки разів слово трапляється в тексті
  }

-- Без урахування регістру.
countLetter :: Char -> Word -> Int
countLetter l = length . filter ((== toLower l) . toLower) . wordChars

-- За зростанням кількості літери, при однаковій кількості - за абеткою.
-- Ключ абетки той самий, що в Ord Word, але обчислюється один раз для кожного слова.
-- unique: однакові слова (без урахування регістру) об'єднуються, показується найчастіша форма.
sortByLetter :: Char -> Bool -> [Word] -> [Entry]
sortByLetter l unique ws = sortOn (\e -> (entryLetters e, sortKey (wordChars (entryWord e)))) entries
  where
    entries
      | unique = map merged (Map.elems groups)
      | otherwise = [Entry w (countLetter l w) 1 | w <- ws]
    groups = Map.fromListWith (Map.unionWith (+)) [(mergeKey (wordChars w), Map.singleton w (1 :: Int)) | w <- ws]
    merged forms = Entry w (countLetter l w) (sum (Map.elems forms))
      where
        w = fst (maximumBy (comparing snd <> flip (comparing fst)) (Map.toList forms))
