module Alphabet (apostrophes, hyphens, sortKey, mergeKey, script) where

import Data.Char (isAsciiLower, isDigit, isUpper, ord, toLower)
import Data.List (elemIndex)

-- Український алфавіт; російські ё, ъ, ы, э - на звичних місцях.
cyrillic :: String
cyrillic = "абвгґдеёєжзиіїйклмнопрстуфхцчшщъыьэюя"

apostrophes, hyphens :: String
apostrophes = "'’ʼ`"
hyphens = "-‐"

-- Ключ сортування слова:
-- 1) літери й цифри без урахування регістру (апостроф, дефіс і '_' пропускаються, як у словниках);
-- 2) разом з ними; 3) мала літера перед великою; 4) самі символи.
sortKey :: String -> ([(Int, Int)], [(Int, Int)], [Bool], String)
sortKey w = (map charKey (filter (not . ignorable) w), map charKey w, map isUpper w, w)
  where
    ignorable c = c `elem` apostrophes || c `elem` hyphens || c == '_'

-- Порядок: службові знаки, цифри, кирилиця, латиниця, решта.
charKey :: Char -> (Int, Int)
charKey c
  | l `elem` apostrophes = (0, 0)
  | l `elem` hyphens = (0, 1)
  | l == '_' = (0, 2)
  | isDigit l = (1, ord l)
  | Just i <- elemIndex l cyrillic = (2, i)
  | isAsciiLower l = (3, ord l)
  | otherwise = (4, ord l)
  where
    l = toLower c

-- Для об'єднання повторів: без урахування регістру й різновиду апострофа.
mergeKey :: String -> String
mergeKey = map canon
  where
    canon c
      | c `elem` apostrophes = '\''
      | c `elem` hyphens = '-'
      | otherwise = toLower c

script :: Char -> Maybe String
script c
  | toLower c `elem` cyrillic = Just "кирилиця"
  | isAsciiLower (toLower c) = Just "латиниця"
  | otherwise = Nothing
