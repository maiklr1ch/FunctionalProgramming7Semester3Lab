module Console (askLine, askInt, askYesNo, trim, plural, wrapList) where

import Data.Char (isSpace, toLower)
import System.Exit (exitSuccess)
import System.IO (hFlush, isEOF, stdout)
import Text.Read (readMaybe)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- Кінець вводу (Ctrl+D) завершує програму.
askLine :: String -> IO String
askLine prompt = do
  putStr prompt
  hFlush stdout
  eof <- isEOF
  if eof then putStrLn "" >> exitSuccess else getLine

askInt :: String -> (Int, Int) -> IO Int
askInt label (lo, hi) = do
  s <- trim <$> askLine (label ++ " (" ++ show lo ++ "-" ++ show hi ++ "): ")
  case readMaybe s of
    Just v | v >= lo && v <= hi -> return v
    _ -> putStrLn ("  ! потрібне ціле число від " ++ show lo ++ " до " ++ show hi) >> askInt label (lo, hi)

askYesNo :: String -> Bool -> IO Bool
askYesNo q def = do
  s <- map toLower . trim <$> askLine (q ++ (if def then " [так]: " else " [ні]: "))
  case s of
    "" -> return def
    _
      | s `elem` ["так", "т", "y", "yes"] -> return True
      | s `elem` ["ні", "н", "n", "no"] -> return False
      | otherwise -> putStrLn "  ! введіть «так» або «ні»" >> askYesNo q def

-- «1 слово», «3 слова», «5 слів».
plural :: Int -> (String, String, String) -> String
plural n (one, few, many) = show n ++ " " ++ form
  where
    form
      | n `mod` 10 == 1 && n `mod` 100 /= 11 = one
      | n `mod` 10 `elem` [2, 3, 4] && (n `mod` 100) `notElem` [12, 13, 14] = few
      | otherwise = many

-- Елементи через кому, рядки не довші за width (якщо елемент не довший сам по собі).
wrapList :: Int -> [String] -> [String]
wrapList width = go
  where
    go [] = []
    go (x : xs) = let (line, rest) = fill x xs in line : go rest
    fill line (y : ys)
      | length line + 2 + length y <= width = fill (line ++ ", " ++ y) ys
    fill line ys = (if null ys then line else line ++ ",", ys)
