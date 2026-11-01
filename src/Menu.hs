module Menu (runMenu) where

import Prelude hiding (Word)

import Control.Exception (IOException, catch, try)
import Control.Monad (unless)
import Data.Char (isLetter, toLower)
import Data.Function (on)
import Data.List (groupBy)
import qualified Data.Set as Set
import System.IO
import Text.Printf (printf)

import Alphabet (mergeKey, script)
import Console
import Parser (parseText)
import Sorting
import Types

data Doc = Doc FilePath Text

defaultFile :: FilePath
defaultFile = "examples/textbook.txt"

runMenu :: Maybe FilePath -> IO ()
runMenu arg = do
  first <- maybe (return Nothing) load arg
  doc <- maybe askFile return first
  loop doc

askFile :: IO Doc
askFile = do
  p <- trim <$> askLine ("Файл з текстом [" ++ defaultFile ++ "]: ")
  load (if null p then defaultFile else p) >>= maybe askFile return

load :: FilePath -> IO (Maybe Doc)
load path = do
  res <- try (readTextFile path)
  case res of
    Left e -> putStrLn ("  ! Не вдалося прочитати файл: " ++ show (e :: IOException)) >> return Nothing
    Right s -> return (Just (Doc path (parseText s)))

-- UTF-8, а якщо файл у ньому не читається - Windows-1251.
readTextFile :: FilePath -> IO String
readTextFile path = readWith utf8 `catch` retry
  where
    retry :: IOException -> IO String
    retry _ = mkTextEncoding "CP1251" >>= readWith
    readWith enc = withFile path ReadMode $ \h -> do
      hSetEncoding h enc
      s <- hGetContents h
      length s `seq` return s

loop :: Doc -> IO ()
loop doc@(Doc path text) = do
  putStrLn "\n=== Обробка тексту підручника ==="
  putStrLn ("Файл: " ++ path)
  putStrLn (stats text)
  putStrLn " 1. Завантажити інший файл"
  putStrLn " 2. Показати текст (табуляції й послідовності пробілів замінено одним пробілом)"
  putStrLn " 3. Показати речення"
  putStrLn " 4. Розбір речення на лексеми"
  putStrLn " 5. Розсортувати слова за кількістю заданої літери"
  putStrLn " 0. Вихід"
  c <- trim <$> askLine "Ваш вибір: "
  case c of
    "1" -> otherFile >>= loop . maybe doc id
    "2" -> output (lines (render text)) >> loop doc
    "3" -> output (zipWith (\i s -> show i ++ ". " ++ flatten s) [1 :: Int ..] (sentencesOf text)) >> loop doc
    "4" -> analyze text >> loop doc
    "5" -> sortTask text >> loop doc
    "0" -> putStrLn "До побачення!"
    _ -> putStrLn "Невідомий пункт меню." >> loop doc

otherFile :: IO (Maybe Doc)
otherFile = do
  p <- trim <$> askLine "Шлях до файлу (Enter - залишити поточний): "
  if null p then return Nothing else load p

stats :: Text -> String
stats t@(Text ps) =
  printf "Абзаців: %d, речень: %d, слів: %d, чисел: %d, розділових знаків: %d, інших символів: %d"
    (length ps) (length (sentencesOf t)) (count "слово") (count "число") (count "розділовий знак") (count "символ")
  where
    kinds = [k | Just k <- map kind (elements t)]
    count k = length (filter (== k) kinds)

kind :: Element -> Maybe String
kind (EWord _) = Just "слово"
kind (ENumber _) = Just "число"
kind (EPunct _) = Just "розділовий знак"
kind (ESymbol _) = Just "символ"
kind _ = Nothing

-- Речення в один рядок: кожна послідовність пробілів і переносів - один пробіл.
flatten :: Sentence -> String
flatten (Sentence es) = trim (go es)
  where
    go [] = ""
    go xs@(x : rest)
      | isLayout x = ' ' : go (dropWhile isLayout xs)
      | otherwise = render x ++ go rest

analyze :: Text -> IO ()
analyze text = case sentencesOf text of
  [] -> putStrLn "Текст не містить речень."
  ss -> do
    i <- askInt "Номер речення" (1, length ss)
    case drop (i - 1) ss of
      s : _ -> output (flatten s : [k ++ ": " ++ render e | e <- elements s, Just k <- [kind e]])
      [] -> return ()

sortTask :: Text -> IO ()
sortTask text = do
  l <- askLetter
  unique <- askYesNo "Показувати кожне слово один раз (з кількістю повторів)?" True
  output (report l (wordsOf text) (sortByLetter l unique (wordsOf text)))

askLetter :: IO Char
askLetter = do
  s <- trim <$> askLine "Літера: "
  case s of
    [c] | isLetter c -> return (toLower c)
    _ -> putStrLn "  ! потрібна одна літера" >> askLetter

report :: Char -> [Word] -> [Entry] -> [String]
report l ws es =
  [ printf "Літера «%c»%s. Слів у тексті: %d, різних: %d." l note (length ws) distinct
  , printf "Слова за зростанням кількості «%c», з однаковою кількістю - за абеткою." l
  ]
    ++ ["Літера не трапляється в жодному слові." | not (null es), all ((== 0) . entryLetters) es]
    ++ concatMap block (groupBy ((==) `on` entryLetters) es)
  where
    note = maybe "" (\s -> " (" ++ s ++ ")") (script l)
    distinct = Set.size (Set.fromList (map (mergeKey . wordChars) ws))
    block g@(e : _) =
      ["", printf "Кількість «%c» = %d (%s):" l (entryLetters e) (plural (length g) ("слово", "слова", "слів"))]
        ++ wrapList 100 (map item g)
    block [] = []
    item e = render (entryWord e) ++ (if entryFreq e > 1 then " (" ++ show (entryFreq e) ++ ")" else "")

-- Виводить рядки і пропонує зберегти їх у файл.
output :: [String] -> IO ()
output ls = do
  mapM_ putStrLn ls
  path <- trim <$> askLine "Зберегти у файл (Enter - не зберігати): "
  unless (null path) $ do
    res <- try (writeFile path (unlines ls)) :: IO (Either IOException ())
    putStrLn (either (\e -> "  ! Помилка запису: " ++ show e) (const ("Збережено: " ++ path)) res)
