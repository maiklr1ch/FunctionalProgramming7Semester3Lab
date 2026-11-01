module Parser (parseText) where

import Prelude hiding (Word)

import Data.Char (isDigit, isLetter, isLower, isMark, isSpace)

import Alphabet (apostrophes, hyphens)
import Types

parseText :: String -> Text
parseText = Text . paragraphs . sentences . tokenize . compose . newlines . filter (/= '\xFEFF')

newlines :: String -> String
newlines ('\r' : '\n' : s) = '\n' : newlines s
newlines ('\r' : s) = '\n' : newlines s
newlines (c : s) = c : newlines s
newlines [] = []

-- «и» + бреве -> «й», «і» + діерезис -> «ї» (розкладені форми трапляються в текстах з PDF).
compose :: String -> String
compose (a : m : s)
  | Just c <- lookup (a, m) pairs = c : compose s
  where
    pairs = [(('и', '\x306'), 'й'), (('И', '\x306'), 'Й'), (('і', '\x308'), 'ї'), (('І', '\x308'), 'Ї')]
compose (c : s) = c : compose s
compose [] = []

punctuationMarks :: String
punctuationMarks = ".,;:!?…-‐‑‒–—―()[]{}\"'«»„“”‘’‚‹›"

isWordChar :: Char -> Bool
isWordChar c = isLetter c || isDigit c || isMark c || c == '_'

tokenize :: String -> [Element]
tokenize [] = []
tokenize ('\n' : rest) = ENewline : tokenize rest
tokenize s@(c : rest)
  | isSpace c = ESpace : tokenize (dropWhile isHSpace rest)
  | isWordChar c = let (w, rest') = spanWord s in classify w ++ tokenize rest'
  | c `elem` punctuationMarks = EPunct (Punctuation c) : tokenize rest
  | otherwise = ESymbol (Symbol c) : tokenize rest
  where
    isHSpace x = isSpace x && x /= '\n'

-- Апостроф і дефіс приєднуються, якщо після них літера (об'єкт, будь-який, 2-й),
-- крапка й кома - якщо стоять між цифрами числа (3.14, 1,5).
spanWord :: String -> (String, String)
spanWord = go []
  where
    go acc (c : rest@(d : _))
      | joins c d acc = go (c : acc) rest
    go acc (c : rest)
      | isWordChar c = go (c : acc) rest
    go acc rest = (reverse acc, rest)
    joins c d acc
      | c `elem` apostrophes = afterLetter acc && isLetter d
      | c `elem` hyphens = (afterLetter acc || afterDigit acc) && isLetter d
      | c `elem` ".," = not (null acc) && all (\x -> isDigit x || x `elem` ".,") acc && isDigit d
      | otherwise = False
    afterLetter (x : _) = isLetter x || isMark x
    afterLetter [] = False
    afterDigit (x : _) = isDigit x
    afterDigit [] = False

classify :: String -> [Element]
classify w
  | any isLetter w = [EWord (Word (map Symbol w))]
  | all isDigit (filter (`notElem` ".,") w) = [ENumber (Number (map Symbol w))]
  | otherwise = map (ESymbol . Symbol) w

-- Пробіли й переноси після речення належать йому, тому render відтворює весь текст.
sentences :: [Element] -> [Sentence]
sentences = go []
  where
    go acc [] = [Sentence (reverse acc) | hasContent acc]
    go acc ts@(t : rest)
      | isLayout t =
          let (gap, rest') = span isLayout ts
              acc' = reverse gap ++ acc
           in if hasContent acc && (blankLine gap || (endsSentence acc && startsSentence rest'))
                then Sentence (reverse acc') : go [] rest'
                else go acc' rest'
      | otherwise = go (t : acc) rest
    hasContent = any (not . isLayout)
    blankLine gap = length [() | ENewline <- gap] >= 2

-- acc - у зворотному порядку. Речення має містити слово, тому «1.» у заголовку його не завершує.
endsSentence :: [Element] -> Bool
endsSentence acc = case dropWhile isCloser acc of
  EPunct (Punctuation c) : _ -> c `elem` ".!?…" && not (null [() | EWord _ <- acc])
  _ -> False
  where
    isCloser (EPunct (Punctuation c)) = c `elem` ")]}»\"”’'›"
    isCloser _ = False

-- Після «т. д.», «1990 р.» далі йде слово з малої літери - це не кінець речення.
startsSentence :: [Element] -> Bool
startsSentence (EWord w : _) = case wordChars w of
  c : _ -> not (isLower c)
  [] -> True
startsSentence _ = True

paragraphs :: [Sentence] -> [Paragraph]
paragraphs [] = []
paragraphs ss = case break endsParagraph ss of
  (p, s : rest) -> Paragraph (p ++ [s]) : paragraphs rest
  (p, []) -> [Paragraph p]
  where
    endsParagraph (Sentence es) = length [() | ENewline <- takeWhile isLayout (reverse es)] >= 2
