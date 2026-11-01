module Types
  ( Symbol (..)
  , Word (..)
  , Number (..)
  , Punctuation (..)
  , Element (..)
  , Sentence (..)
  , Paragraph (..)
  , Text (..)
  , Render (..)
  , Container (..)
  , wordChars
  , wordsOf
  , sentencesOf
  , isLayout
  ) where

import Prelude hiding (Word)

import Data.Ord (comparing)

import Alphabet (sortKey)

-- Символ.
newtype Symbol = Symbol Char
  deriving (Eq)

-- Слово: літери, цифри, '_', а також апостроф і дефіс усередині (об'єкт, будь-який).
newtype Word = Word [Symbol]
  deriving (Eq)

-- Число: цифри, можливо з крапкою чи комою всередині (3.14, 1,5).
newtype Number = Number [Symbol]

-- Розділовий знак.
newtype Punctuation = Punctuation Char

-- Лексема.
data Element
  = EWord Word
  | ENumber Number
  | EPunct Punctuation
  | ESymbol Symbol -- інші знаки: + = < > * / # ...
  | ESpace -- будь-яка послідовність пробілів і табуляцій
  | ENewline

-- Речення («пропозиція» в завданні) разом із пробілами й переносами рядків після нього.
newtype Sentence = Sentence [Element]

-- Абзаци розділяються порожнім рядком.
newtype Paragraph = Paragraph [Sentence]

newtype Text = Text [Paragraph]

-- Формування тексту. ESpace завжди дає один пробіл.
class Render a where
  render :: a -> String

instance Render Symbol where
  render (Symbol c) = [c]

instance Render Word where
  render (Word s) = concatMap render s

instance Render Number where
  render (Number s) = concatMap render s

instance Render Punctuation where
  render (Punctuation c) = [c]

instance Render Element where
  render (EWord w) = render w
  render (ENumber n) = render n
  render (EPunct p) = render p
  render (ESymbol s) = render s
  render ESpace = " "
  render ENewline = "\n"

instance Render Sentence where
  render (Sentence es) = concatMap render es

instance Render Paragraph where
  render (Paragraph ss) = concatMap render ss

instance Render Text where
  render (Text ps) = concatMap render ps

-- Усі лексеми речення, абзацу чи тексту.
class Container a where
  elements :: a -> [Element]

instance Container Sentence where
  elements (Sentence es) = es

instance Container Paragraph where
  elements (Paragraph ss) = concatMap elements ss

instance Container Text where
  elements (Text ps) = concatMap elements ps

wordsOf :: Container a => a -> [Word]
wordsOf x = [w | EWord w <- elements x]

sentencesOf :: Text -> [Sentence]
sentencesOf (Text ps) = [s | Paragraph ss <- ps, s <- ss]

wordChars :: Word -> String
wordChars (Word s) = [c | Symbol c <- s]

-- Слова впорядковуються за українською абеткою.
instance Ord Word where
  compare = comparing (sortKey . wordChars)

isLayout :: Element -> Bool
isLayout ESpace = True
isLayout ENewline = True
isLayout _ = False
