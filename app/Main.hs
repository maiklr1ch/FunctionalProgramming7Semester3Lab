module Main (main) where

import Data.Maybe (listToMaybe)
import GHC.IO.Encoding (setLocaleEncoding)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, hSetEncoding, stderr, stdin, stdout, utf8)

import Menu (runMenu)

main :: IO ()
main = do
  setLocaleEncoding utf8
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  hSetBuffering stdout LineBuffering
  getArgs >>= runMenu . listToMaybe
