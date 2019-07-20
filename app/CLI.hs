module Main where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans.State
import Data.Coerce
import Data.Maybe
import System.IO
import System.IO.Error
import System.Random
import Unsafe.Coerce

import Paths_perplex

import qualified Trie as T
-- import Trie (Trie)

import Perplex

main = do
  wordsFile <- getDataFileName "words.txt"
  raw <- filter ((>2) . length) . lines <$> readFile wordsFile

  let dict = T.fromList raw
      letterFreqs = freqs (concat raw)

  forkIO $ void (evaluate (force dict))
  forkIO $ void (evaluate (force letterFreqs))

  let loop :: Int -> StdGen -> IO ()
      loop n g = do
        putStr' ("Board size? (" ++ show n ++ "): ")
        n' <- fromMaybe n . read' <$> getLine

        let (b, g') = runState (genBoard (coerce <$> pick letterFreqs) n') g
        print b
        let prompt = "(Any key to see answers)"
        putStr' prompt
        anyKey

        putStrLn $ "\r" ++ padRight (length prompt) ' ' "All the words I found:"
        putStrLn (unwords . T.toList . T.intersection dict $ unsafeCoerce (candidates n' b))
        putStrLn ""
        loop n' g'

  (getStdGen >>= loop 4) `catchIOError` (\e -> if isEOFError e then return () else ioError e)

read' :: Read a => String -> Maybe a
read' s = case reads s of
  [(x,"")] -> Just x
  _      -> Nothing

putStr' :: String -> IO ()
putStr' s = putStr s >> hFlush stdout

padRight :: Int -> a -> [a] -> [a]
padRight n c s = s ++ replicate (n - length s) c

anyKey :: IO ()
anyKey = bracket_
    (do hSetBuffering stdin NoBuffering
        hSetEcho stdin False)
    (do hSetBuffering stdin LineBuffering
        hSetEcho stdin True)
    (void getChar)
