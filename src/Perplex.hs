{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

module Perplex where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Coerce
import System.Random

import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)

import qualified Trie as T
import Trie (Trie)

newtype Letter = Letter { getLetter :: Char }
  deriving (Eq, Ord)

instance Show Letter where
  show = show . getLetter
  showList xs = showList (coerce xs :: String)

instance Random Letter where
  randomR (Letter l, Letter h) g = coerce $ randomR (l,h) g
  random = randomR (Letter 'a', Letter 'z')

newtype Board = Board (Map (Int, Int) Letter)

instance Show Board where
  show (Board b) = unlines $ [ [ coerce (look b (x,y)) | x <- [0..n-1] ] | y <- [0..n-1] ]
    where
      n :: Int
      n = boardSize (Board b)

boardSize :: Board -> Int
boardSize (Board b) = fst (fst (M.findMax b)) + 1

genBoard :: Applicative f => f Letter -> Int -> f Board
genBoard a n = Board . M.fromList <$> traverse (\p -> (p,) <$> a) (keys n)

freqs :: Ord a => [a] -> Map a Int
freqs = foldl (flip $ M.alter (Just . maybe 1 (+1))) M.empty

pick :: (MonadState StdGen m, Monad m, Functor m) => Map Char Int -> m Char
pick t
  | M.null t  = error "pick: letter frequency table is empty"
  | otherwise = go (M.toList t') <$> state (randomR (0, total - 1))
  where
    t' = M.filterWithKey (\k _ -> within 'a' 'z' k) t
    total = M.foldl' (+) 0 t'

    go [] _ = error "pick: random draw exceeds letter frequency total"
    go ((x,m) : xs) n
      | n < m = x
      | otherwise = go xs (n-m)

keys :: (Enum n, Num n) => n -> [(n, n)]
keys n = [ (x,y) | y <- [0..n-1], x <- [0..n-1] ]

getTileAt :: (Int, Int) -> Board -> Letter
getTileAt k (Board b) = look b k

look :: (Show k, Ord k) => Map k v -> k -> v
look b p = case M.lookup p b of
  Just x -> x
  Nothing -> error ("Bad tile " ++ show p)

-- | The strings obtained by valid moves on the board.
candidates :: Int -> Board -> Trie Letter
candidates n (Board b) = T.no . M.fromListWith T.union $ (look b &&& go S.empty) <$> keys n
  where
    go :: Set (Int, Int) -> (Int, Int) -> Trie Letter
    go seen p = T.yes . M.fromListWith T.union $
                  (look b &&& go seen') <$> filter (`S.notMember` seen') (neighbours n p)
      where seen' = S.insert p seen

-- | The neighbouring coordinates of a board tile
neighbours :: (Ord a, Enum a, Num a) => a -> (a, a) -> [(a, a)]
neighbours n (x,y) = filter valid (liftM2 (,) [x-1..x+1] [y-1..y+1])
  where valid (x,y) = within 0 (n-1) x && within 0 (n-1) y

within :: Ord a => a -> a -> a -> Bool
within l h x = x >= l && x <= h
