module Trie where

import Control.DeepSeq
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map (Map)
import Prelude hiding (null)

-- | Simple radix trees
data Trie a = Empty
            | Node Bool (Map a (Trie a))

instance Show a => Show (Trie a) where
  show = show . toList

instance NFData a => NFData (Trie a) where
  rnf Empty = ()
  rnf (Node b ts) = b `deepseq` rnf ts

empty :: Trie a
empty = Empty

node :: Bool -> Map a (Trie a) -> Trie a
node = Node

no :: Map a (Trie a) -> Trie a
no  = node False

yes :: Map a (Trie a) -> Trie a
yes = node True

elem :: Ord a => [a] -> Trie a -> Bool
elem _ Empty = False
elem [] (Node b _) = b
elem (k:ks) (Node _ ts) = case M.lookup k ts of
  Just t -> Trie.elem ks t
  Nothing -> False

size :: Trie a -> Int
size Empty = 0
size (Node False ts) = M.foldl' (\a x -> a + size x) 0 ts
size (Node True ts) = M.foldl' (\a x -> a + size x) 1 ts

single :: [a] -> Trie a
single [] = yes M.empty
single (k:ks) = no (M.singleton k (single ks))

insert :: Ord a => [a] -> Trie a -> Trie a
insert k      Empty       = single k
insert []     (Node _ ts) = yes ts
insert (k:ks) (Node b ts) = node b (M.alter f k ts)
  where
    f = Just . maybe (single ks) (insert ks)

intersection :: Ord a => Trie a -> Trie a -> Trie a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection (Node a ts) (Node b us) = node (a && b) (M.intersectionWith intersection ts us)

union :: Ord a => Trie a -> Trie a -> Trie a
union Empty b = b
union a Empty = a
union (Node a ts) (Node b us) = node (a || b) (M.unionWith union ts us)

toList :: Trie a -> [[a]]
toList t = go [] t []
  where
    go _ Empty z = z
    go ks (Node True ts) z = reverse ks : M.foldrWithKey (\k -> go (k:ks)) z ts
    go ks (Node False ts) z = M.foldrWithKey (\k -> go (k:ks)) z ts

fromList :: Ord a => [[a]] -> Trie a
fromList = L.foldl' (flip insert) empty
