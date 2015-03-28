module Huffman.Types where

data HNode = HNode { nodeValue :: Int
                   , frequency :: Int
                   , entropy   :: Float
                   } deriving (Show)

instance Eq HNode where
  (==) (HNode _ _ a) (HNode _ _ b) = a == b
  (/=) (HNode _ _ a) (HNode _ _ b) = a /= b

instance Ord HNode where
  (HNode _ _ a) `compare` (HNode _ _ b) = if a < b then LT else GT

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show, Eq)

-- mkHNode -- Makes a new HNode data object.
mkHNode :: (Int, Int, Float) -> HNode
mkHNode (a, b, c) = HNode a b c
