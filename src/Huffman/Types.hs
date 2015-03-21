module Huffman.Types where

type Freq = Int
data Entropy = Entropy { eValue :: Double } deriving (Show, Ord, Eq)

data HNode = HNode { nodeValue :: Int
                   , frequency :: Freq
                   , entropy   :: Entropy
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
mkHNode :: (Int, Int) -> Entropy -> HNode
mkHNode (nv, freq) = HNode nv freq
