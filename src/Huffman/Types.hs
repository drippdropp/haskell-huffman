module Huffman.Types where

----- Huffman Node
----- Represents individual elements of the Huffman tree

data HNode = HNode { nodeValue :: Int
                   , frequency :: Int
                   , entropy   :: Float
                   } deriving (Show)

instance Eq HNode where
  (==) (HNode _ _ a) (HNode _ _ b) = a == b
  (/=) (HNode _ _ a) (HNode _ _ b) = a /= b

instance Ord HNode where
  (HNode _ _ a) `compare` (HNode _ _ b) = if a < b then LT else GT

----- Binary Search Tree

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

instance Ord a => Ord (Tree a) where
  (Node a _ _) `compare` (Node b _ _) = if a < b then LT else GT
  Node{} `compare` _ = GT
  Empty `compare` Empty = EQ

instance Eq a => Eq (Tree a) where
  (==) (Node a _ _) (Node b _ _) = a == b
  (/=) (Node a _ _) (Node b _ _) = a /= b

------------------------------------------------------------------------------
-- Functions for Trees
------------------------------------------------------------------------------

-- Extra value from single tree Node,
-- returning Just <value> or Nothing
getNodeValue :: Tree a -> Maybe a
getNodeValue (Node x _ _) = Just x
getNodeValue (Empty) = Nothing

-- Insert tree into another tree
-- Second entry is added to the first
(+<) :: Ord a => Tree a -> Tree a -> Tree a
(+<) a@(Node ax ay az) b@Node{} =
  if b <= a
  then case ay of
        Empty  -> Node ax b az
        Node{} -> Node ax (ay +< b) az
  else case az of
        Empty  -> Node ax ay b
        Node{} -> Node ax ay (az +< b)
(+<) Empty Empty = Empty
(+<) a@Node{} Empty = a
(+<) Empty a@Node{} = a

-- Same as (+<), but first tree is inserted into second
(>+) :: Ord a => Tree a -> Tree a -> Tree a
(>+) a@Node{} b@Node{} = b +< a
(>+) a@Node{} Empty = a
(>+) Empty a@Node{} = a
(>+) Empty Empty = Empty

-- Verbal alias for (+<)
intoTree :: Ord a => Tree a -> Tree a -> Tree a
a `intoTree` b = b +< a

-- Check if an element exists in the tree
(=?) :: (Ord a, Eq a) => Tree a -> a -> Bool
(=?) Empty _ = False
(=?) (Node x y z) el
  | x == el  = True
  | x >  el  = y =? el
  | x <  el  = z =? el
  | otherwise  = False

-- Verbal alias for (=?)
hasElem :: (Ord a, Eq a) => Tree a -> a -> Bool
x `hasElem` y = x =? y

-- Get the absolute minimum value stored in the tree
treeMin :: Ord a => Tree a -> Maybe a
treeMin (Node x Empty _) = Just x
treeMin (Node _ y _)     = treeMin y
treeMin Empty            = Nothing

-- Get the absolute maximum value stored in the tree
treeMax :: Ord a => Tree a -> Maybe a
treeMax (Node x _ Empty) = Just x
treeMax (Node _ _ z)     = treeMax z
treeMax Empty            = Nothing

-- Make a new Huffman Node structure
mkHNode :: (Int, Int, Float) -> HNode
mkHNode (a, b, c) = HNode a b c
