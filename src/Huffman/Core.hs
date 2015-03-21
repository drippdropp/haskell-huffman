module Huffman.Core where

-- This is for testing, will accept command line input in final version
secretMessage :: String
secretMessage = "this is a secret message we need to encode"

-- This is just for self-documenting purposes, but maybe `length` is sufficient
messageLength :: [a] -> Int
messageLength = length

-- Prepare input for processing
chunkLengths :: [Char] -> [(Int, Int)]
chunkLengths [] = []
chunkLengths (x:xs) = (charValue, cursorLength) :
                      chunkLengths (drop (cursorLength-1) xs)
  where cursor = x : takeWhile (== x) xs
        cursorLength = length cursor
        charValue = Data.Char.ord x

-- Handles initial processing with `chunkLengths`
convertedInput :: [(Int, Int)]
convertedInput = chunkLengths $ qsort secretMessage

-- Function to iterate through processed data and package in HNode
                 -- data objects.
buildHNodes :: [(Int, Int)] -> ( (Int, Int) -> HNode ) -> [HNode]
buildHNodes xs = map (\(x, y) -> (mkHNode ) x y) xs

-- Not sure if this is even necessary, maybe approach this differently.
calcEntropy :: Int -> Int -> Entropy
calcEntropy freq msgLen = read (show freq / show msgLen) :: Entropy

-- buildSingletonTree :: (Int, Int) ->
buildSingletonTree = undefined
