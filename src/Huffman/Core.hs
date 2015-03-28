module Huffman.Core where

import Data.Char (ord)
import Data.Function (on)
import Huffman.Types
import Sort.QSort

-- This is for testing, will accept command line input in final version
secretMessage :: String
secretMessage = "this is a secret message we need to encode"

-- This is just for self-documenting purposes, but maybe `length` is sufficient
messageLength :: [a] -> Int
messageLength = length

-- Prepare input for processing
chunkLengths :: String -> [(Int, Int)]
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
buildHNodes :: [(Int, Int)] -> Int -> [HNode]
buildHNodes txs len =
  map (\x -> mkHNode $ prepForHNode x len) txs

prepForHNode :: (Int, Int) -> Int -> (Int, Int, Float)
prepForHNode tpl len = (fst tpl, snd tpl, calcEntropy (snd tpl) len)

calcEntropy :: Int -> Int -> Float
calcEntropy = divideToFloat

divideToFloat :: Int -> Int -> Float
divideToFloat = (/) `on` fromIntegral
