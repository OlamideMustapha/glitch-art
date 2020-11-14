module Lib
    ( randomReplaceByte
    , randomReverseByte
    , randomSortSection
    ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import System.Random
import Data.List


intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]


replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte target int byte = mconcat [before, intChar,rest]
  where
    (before, rest) = BC.splitAt target byte
    intChar        = intToBC int

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal  <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)



sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest)  = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed         = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)



reverseByte :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseByte start size bytes = mconcat [before, reversedBytes, after]
  where
    (before, rest)  = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    reversedBytes   = BC.reverse target

randomReverseByte :: BC.ByteString -> IO BC.ByteString
randomReverseByte bytes = do
  let size = 30
  let len  = BC.length bytes
  start <- randomRIO (0, len - size)
  return (reverseByte start size bytes)
