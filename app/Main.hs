module Main where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import System.Environment
import Control.Monad
import System.IO
import Lib

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched  <- foldM (\bytes func -> func bytes) imageFile
                              [ randomReplaceByte
                              , randomSortSection
                              , randomReplaceByte
                              , randomSortSection
                              , randomReplaceByte
                              , randomReverseByte
                              ]
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"
