{-# LANGUAGE OverloadedStrings #-}

module Chapter24 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO

-- Quick check 2
processFile :: IO ()
processFile = do
  srcFile <- openFile "res/srcFile.txt" ReadMode
  isSrcFileEmpty <- hIsEOF srcFile
  firstLine <-
    if not isSrcFileEmpty
      then hGetLine srcFile
      else return "empty"
  putStrLn firstLine
  hasSrcFileSecondLine <- hIsEOF srcFile
  secondLine <-
    if not hasSrcFileSecondLine
      then hGetLine srcFile
      else return ""
  dstFile <- openFile "res/dstFile.txt" WriteMode
  hPutStrLn dstFile secondLine
  hClose srcFile
  hClose dstFile
  putStrLn "Done!"

type FileStats = (Int, Int, Int)

getInputStats :: T.Text -> FileStats
getInputStats input = (charCount, wordCount, lineCount)
  where
    charCount = T.length input
    wordCount = (length . T.words) input
    lineCount = (length . T.lines) input

processStats :: FileStats -> T.Text
processStats (charCount, wordCount, lineCount) =
  T.pack $
  unwords
    [ "chars:"
    , show charCount
    , ", words:"
    , show wordCount
    , ", lines:"
    , show lineCount
    ]

getFileStats :: IO ()
getFileStats = do
  let fileName = "res/srcFile.txt"
  input <- TIO.readFile fileName
  let fileStats = (processStats . getInputStats) input
  TIO.appendFile "res/stats.dat" $
    mconcat [T.pack fileName, " ", fileStats, "\n"]
  TIO.putStrLn "Processing is over!"

-- Final question 1
cp :: FilePath -> FilePath -> IO ()
cp src dst = do
  srcFileContent <- TIO.readFile src
  TIO.writeFile dst srcFileContent
  TIO.putStrLn $ mconcat ["Copied: ", T.pack src, " -> ", T.pack dst]

-- Final question 2
rewriter :: FilePath -> IO ()
rewriter file = do
  fileContent <- TIO.readFile file
  TIO.writeFile file $ T.toUpper fileContent
