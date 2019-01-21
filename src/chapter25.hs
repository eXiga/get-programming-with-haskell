{-# LANGUAGE OverloadedStrings #-}

module Chapter25 where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Random

-- Quick check 1
bcInt :: BC.ByteString
bcInt = "6"

bcToInt :: BC.ByteString -> Int
bcToInt = read . BC.unpack

glitcher :: IO ()
glitcher = do
  let fileName = "res/lovecraft.jpg"
  imageFile <- BC.readFile fileName
  glitched <- foldM (\b f -> f b) imageFile glitchActions
  let glitchedFileName = "res/lovecraft_glitched.jpg"
  BC.writeFile glitchedFileName glitched
  putStrLn "Done!"

intToChar :: Int -> Char
intToChar int = toEnum $ int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte l c b = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt l b
    after = BC.drop 1 rest
    newChar = intToBC c

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

sortByteSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortByteSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortByteSection start sectionSize bytes)

-- Quick check 2
randomChar :: IO Char
randomChar = do
  randomInt <- randomRIO (0, 255)
  return $ toEnum randomInt

-- Quick check 3
glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions =
  [ randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  , randomReplaceByte
  , randomReverseSection
  , randomSortSection
  ]

-- Final question 1
findDifference :: IO ()
findDifference = do
  let fileName = "res/srcFile.txt"
  input <- B.readFile fileName
  putStrLn $ mconcat ["Bytes: ", show $ B.length input]
  putStrLn $ mconcat ["Chars: ", show $ (T.length . E.decodeUtf8) input]

-- Final question 2
reverseByteSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseByteSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse target

randomReverseSection :: BC.ByteString -> IO BC.ByteString
randomReverseSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (reverseByteSection start sectionSize bytes)
