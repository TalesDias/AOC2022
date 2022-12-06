#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-20.3   
  --package "parsec data-ordlist"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

module Main(main) where

import Text.Parsec
import Data.List.Ordered(nub, sort)


main :: IO ()
main = do

  let filename = "input.txt"
  file <- readFile filename

  let contents = parse contentP filename file

  case contents of
    Left e -> print e
    Right ds -> do

      print "Part1"
      (print. findStartMarker) ds

      print "Part2"
      (print. findMsgMarker) ds


  return ()

{-
    Answers
      Part1: 1542
      Part2: 3153
-}

-- Data and Type declarations
type DataStream = String


-- Logic
distinct :: Ord a => [a] -> Bool
distinct xs = xs' == nub xs'
  where
    xs' = sort xs

findStartMarker :: DataStream -> Int
findStartMarker = findM 4
  where
    findM n ds
      | distinct start = n
      | otherwise      = 1 + findM n (tail ds)
      where
        start = take 4 ds

findMsgMarker :: DataStream -> Int
findMsgMarker = findM 14
  where
    findM n ds
      | distinct start = n
      | otherwise      = 1 + findM n (tail ds)
      where
        start = take 14 ds


-- Parsing methods
type Parser = Parsec String ()

numberP :: Parser Int
numberP = read <$> many1 digit <?> "digit"

contentP :: Parser DataStream
contentP = many1 lower


