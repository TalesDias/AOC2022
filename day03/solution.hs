#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-18.21   
  --package "parsec split containers"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

module Main(main) where

import Data.Char

import Debug.Trace
import Text.Parsec
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split(chunksOf)


main :: IO ()
main = do
  file <- readFile "input.txt"

  let contents1 = parse contentP "input.txt" file

  case contents1 of
    Left e -> print e
    Right conts -> do

      print "Part1"
      (print. sumPriorities) conts


      print "Part2"
      (print. sumBadges) conts



  return ()

{-
   "Part1"
    8240

    "Part2"
    2587
-}

-- Data and Type declarations
data Rucksack = R String String
  deriving Show


-- Logic
sumPriorities :: [Rucksack] -> Int
sumPriorities = foldr (\ x -> (+) (priority $ wrongItem x)) 0

priority :: Char -> Int
priority c
  | isLower c = ord c - 96
  | otherwise = ord c - 38

wrongItem :: Rucksack -> Char
wrongItem (R xs ys) = Set.findMin $ Set.intersection xs' ys'
  where
    xs' = Set.fromList xs
    ys' = Set.fromList ys


collectItens :: [Rucksack] -> [[String]]
collectItens = chunksOf 3. foldl (\ rs (R x y) -> (x++y):rs) []

sumBadges :: [Rucksack] -> Int
sumBadges = sum. map (priority. commonItem). collectItens

commonItem :: [String] -> Char
commonItem = Set.findMin
           . foldl1 Set.intersection
           . map Set.fromList


-- Parsing methods
type Parser = Parsec String ()


numberP :: Parser Int
numberP = read <$> many1 digit <?> "digit"

mkRucksack :: [String] -> Rucksack
mkRucksack [a, b] = R a b

rucksackP :: Parser Rucksack
rucksackP = mkRucksack. ((flip div 2. length) >>= chunksOf) <$> many1 letter

contentP :: Parser [Rucksack]
contentP = rucksackP  `sepEndBy1` newline

