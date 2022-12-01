#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-18.21   
  --package "parsec"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}


module Main(main) where

import Text.Parsec
import Data.List(sortBy)

main :: IO ()
main = do

  file <- readFile "input.txt"
  let contents = parse contentP "input.txt" file

  case contents of
    Left e -> print e
    Right cals -> do

      print "Part1"
      (print. mostCalories) cals

      print "Part2"
      (print. top3Calories) cals

  return ()

{-
   "Part1"
    70116

    "Part2"
    206582
-}

-- Data and Type declarations
newtype Elf = MkElf [Int] 
  deriving (Show, Eq, Ord)

-- Logic
mostCalories :: [Elf] -> Int
mostCalories = maximum . map (\(MkElf x) -> sum x)


top3Calories :: [Elf] -> Int
top3Calories = sum . take 3 
              . sortBy (flip compare) 
              . map (\(MkElf x) -> sum x)

-- Parsing methods
type Parser = Parsec String ()


numberP :: Parser Int
numberP = read <$> many1 digit <?> "digit"

elfP :: Parser Elf
elfP = MkElf <$> many1 (numberP <* newline)

contentP :: Parser [Elf]
contentP = elfP `sepEndBy1` newline
