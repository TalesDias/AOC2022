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

main :: IO ()
main = do
  file <- readFile "input.txt"

  let contents = parse contentP "input.txt" file

  case contents of
    Left e -> print e
    Right assigs -> do

      print "Part1"
      (print. length. containedAreas) assigs


      print "Part2"
      (print. length. overlapingAreas) assigs



  return ()

{-
    Answers
      Part1: 584
      Part2: 933
-}

-- Data and Type declarations
data Assignment =
  A {
    start ::Int,
    end :: Int
    }
  deriving Show


-- Logic
containedAreas :: [(Assignment, Assignment)] -> [(Assignment, Assignment)]
containedAreas = filter f'
    where
      f' (a,b) = contains a b
              || contains b a

contains :: Assignment -> Assignment -> Bool
contains a b = left && right
  where
    left  = start a <= start b
    right = end a >= end b


overlapingAreas :: [(Assignment, Assignment)] -> [(Assignment, Assignment)]
overlapingAreas = filter (uncurry overlaps)

overlaps :: Assignment -> Assignment -> Bool
overlaps a b 
  | end a < start b = False
  | end b < start a = False
  | otherwise       = True


-- Parsing methods
type Parser = Parsec String ()


numberP :: Parser Int
numberP = read <$> many1 digit <?> "digit"

assignmentP :: Parser Assignment
assignmentP = A <$> numberP <* char '-' <*> numberP

pairP :: Parser (Assignment, Assignment)
pairP = (,) <$> assignmentP <* char ',' <*> assignmentP

contentP :: Parser [(Assignment, Assignment)]
contentP = pairP  `sepEndBy1` newline

