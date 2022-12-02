#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-18.21   
  --package "parsec"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

{-# LANGUAGE LambdaCase #-}

module Main(main) where

import Text.Parsec

main :: IO ()
main = do
  file <- readFile "input.txt"
  
  let contents1 = parse contentP "input.txt" file

  case contents1 of
    Left e -> print e
    Right plays -> do

      print "Part1"
      (print. totalScore) plays
 

  let contents2 = parse contentP2 "input.txt" file

  case contents2 of
    Left e -> print e
    Right plays -> do

      print "Part1"
      (print. totalScore. map chooseShape) plays



  return ()

{-
   "Part1"
    14264

    "Part2"
    12382
-}

-- Data and Type declarations
data Shape = R | P | S
  deriving (Eq, Show)

instance Ord Shape where
  R <= P = True
  P <= S = True
  S <= R = True
  x <= y = x == y

type Match = (Shape, Shape)

data Outcome = L | D | W
type Match2 = (Shape, Outcome)

instance Enum Shape where
  toEnum 0 = R
  toEnum 1 = P
  toEnum 2 = S
  toEnum x = toEnum (x `mod` 3)
  fromEnum R = 0
  fromEnum P = 1
  fromEnum S = 2


-- Logic
win :: Shape -> Shape -> Int
win x y
  | x <  y = 6
  | x >  y = 0
  | x == y = 3

shapeScore :: Shape -> Int
shapeScore R = 1
shapeScore P = 2
shapeScore S = 3

totalScore :: [Match] -> Int
totalScore = sum . map (\(x,y) -> win x y + shapeScore y)

chooseShape :: Match2 -> Match
chooseShape (x, L) = (x, pred x) 
chooseShape (x, D) = (x, x)
chooseShape (x, W) = (x, succ x) 


-- Parsing methods
type Parser = Parsec String ()


numberP :: Parser Int
numberP = read <$> many1 digit <?> "digit"

shapeP :: Parser Shape
shapeP = (\case
          'A' -> R
          'X' -> R
          'B' -> P
          'Y' -> P
          'C' -> S
          'Z' -> S
         ) <$> letter

matchP :: Parser Match
matchP = (,)
      <$> shapeP <* space <*> shapeP

contentP :: Parser [Match]
contentP = matchP `sepEndBy1` newline


outcomeP :: Parser Outcome
outcomeP = (\case
          'X' -> L
          'Y' -> D
          'Z' -> W
         ) <$> letter

matchP2 :: Parser Match2
matchP2 = (,)
      <$> shapeP <* space <*> outcomeP

contentP2 :: Parser [Match2]
contentP2 = matchP2 `sepEndBy1` newline
