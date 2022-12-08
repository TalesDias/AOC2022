#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-20.3   
  --package "parsec"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

module Main(main) where

import Text.Parsec
import Data.List

main :: IO ()
main = do
  let filename = "input.txt"
  file <- readFile filename

  let contents = parse contentP filename file

  case contents of
    Left e -> print e
    Right grid -> do

      print "Part1"
      print. qtdVisibleTrees $ grid

      print "Part2"
      print. scenicScore $ grid

  return ()

{-
    Answers
      Part1: 1182909
      Part2: 2832508
-}

-- Data and Type declarations
type Grid = [Row]
type Row = [Int]
type Coord = (Int, Int)


-- Logic

elemWise :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
elemWise op as = zipWith (\a b -> a b) ((map $ zipWith op) as)

invisibleInLine :: Row -> Row
invisibleInLine = vil (-1)
  where
    vil _ []      = []
    vil m (x:xs)
      | x > m     = 0:vil x xs
      | otherwise = 1:vil m xs

invisibleTrees :: Grid -> Grid
invisibleTrees xs = elemWise (*) visiblerows visiblecols
  where
    left2right  = map invisibleInLine xs
    right2left  = map (reverse. invisibleInLine. reverse) xs
    visiblerows = elemWise (*) left2right right2left

    top2bottom  = transpose $ map invisibleInLine $ transpose xs
    bottom2top  = transpose $ map (reverse. invisibleInLine. reverse) $ transpose xs
    visiblecols = elemWise (*) top2bottom bottom2top

qtdVisibleTrees :: Grid -> Int
qtdVisibleTrees xs
  = sz - sum (map sum $ invisibleTrees xs)
    where
      sz = (^2). length. head $ xs


lookUp :: Grid -> Coord -> Int 
lookUp gs c@(ox, oy) = go 0 c
  where
    val = gs !!ox !! oy
    go :: Int -> Coord -> Int
    go l (x, y) 
      | x == 0    = l
      | up < val  = go (l+1) (x-1, y)
      | otherwise = l+1
      where
        up  = gs !!(x-1) !!y

lookRight :: Grid -> Coord -> Int 
lookRight gs c@(ox, oy) = go 0 c
  where
    val = gs !!ox !! oy
    len = length. head $ gs 
    go :: Int -> Coord -> Int
    go l (x, y) 
      | y == len-1 = l
      | ri < val   = go (l+1) (x, y+1)
      | otherwise  = l+1
      where
        ri  = gs !!x !!(y+1)


lookDown :: Grid -> Coord -> Int 
lookDown gs c@(ox, oy) = go 0 c
  where
    val = gs !!ox !! oy
    len = length. head $ gs 
    go :: Int -> Coord -> Int
    go l (x, y) 
      | x == len-1 = l
      | ri < val   = go (l+1) (x+1, y)
      | otherwise  = l+1
      where
        ri  = gs !!(x+1) !!y


lookLeft :: Grid -> Coord -> Int 
lookLeft gs c@(ox, oy) = go 0 c
  where
    val = gs !!ox !! oy
    go :: Int -> Coord -> Int
    go l (x, y) 
      | y == 0    = l
      | ri < val  = go (l+1) (x, y-1)
      | otherwise = l+1
      where
        ri  = gs !!x !!(y-1)


coords :: Grid -> [Coord]
coords gs = [ (x,y) 
          | x <- [0..len]
          , y <- [0..len]]
            where 
              len = subtract 1. length. head $ gs


scenicScore :: Grid -> Int
scenicScore gs = maximum scores 
  where
    cs  = coords gs
    
    ups = map (lookUp gs) cs
    dws = map (lookDown gs) cs
    ver = zipWith (*) ups dws

    ris = map (lookRight gs) cs
    lfs = map (lookLeft gs) cs
    hor = zipWith (*) ris lfs

    scores = zipWith (*) hor ver


-- Parsing methods
type Parser = Parsec String ()

numberP :: Parser Int
numberP = read <$> many1 digit <?> "number"

digitP :: Parser Int
digitP = read. (:[]) <$> digit

rowP :: Parser Row
rowP = many1 digitP

contentP :: Parser Grid
contentP = rowP `sepEndBy1` newline

