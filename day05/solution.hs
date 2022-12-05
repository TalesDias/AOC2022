#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-20.3   
  --package "parsec containers transformers"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

module Main(main) where

import Text.Parsec hiding (State)
import System.IO
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map.Lazy as M


main :: IO ()
main = do

  -- The input format of the crate stacks is very 
  -- annoying for parsec to deal with, just making
  -- it easier for the parser with a transpose + reverse
  handle <- openFile "input.txt" ReadMode
  stacks <- replicateM 9 . hGetLine $ handle
  rest   <- hGetContents handle

  let stacks' = (intercalate "\n"
                . filter (not. null)
                . map (filter (`notElem` "[] ")
                      . reverse)
                . transpose
                ) stacks

  handle2 <- openFile "inputTrans.txt" WriteMode
  hPutStr handle2 stacks'
  hPutStr handle2 "\n"
  hPutStr handle2 rest
  hClose handle2


  -- now it's business as usual
  let filename = "inputTrans.txt"
  file <- readFile filename

  let contents = parse contentP filename file

  case contents of
    Left e -> print e
    Right (stacks, moves) -> do


      print "Part1"
      (print. topCrates stacks False) moves

      print "Part2"
      (print. topCrates stacks True) moves



  return ()

{-
    Answers
      Part1: RFFFWBPNS
      Part2: CQQBBJFCS
-}

-- Data and Type declarations
type Stacks = Map Int [Char]
data Move = M Int Int Int

-- Logic
makeMove :: Bool -> Move -> State Stacks ()
makeMove newVersion (M n f t) = do
  ss <- get

  let toMove = take n $ ss!f
  modify (M.adjust (drop n) f)
  
  let moved = if newVersion then toMove
                            else reverse toMove
  modify (M.adjust (moved ++) t)

topCrates :: Stacks -> Bool -> [Move] -> [Char]
topCrates ss newV
  = map head
  . M.elems
  . runMoves
  where
    runMoves ms
      = execState (mapM_ (makeMove newV) ms) ss


-- Parsing methods
type Parser = Parsec String ()


numberP :: Parser Int
numberP = read <$> many1 digit <?> "digit"

stackP :: Parser (Int, [Char])
stackP = (,) <$> numberP <*> (reverse <$> many1 upper)

stacksP :: Parser Stacks
stacksP = M.fromList <$> stackP `sepEndBy` newline

moveP :: Parser Move
moveP = M
  <$  string "move "
  <*> numberP
  <*  string " from "
  <*> numberP
  <*  string " to "
  <*> numberP


contentP :: Parser (Stacks, [Move])
contentP = (,)  <$> stacksP
                <*  newline
                <*> moveP `sepEndBy1` newline


