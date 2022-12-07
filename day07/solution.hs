#!/usr/bin/env stack
{- stack 
  script
  --resolver lts-20.3   
  --package "parsec transformers"
  --optimize
  --ghc-options=-no-keep-hi-files 
  --ghc-options=-no-keep-o-files 
-}

module Main(main) where

import Text.Parsec
import Data.Either
import Control.Monad
import Data.List
import Control.Monad.Trans.Writer.Lazy
import Data.Monoid

main :: IO ()
main = do
  let filename = "input.txt"
  file <- readFile filename

  let contents = runParser contentP [] filename file

  case contents of
    Left e -> print e
    Right root -> do

      print "Part1"
      (print. sumSmallSizes) root

      print "Part2"
      (print.sizeToDelete) root

  return ()

{-
    Answers
      Part1: 1182909
      Part2: 2832508
-}

-- Data and Type declarations
type Size = Int

data File = F
  { fsz :: Size
  , fnm :: String
  }
  deriving Show

newtype DirName = DN String
  deriving Show

data Dir = D [String] [File] [Dir]
  deriving Show


-- Logic
sumSmallSizes :: Dir -> Size
sumSmallSizes = sum
              . filter (<100_000)
              . execWriter
              . collectSizes

filesSize :: [File] -> Int
filesSize = foldl (flip ((+). fsz)) 0

collectSizes :: Dir -> Writer [Size] Size
collectSizes (D _ fs ds) = do
  let fsSize = filesSize fs
  dsSize <- sum <$> mapM collectSizes ds
  let totSz = dsSize + fsSize
  
  tell [totSz]
 
  return totSz

sizeToDelete :: Dir -> Size 
sizeToDelete root = minimum (filter (szNeeded <) szs)
  where
    (totSz, szs) = runWriter $ collectSizes root
    szNeeded     = totSz - (70_000_000 - 30_000_000)



-- Parsing methods
type Parser = Parsec String [String]

numberP :: Parser Int
numberP = read <$> many1 digit <?> "digit"


fileP :: Parser File
fileP = F
  <$> numberP
  <*  space
  <*> many1 (lower <|> char '.' )
  <* newline


dirNameP :: Parser DirName
dirNameP = DN
  <$ string "dir "
  <*> many1 lower
  <* newline


parseOutput :: Parser (Either File DirName)
parseOutput =   Left <$> fileP
            <|> Right <$> dirNameP


lsP :: Parser ([File], [DirName])
lsP =   string "$ ls" <* newline
    >>  partitionEithers
    <$> manyTill parseOutput (lookAhead $ char '$')


dirP :: DirName -> Parser Dir
dirP (DN n) = do
  (fsi, dsi) <- commandP
  prefix <- getState
  return $ D (n:prefix) fsi dsi


commandP :: Parser ([File], [Dir])
commandP = do
  string "$ cd "
  base <- many1 lower <|> singleton <$> char '/'
  newline

  modifyState (base:)

  (fs, dns) <- lsP
  ds <- forM dns dirP

  choice  [ try $ lookAhead (string "$ endflag") -- You must mannualy add this line
          , string "$ cd .." <* newline
          ]
  modifyState tail

  return (fs, ds)


contentP :: Parser Dir
contentP = uncurry (D ["/"]) <$> commandP
