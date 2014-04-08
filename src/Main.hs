{-# LANGUAGE DeriveDataTypeable #-}
-- | Main entry point to the application.
module Main where

--import           Control.Applicative
import           Control.Monad
import           Data.Either
import           Data.Functor
import           Data.Maybe             (fromMaybe, isJust)
import           Data.Set               (Set)
import qualified Data.Set               as S
import           System.Console.CmdArgs
import           System.IO

import           Data.Lts
import           Data.LtsPretty

-- CmdArgs

data Files = Files {input  :: Maybe FilePath
                   ,output :: Maybe FilePath}
             deriving (Show, Data, Typeable, Eq)

files = Files {input = def
              ,output = def}

-- Build LTS

-- | The main entry point.
main :: IO ()
main = do
  args <- cmdArgs files
  print args
  let i = input args
  let o = output args
  inh <- maybe (return stdin) (`openFile` ReadMode) i
  c <- hGetContents inh
  let l = either undefined convert $ parseLts (fromMaybe "(stdin)" i) c
  outh <- maybe (return stdout) (`openFile` WriteMode) o
  hPutStrLn outh $ renderLts l
  hPutStrLn outh . renderLts $ minimiseLts l
  when (isJust i) $ hClose inh
  when (isJust o) $ hClose outh

--

test = do
  f <- readFile "test.lts"
  let g = convert <$> parseLts "" f
  let l = either undefined id g
  --print l
  print $ minStep l (S.singleton $ processes l)
  print "bye"
  return l
