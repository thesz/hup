module Main where

import Control.Monad (forM_)

import Control.Monad.State

import Data.Bits

import qualified Data.ByteString as BS

import qualified Data.List as List

import qualified Data.Set as Set

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM

import Data.Word

import System.Environment
import System.IO

import Debug.Trace

import Hash
import ZDD


main = do
	hSetBuffering stdout NoBuffering
	text' <- fmap (BS.take 1000000000) $ BS.readFile "enwik9"
	let	len = BS.length text'
		text = BS.concat [BS.replicate contextLength 0x20, text']
	putStrLn $ "read " ++ show len ++ " bytes."
	putStrLn $ "duda"

t = main

-- немного русского.
