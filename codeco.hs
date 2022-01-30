-- |codeco.hs
--
-- Compressor-decompressor main proper.
--
-- Copyright (C) 2022 Serguey Zefirov.

module Main where

import Control.Monad

import Control.Monad.State

import Data.Bits

import qualified Data.ByteString as BS

import qualified Data.List as List

import qualified Data.Set as Set

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM

import Data.Word

import System.Environment
import System.Exit
import System.IO

import Debug.Trace

import Hash
import ZDD

prefixLength, blockLength :: Int
prefixLength = 5000
blockLength = 20000

firstPrefix :: BS.ByteString
firstPrefix = BS.replicate prefixLength 0

usage :: IO a
usage = do
	putStrLn $ "usage: codeco c|d input-file-name output-file-name"
	exitFailure

runCompress :: String -> String -> IO ()
runCompress inputFN outputFN = do
	text <- BS.readFile inputFN
	when (mod (BS.length text) blockLength /= 0) $ error $ "input file size must be divisible by block length which is " ++ show blockLength
	error "compress"

decompressBlocks :: Handle -> BS.ByteString -> String -> [BS.ByteString] -> ZDDM ()
decompressBlocks h _ "" [] = return ()
decompressBlocks h _ ('0':flags) (block:blocks)
	| BS.length block == blockLength = do
		liftIO $ BS.hPut h block
		decompressBlocks h (BS.drop (blockLength - prefixLength) block) flags blocks
decompressBlocks h prefix ('1':flags) blocks = do
	error "actual decompression"
decompressBlocks _ _ _ _ = error "something gone wrong"

runDecompress :: String -> String -> IO ()
runDecompress inputFN outputFN = do
	text <- readFile inputFN
	error "decompress"
	case reads text of
		[((zddNodes, flags, skippedBlocks), "")] -> do
			h <- openBinaryFile outputFN WriteMode
			runWithNodes zddNodes $ decompressBlocks h firstPrefix flags skippedBlocks
			hClose h

runCommand :: [String] -> IO ()
runCommand ["c", input, output] = runCompress input output
runCommand ["d", input, output] = runDecompress input output

main = do
	hSetBuffering stdout NoBuffering
	args <- getArgs
	runCommand args

t = main

