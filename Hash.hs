{-# LANGUAGE BangPatterns, ForeignFunctionInterface, CApiFFI #-}

module Hash where

import Data.Bits

import qualified Data.ByteString as BS

import Data.Word

import Foreign
import Foreign.C.Types

import System.IO.Unsafe

foreign import ccall unsafe "siphash64"
	siphash64 :: Ptr Word8 -> CSize -> IO Word64

hash :: BS.ByteString -> Word64
hash b = unsafePerformIO $ BS.useAsCStringLen b $ \(p, l) -> siphash64 (castPtr p) (fromIntegral l)

