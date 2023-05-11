{-# LANGUAGE OverloadedStrings #-}

module Lib.Hotp where

import Codec.Utils (Octet)
import Data.Binary.Get (getWord32be, runGet)
import Data.Bits (Bits ((.&.)))
import Data.ByteString.Lazy qualified as BL
import Data.HMAC (hmac_sha1)
import Text.Printf (printf)

-- | calculate HOTP code from seed and moving factor
hotp :: [Octet] -> [Octet] -> Int -> String
hotp key counter digits = printf ("%0" ++ show digits ++ "u") codeStr
  where
    hash = hmac_sha1 key counter
    offset = fromIntegral $ last hash .&. 0x0f :: Int
    codeInt = fromIntegral $ runGet getWord32be (BL.pack $ take 4 . drop offset $ hash) .&. 0x7fffffff :: Int
    codeStr = codeInt `mod` (10 ^ digits)
