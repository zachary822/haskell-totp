{-# LANGUAGE OverloadedStrings #-}

module Lib.Hotp where

import Crypto.Hash (Digest, SHA1)
import Crypto.MAC.HMAC (HMAC (hmacGetDigest), hmac, hmacGetDigest)
import Data.Binary.Get (getWord32be, runGet)
import Data.Bits (Bits ((.&.)))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Text.Printf (printf)

-- | calculate HOTP code from seed and moving factor
hotp :: ByteString -> ByteString -> Int -> String
hotp key counter digits = printf ("%0" ++ show digits ++ "u") codeStr
  where
    digest = hmacGetDigest $ hmac key counter :: Digest SHA1
    hash = B.unpack $ convert digest
    offset = fromIntegral $ last hash .&. 0x0f :: Int
    codeInt = fromIntegral $ runGet getWord32be (BL.pack $ take 4 . drop offset $ hash) .&. 0x7fffffff :: Int
    codeStr = codeInt `mod` (10 ^ digits)
