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

-- | calculate HOTP code from seed and moving factor
hotp :: ByteString -> ByteString -> Int -> Either String String
hotp key counter digits
  | digits < 6 = Left "too few digits"
  | otherwise = Right $ show $ codeInt `mod` (10 ^ digits)
 where
  digest = hmacGetDigest $ hmac key counter :: Digest SHA1
  hash = B.unpack $ convert digest
  offset = fromIntegral $ last hash .&. 0x0f
  codeInt = fromIntegral $ runGet getWord32be (BL.pack $ take 4 . drop offset $ hash) .&. 0x7fffffff :: Int
