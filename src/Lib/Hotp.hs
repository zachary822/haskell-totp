{-# LANGUAGE OverloadedStrings #-}

module Lib.Hotp where

import Crypto.Hash (Digest, SHA1)
import Crypto.MAC.HMAC (HMAC (hmacGetDigest), hmac)
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
  hash = convert (hmacGetDigest $ hmac key counter :: Digest SHA1)
  offset = fromIntegral $ B.last hash .&. 0x0f
  codeInt =
    runGet getWord32be (BL.fromStrict . B.take 4 . B.drop offset $ hash)
      .&. 0x7fffffff
