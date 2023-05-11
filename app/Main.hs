{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Binary.Builder (putInt64be)
import Data.Binary.Get (runGet)
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.ByteString.Lazy.Base32 as Base32 (decodeBase32)
import Data.Either (fromRight)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lib.Hotp (hotp)
import Lib.Utils (getOctets)
import Options.Applicative

data TotpArgs = TotpArgs
  { getKey :: String,
    getStep :: Integer,
    getDigits :: Integer
  }
  deriving (Show)

totpArgsParser :: Parser TotpArgs
totpArgsParser =
  TotpArgs
    <$> argument
      str
      ( help "Base32 totp secret key"
          <> metavar "KEY"
      )
    <*> option
      auto
      ( help "Time step"
          <> long "step"
          <> short 's'
          <> showDefault
          <> value 30
          <> metavar "INT"
      )
    <*> option
      auto
      ( help "Digits"
          <> long "digits"
          <> short 'd'
          <> showDefault
          <> value 6
          <> metavar "INT"
      )

main :: IO ()
main = do
  let opts = info (totpArgsParser <**> helper) fullDesc

  args <- execParser opts

  let key = runGet getOctets (fromRight "" (Base32.decodeBase32 $ (toLazyByteString . stringUtf8 . getKey) args))
  timestamp <- (round :: NominalDiffTime -> Integer) <$> getPOSIXTime
  let counter = runGet getOctets $ (toLazyByteString . putInt64be) (fromIntegral $ timestamp `div` getStep args)

  putStr $ hotp key counter (getDigits args)
