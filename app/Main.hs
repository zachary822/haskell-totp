{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Binary.Builder (putInt64be)
import Data.ByteArray.Encoding (Base (Base32), convertFromBase)
import Data.ByteString.Builder (string7, toLazyByteString)
import Data.ByteString.Lazy qualified as L
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lib.Hotp (hotp)
import Options.Applicative

data TotpArgs = TotpArgs
  { getKey :: String
  -- ^ hotp seed
  , getStep :: Integer
  -- ^ totp time step
  , getDigits :: Int
  -- ^ digits of hotp output
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

  case (convertFromBase Base32 . L.toStrict . toLazyByteString . string7 . getKey) args of
    Left a -> error a
    Right key -> do
      timestamp <- round <$> getPOSIXTime
      let counter = L.toStrict . toLazyByteString . putInt64be . fromIntegral . div timestamp $ getStep args
      putStr $ hotp key counter (getDigits args)
