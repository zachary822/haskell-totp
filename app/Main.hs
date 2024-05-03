{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteArray.Encoding (Base (Base32), convertFromBase)
import Data.ByteString.Builder (int64BE, toLazyByteString)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BL
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

  TotpArgs{..} <- execParser opts

  res <- runExceptT $ do
    key <-
      except $
        (convertFromBase Base32 . C8.pack) getKey
    timestamp <- round <$> lift getPOSIXTime
    let counter =
          BL.toStrict . toLazyByteString . int64BE . fromIntegral . div timestamp $
            getStep
    except $ hotp key counter getDigits

  case res of
    Left e -> fail e
    Right code -> putStr code
