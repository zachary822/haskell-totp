module Lib.Utils where

import Codec.Utils (Octet)
import Data.Binary.Get (Get, getWord8, isEmpty)

getOctets :: Get [Octet]
getOctets = do
  empty <- isEmpty
  if empty
    then return []
    else do
      octet <- getWord8
      octets <- getOctets
      return (octet : octets)
