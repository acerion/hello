{-# LANGUAGE ForeignFunctionInterface #-}

-- Exporting function names for tests in ghci.
module URL(hostIsIP
          , hostIsIPv4
          , hostIsIPv6
          ) where

import Prelude
import Foreign.C.String
import Foreign

import Data.Text
import qualified Data.Text as T


foreign export ccall "hll_hostIsIP" hll_hostIsIP :: CString -> IO Bool


hll_hostIsIP :: CString -> IO Bool
hll_hostIsIP host = do
  str <- peekCString host
  return (hostIsIP . T.pack $ str)


hostIsIP :: Text -> Bool
hostIsIP host = hostIsIPv4 host || hostIsIPv6 host


-- These two are just naive implementations that mimic original Dillo
-- function. They will accept a malformed IP that consists of the right
-- chars. TODO: provide better implementations.

hostIsIPv4 :: Text -> Bool
hostIsIPv4 host = T.all (\c -> elem c "0123456789.") host

hostIsIPv6 :: Text -> Bool
hostIsIPv6 host = T.all (\c -> elem c "0123456789abcdefABCDEF:.") host
-- The precise format is shown in section 3.2.2 of rfc 3986
