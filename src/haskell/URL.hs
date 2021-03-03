{-
Copyright (C) 2021 Kamil Ignacak acerion@wp.pl

This file is part of "hello" web browser.

"hello" is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

"hello" is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with "hello".  If not, see <https://www.gnu.org/licenses/>.

This file is derived from dillo-3.0.5/src/url.c.
Copyright assignments from that file:
Copyright (C) 2001-2009 Jorge Arellano Cid <jcid@dillo.org>
-}

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
