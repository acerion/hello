{-
Copyright (C) 2022 Kamil Ignacak acerion@wp.pl

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
-}




{-# LANGUAGE ForeignFunctionInterface #-}




module Hello.Ffi.Html.Entity
  (
  )
where




--import Prelude
import Foreign
import Foreign.C

import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.ByteString.Unsafe as BSU

import Hello.Html.Entity




foreign export ccall "hll_htmlEntityToIsoCode" hll_htmlEntityToIsoCode :: CString -> Int -> IO Int64




-- Caller has a big buffer with remainder of html document (which can be
-- huge), and also has a length of sub-area/prefix in this big buffer (the
-- sub-area is at the beginning of the buffer), from which a html entity
-- should be parsed.
--
-- Pass the pointer to the big buffer, and a size of this prefix to this
-- function.
--
-- This function encodes iso code value, length of entity and (in future)
-- error code in one single returned integer. In original code the length and
-- error code were returned by function arugments (pointers). I don't want to
-- put too much work in doing this in similar way in FFI code, because sooner
-- or later the FFI code will be removed (replaced with "pure" Haskell code).
hll_htmlEntityToIsoCode :: CString -> Int -> IO Int64
hll_htmlEntityToIsoCode cBuf len = do
  buf <- if len > 0
         then BSU.unsafePackCStringLen (cBuf, len)
         else BSU.unsafePackCString cBuf
  case htmlEntityToIsoCode . T.E.decodeUtf8 $ buf of
    Just parser ->
      case entityIsoCode parser of
        Just code -> return (fromIntegral ((consumed `shiftL` 32) .|. code))
          where consumed = (T.length . T.E.decodeUtf8 $ buf) - T.length (remainder parser)
        Nothing -> return (-1)
    Nothing     -> return (-1)




