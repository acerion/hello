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




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}




module Hello.Ffi.Cookies
  (
  )
where




import Prelude
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Foreign.C.String
--import qualified Data.Text.Encoding as T.E
--import qualified Data.ByteString.Unsafe as BSU
import Hello.Cookies




foreign export ccall "hll_lookupActionForDomain" hll_lookupActionForDomain :: CString -> IO Int




hll_lookupActionForDomain :: CString -> IO Int
hll_lookupActionForDomain dom = do
  domainString <- T.pack <$> peekCString dom
  cookiesConfig <- getCookiesConfig
  -- putStr ("hello: cookies: " ++ (show cookiesConfig) ++ "\n") -- For debug only.
  let actionForDomain = lookupActionForDomain domainString (rules cookiesConfig) (defaultAction cookiesConfig)
  T.IO.putStr (T.concat ["hello: cookies: ", domainString, " = ", T.pack (show actionForDomain), "\n"])
  case actionForDomain of
    -- Convert to values of CookieAction enum in C file.
    CookieActionAccept        -> return 0
    CookieActionAcceptSession -> return 1
    CookieActionDeny          -> return 2



