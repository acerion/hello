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
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Ffi.Utils( ptrCCharToText
                      , textToPtrCChar

                      , cArrayLenToList
                      )
  where




import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU




ptrCCharToText :: Ptr CChar -> IO T.Text
ptrCCharToText ptr = do
  if nullPtr == ptr
    then return ""
    else do
    bs :: BS.ByteString <- BSU.unsafePackCString ptr
    return $ T.E.decodeLatin1 bs




-- Get pointer to newly allocated C string representing given text.
--
-- This function allocates memory, but since the goal of this project is to
-- replace C/C++ code with Haskell code, the allocation will be eventually
-- removed. So I don't care about deallocating the memory.
textToPtrCChar :: T.Text -> IO CString
textToPtrCChar = newCString . T.unpack





-- Convert array of pointers to C objects to list of Haskell objects.
--
-- Example: "char * array[n]" -> n -> acc -> conversionFun -> [T.Text].
cArrayLenToList :: (Storable a) => Ptr (Ptr a) -> Int -> (Ptr a -> IO b) -> IO [b]
cArrayLenToList array n f = if nullPtr == array
                            then return []
                            else cArrayLenToList' array n f []
  where
    cArrayLenToList' :: (Storable a) => Ptr (Ptr a) -> Int -> (Ptr a -> IO b) -> [b] -> IO [b]
    cArrayLenToList' array 0 f acc = return acc -- The function iterates array from end, so we don't have to reverse the list.
    cArrayLenToList' array n f acc = do
      ptr :: Ptr a <- peekElemOff array (n - 1)
      x <- f ptr
      cArrayLenToList' array (n - 1) f (x : acc)


