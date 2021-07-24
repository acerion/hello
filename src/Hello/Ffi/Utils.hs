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

                      , peekArrayOfPointers
                      , pokeArrayOfPointersWithAlloc
                      , pokeArrayOfPreallocedPointers
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
peekArrayOfPointers :: (Storable a) => Ptr (Ptr a) -> Int -> (Ptr a -> IO b) -> IO [b]
peekArrayOfPointers array n f = if nullPtr == array
                                then return []
                                else peekArrayOfPointers' array n f []
  where
    peekArrayOfPointers' :: (Storable a) => Ptr (Ptr a) -> Int -> (Ptr a -> IO b) -> [b] -> IO [b]
    peekArrayOfPointers' array 0 f acc = return acc -- The function iterates array from end, so we don't have to reverse the list.
    peekArrayOfPointers' array n f acc = do
      ptr :: Ptr a <- peekElemOff array (n - 1)
      x <- f ptr
      peekArrayOfPointers' array (n - 1) f (x : acc)




-- Set elements of an array. The elements are pointers to memory that will be
-- allocated by this function. More precisely: the memory is allocated by
-- constructor function that is a second arg to this function. The
-- constructor function allocates memory and sets its contents.
--
-- Save given list of Haskell objects [a] into C array of pointers to objects 'b'.
-- The pointers are allocated by this function.
-- The pointers are stored in an array given by third arg.
--
-- 'f' converts input items into pointers to allocated memory. The pointers
-- will be read and interpreted by C code. This can be a function that e.g.
-- converts Data.Text into char* strings.
pokeArrayOfPointersWithAlloc :: (Storable b) => [a] -> (a -> IO (Ptr b)) -> Ptr (Ptr b) -> IO ()
pokeArrayOfPointersWithAlloc xs ctor array = pokeArrayOfPointers' xs ctor array 0
  where
    pokeArrayOfPointers' :: (Storable b) => [a] -> (a -> IO (Ptr b)) -> Ptr (Ptr b) -> Int -> IO ()
    pokeArrayOfPointers' [] ctor array _ = do
      return ()
    pokeArrayOfPointers' (x:xs) ctor array idx = do
      ptr :: Ptr b <- ctor x
      pokeElemOff array idx ptr
      pokeArrayOfPointers' xs ctor array (idx + 1)




-- Set elements of an array. The elements are pointers to already allocated
-- memory, so a function that sets each element doesn't allocate memory for
-- the element (the memory is preallocated by owner/creator of the array).
pokeArrayOfPreallocedPointers :: (Storable b) => [a] -> (a -> Ptr b -> IO ()) -> Ptr (Ptr b) -> IO ()
pokeArrayOfPreallocedPointers xs setter array = pokeArrayOfPointers' xs setter array 0
  where
    pokeArrayOfPointers' :: (Storable b) => [a] -> (a -> Ptr b -> IO ()) -> Ptr (Ptr b) -> Int -> IO ()
    pokeArrayOfPointers' [] setter array _ = do
      return ()
    pokeArrayOfPointers' (x:xs) setter array idx = do
      ptr :: Ptr b <- peekElemOff array idx
      setter x ptr
      pokeElemOff array idx ptr
      pokeArrayOfPointers' xs setter array (idx + 1)




