{-
Copyright (C) 2021-2023 Kamil Ignacak acerion@wp.pl

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




module Hello.Ffi.Utils
  (
    ptrCCharToText
  , allocAndPokeCString
  , allocAndPokeCStringIfNonEmpty

  , peekArrayOfPointers
  , pokeArrayOfPointersWithAlloc
  , pokeArrayOfPreallocedPointers

  , peekCharBuffer
  , pokeCharBuffer

  , cDoubleToDouble
  )
where




import Foreign
--import Foreign.Ptr
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
allocAndPokeCString :: T.Text -> IO CString
allocAndPokeCString = newCString . T.unpack





-- If given text is non-empty, get pointer to newly allocated C string
-- representing given text. Otherwise return NULL.
--
-- This function allocates memory, but since the goal of this project is to
-- replace C/C++ code with Haskell code, the allocation will be eventually
-- removed. So I don't care about deallocating the memory.
allocAndPokeCStringIfNonEmpty :: T.Text -> IO CString
allocAndPokeCStringIfNonEmpty text = if T.null text
                                     then return nullPtr
                                     else newCString . T.unpack $ text




-- Convert array of pointers to C objects to list of Haskell objects.
--
-- Example: "char * array[n]" -> n -> acc -> conversionFun -> [T.Text].
peekArrayOfPointers :: (Storable a) => Ptr (Ptr a) -> Int -> (Ptr a -> IO b) -> IO [b]
peekArrayOfPointers array nElems fun = if nullPtr == array
                                       then return []
                                       else peekArrayOfPointers' array nElems fun []
  where
    peekArrayOfPointers' :: (Storable a) => Ptr (Ptr a) -> Int -> (Ptr a -> IO b) -> [b] -> IO [b]
    peekArrayOfPointers' _      0 _ acc = return acc -- The function iterates array from end, so we don't have to reverse the list.
    peekArrayOfPointers' array' n f acc = do
      ptr :: Ptr a <- peekElemOff array' (n - 1)
      x <- f ptr
      peekArrayOfPointers' array' (n - 1) f (x : acc)




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
pokeArrayOfPointersWithAlloc items constructor array = pokeArrayOfPointers' items constructor array 0
  where
    pokeArrayOfPointers' :: (Storable b) => [a] -> (a -> IO (Ptr b)) -> Ptr (Ptr b) -> Int -> IO ()
    pokeArrayOfPointers' [] _ _ _ = do
      return ()
    pokeArrayOfPointers' (x:xs) ctor array' idx = do
      ptr :: Ptr b <- ctor x
      pokeElemOff array' idx ptr
      pokeArrayOfPointers' xs ctor array' (idx + 1)




-- Set elements of an array. The elements are pointers to already allocated
-- memory, so a function that sets each element doesn't allocate memory for
-- the element (the memory is preallocated by owner/creator of the array).
--
-- Order of arguments to setter function is the same as in
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Foreign-Storable.html#v:poke
pokeArrayOfPreallocedPointers :: (Storable b) => [a] -> (Ptr b -> a -> IO ()) -> Ptr (Ptr b) -> IO ()
pokeArrayOfPreallocedPointers items setter' array' = pokeArrayOfPointers' items setter' array' 0
  where
    pokeArrayOfPointers' :: (Storable b) => [a] -> (Ptr b -> a -> IO ()) -> Ptr (Ptr b) -> Int -> IO ()
    pokeArrayOfPointers' [] _ _ _ = do
      return ()
    pokeArrayOfPointers' (x:xs) setter array idx = do
      ptr :: Ptr b <- peekElemOff array idx
      setter ptr x
      pokeElemOff array idx ptr
      pokeArrayOfPointers' xs setter array (idx + 1)





cDoubleToDouble :: CDouble -> Double
cDoubleToDouble = realToFrac




peekCharBuffer :: Ptr CChar -> IO T.Text
peekCharBuffer = ptrCCharToText




{-
-- Not tested in real code.
pokeCharBuffer :: Ptr CChar -> T.Text -> IO ()
pokeCharBuffer ptr text = pokeArray0 0 ptr cstring
  where
    cstring = fmap castCharToCChar (T.unpack text)
-}




-- Write given ASCII text to pre-allocated char array of given size.
--
-- Function can be used to write to e.g. to something like this:
--
-- char c_member_buf[10].
--
-- The size of the target array (including space for terminating NUL) is
-- passed as third argument. If the 'c_member_buf' buffer is a member of
-- struct, then the size of the buffer can be calculated in 'peek' function
-- with this expression:
--
-- let ffiBuf :: Ptr CChar = ...
-- pokeCharBuffer ffiBuf #{size ((struct_type_t *)0)->c_member_buf} (text :: T.Text)
--
-- The function does not allocate memory for output buffer.
--
-- The function always writes terminating NUL.
--
-- If if 'text' argument is larger than space available in target buffer, the
-- text in target buffer is truncated.
--
-- Loss of data may occur if input text is non-ASCII.
--
-- Tested in real code.
--
-- I know that it's too complicated and that I should rather use the other
-- implementation from above.
pokeCharBuffer :: Ptr CChar -> Int -> T.Text -> IO ()
pokeCharBuffer pointer nItems inText = if pointer == nullPtr
                                     then return ()
                                     else pokeCharBuffer' pointer nItems inText
  where
    pokeCharBuffer'       _ 0    _ = return ()
    pokeCharBuffer'     ptr 1    _ = poke ptr 0
    pokeCharBuffer'     ptr n text =
      do
        case T.uncons text of
          Just (c, remainter) ->
            do
              poke ptr (castCharToCChar c)
              pokeCharBuffer (plusPtr ptr 1) (n - 1) remainter
          Nothing -> poke ptr 0

