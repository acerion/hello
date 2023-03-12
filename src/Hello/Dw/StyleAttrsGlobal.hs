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




{-# LANGUAGE ScopedTypeVariables #-}




{-
Store global variables with StyleAttrs objects.
-}




module Hello.Dw.StyleAttrsGlobal
  (
    globalStyleAttrsCtor
  , globalStyleAttrsCopyCtor
  , globalStyleAttrsUpdate
  , globalStyleAttrsPut
  , globalStyleAttrsGet
  )
where




import Prelude

--import Debug.Trace

import Data.IORef
import System.IO.Unsafe

import Hello.Dw.Style
import Hello.GlobalContainer




myGlobalStyleAttrs :: IORef [StyleAttrs]
{-# NOINLINE myGlobalStyleAttrs #-}
myGlobalStyleAttrs = unsafePerformIO (newIORef [])




-- Add new default element. Return reference to it.
-- Second argument is a constructor of default element.
globalStyleAttrsCtor :: IO Int
globalStyleAttrsCtor = globalContainerCtor myGlobalStyleAttrs defaultStyleAttrs




-- A copy constructor.
-- Construct a new item from given item.
globalStyleAttrsCopyCtor :: Int -> IO Int
globalStyleAttrsCopyCtor = globalContainerCopyCtor myGlobalStyleAttrs




-- Update existing entry indicated by a reference with given element.
globalStyleAttrsUpdate :: Int -> StyleAttrs -> IO ()
globalStyleAttrsUpdate = globalContainerUpdate myGlobalStyleAttrs




-- Add given element. Return reference to it.
globalStyleAttrsPut :: StyleAttrs -> IO Int
globalStyleAttrsPut = globalContainerPut myGlobalStyleAttrs




-- Get an element indicated by given reference.
globalStyleAttrsGet :: Int -> IO StyleAttrs
globalStyleAttrsGet = globalContainerGet myGlobalStyleAttrs





