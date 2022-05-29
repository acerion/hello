{-
Copyright (C) 2021-2022 Kamil Ignacak acerion@wp.pl

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




module Hello.Css.ContextGlobal
  (
    globalContextCtor
  , globalContextUpdate
  , globalContextPut
  , globalContextGet
  )
where




import Prelude

import Debug.Trace

import Data.IORef
import System.IO.Unsafe

import Hello.Css.StyleSheet
import Hello.Utils




myGlobalContexts :: IORef [CssContext]
{-# NOINLINE myGlobalContexts #-}
myGlobalContexts = unsafePerformIO (newIORef [])




globalContextCtor :: IO Int
globalContextCtor = do
  oldList <- readIORef myGlobalContexts
  let newList = oldList ++ [ defaultCssContext ]
  writeIORef myGlobalContexts newList

  return ((length newList) - 1)




globalContextUpdate :: Int -> CssContext -> IO ()
globalContextUpdate ref context = do
  oldList <- readIORef myGlobalContexts
  --let item = oldList !! ref
  let newList = listReplaceElem oldList context ref

  writeIORef myGlobalContexts newList




globalContextPut :: CssContext -> IO Int
globalContextPut context = do
  oldList <- readIORef myGlobalContexts
  let newList = oldList ++ [ context ]
  writeIORef myGlobalContexts newList

  return ((length newList) - 1)




globalContextGet :: Int -> IO CssContext
globalContextGet ref = do
  list <- readIORef myGlobalContexts
  return (list !! ref)




