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




{-
Store global variables with Css Contexts.

This global state is needed until Css Style Engine is moved into Haskell.
Then Css Context variables will be members of the Style Engine variables (one
context per one engine).
-}




module Hello.Css.ContextGlobal
  (
    globalContextCtor
  , globalContextUpdate
  , globalContextPut
  , globalContextGet
  )
where




import Prelude

--import Debug.Trace

import Data.IORef
import System.IO.Unsafe

import Hello.Css.StyleSheet
import Hello.GlobalContainer




myGlobalContexts :: IORef [CssContext]
{-# NOINLINE myGlobalContexts #-}
myGlobalContexts = unsafePerformIO (newIORef [])




-- Add new default element. Return reference to it.
-- Second argument is a constructor of default element.
globalContextCtor :: IO Int
globalContextCtor = globalContainerCtor myGlobalContexts defaultCssContext




-- Update existing entry indicated by a reference with given element.
globalContextUpdate :: Int -> CssContext -> IO ()
globalContextUpdate = globalContainerUpdate myGlobalContexts




-- Add given element. Return reference to it.
globalContextPut :: CssContext -> IO Int
globalContextPut = globalContainerPut myGlobalContexts




-- Get an element indicated by given reference.
globalContextGet :: Int -> IO CssContext
globalContextGet = globalContainerGet myGlobalContexts




