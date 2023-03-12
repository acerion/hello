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




module Hello.Css.StyleEngineGlobal
  (
    globalStyleEnginePut
  , globalStyleEngineCtor
  , globalStyleEngineGet
  , globalStyleEngineUpdate

  , CssStyleEngine (..)
  )
where




import Prelude
--import Debug.Trace

import Data.IORef
import System.IO.Unsafe

import Hello.GlobalContainer




data CssStyleEngine = CssStyleEngine
  { styleNodesStackSize :: Int
  } deriving (Show)

defaultCssStyleEngine :: CssStyleEngine
defaultCssStyleEngine = CssStyleEngine
  { styleNodesStackSize = 0
  }




-- This is only temporary, until more C++ code is moved to Haskell.
globalStyleEngines :: IORef (GlobalContainer CssStyleEngine)
{-# NOINLINE globalStyleEngines #-}
globalStyleEngines = unsafePerformIO (newIORef mkGlobalContainer)




-- Add given element. Return reference to it.
globalStyleEnginePut :: CssStyleEngine -> IO Int
globalStyleEnginePut = globalContainerPut globalStyleEngines




-- Add new default element. Return reference to it.
globalStyleEngineCtor :: IO Int
globalStyleEngineCtor = globalContainerCtor globalStyleEngines defaultCssStyleEngine




-- Get an element indicated by given reference.
globalStyleEngineGet :: Int -> IO CssStyleEngine
globalStyleEngineGet = globalContainerGet globalStyleEngines




-- Update existing entry indicated by a reference with given element.
globalStyleEngineUpdate :: Int -> CssStyleEngine -> IO ()
globalStyleEngineUpdate = globalContainerUpdate globalStyleEngines

