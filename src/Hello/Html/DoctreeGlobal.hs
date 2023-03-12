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




module Hello.Html.DoctreeGlobal
  (
    globalDoctreeCtor
  , globalDoctreeGet
  , globalDoctreeUpdate
  )
where




import Prelude

--import Debug.Trace

import Data.IORef
import System.IO.Unsafe

import Hello.GlobalContainer
import Hello.Html.Doctree




myGlobalDoctrees :: IORef (GlobalContainer Doctree)
{-# NOINLINE myGlobalDoctrees #-}
myGlobalDoctrees = unsafePerformIO (newIORef mkGlobalContainer)




-- Add new default element. Return reference to it.
-- Argument is a constructor of default element.
globalDoctreeCtor :: IO Int
globalDoctreeCtor = globalContainerCtor myGlobalDoctrees defaultDoctree




-- Get an element indicated by given reference.
globalDoctreeGet :: Int -> IO Doctree
globalDoctreeGet = globalContainerGet myGlobalDoctrees




-- Update existing entry indicated by a reference with given element.
globalDoctreeUpdate :: Int -> Doctree -> IO ()
globalDoctreeUpdate = globalContainerUpdate myGlobalDoctrees

