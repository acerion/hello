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




{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Css.DeclarationSetsGlobal
  (
    globalDeclarationSetPut
  , globalDeclarationSetCtor
  , globalDeclarationSetGet
  , globalDeclarationSetUpdate
  )
where




import Prelude
--import Debug.Trace

import Data.IORef
import System.IO.Unsafe

import Hello.Css.Parser.Declaration

import Hello.GlobalContainer




-- This is only temporary, until more C++ code is moved to Haskell.
globalDeclSets :: IORef [CssDeclarationSet]
{-# NOINLINE globalDeclSets #-}
globalDeclSets = unsafePerformIO (newIORef [])




-- Add given element. Return reference to it.
globalDeclarationSetPut :: CssDeclarationSet -> IO Int
globalDeclarationSetPut = globalContainerPut globalDeclSets




-- Add new default element. Return reference to it.
globalDeclarationSetCtor :: IO Int
globalDeclarationSetCtor = globalContainerCtor globalDeclSets defaultCssDeclarationSet




-- Get an element indicated by given reference.
globalDeclarationSetGet :: Int -> IO CssDeclarationSet
globalDeclarationSetGet = globalContainerGet globalDeclSets




-- Update existing entry indicated by a reference with given element.
globalDeclarationSetUpdate :: Int -> CssDeclarationSet -> IO ()
globalDeclarationSetUpdate = globalContainerUpdate globalDeclSets

