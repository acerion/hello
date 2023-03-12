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




-- Wrappers around 'IORef as global variable' operations.




module Hello.GlobalContainer
  (
    globalContainerPut
  , globalContainerCtor
  , globalContainerCopyCtor
  , globalContainerGet
  , globalContainerUpdate

  , GlobalContainer
  , mkGlobalContainer
  )
where




import Prelude
--import Debug.Trace

import Data.IORef
import qualified Data.Vector as V




-- Vector is noticeable faster than a list. For small sets of global items
-- (e.g. StyleEngines) it doesn't matter, but when we deal with hundreds or
-- even thousands of StyleAttr objects, the speed of operations on a global
-- container suddently starts to matter.
--
-- Perhaps in future I will also add incrementing the size of vector not by
-- one (as needed on each 'put' operation) but perhaps by e.g. 10
-- (preallocate vector cells every time 'put' detects lack of free space in
-- the vector). This would also require adding an explicit 'count' field to
-- the type, because with such approach the count of real items in container
-- != length of container.
data GlobalContainer a = GlobalContainer
  { items :: V.Vector a
  }

mkGlobalContainer :: GlobalContainer a
mkGlobalContainer = GlobalContainer { items = V.empty }




-- Add given item. Return reference to it.
globalContainerPut :: IORef (GlobalContainer a) -> a -> IO Int
globalContainerPut globalContainer item = do
  container <- readIORef globalContainer
  let container' = GlobalContainer { items = V.snoc (items container) item }
  writeIORef globalContainer container'
  return $ (length . items $ container') - 1




-- Add new default item. Return reference to it.
-- Second argument is a constructor of default item.
globalContainerCtor :: IORef (GlobalContainer a) -> a -> IO Int
globalContainerCtor globalContainer defaultMaker = do
  container <- readIORef globalContainer
  let container' = GlobalContainer { items = V.snoc (items container) defaultMaker }
  writeIORef globalContainer container'
  return $ (length . items $ container') - 1




-- Copy given item, add it to container as new item. Return reference to new
-- item.
globalContainerCopyCtor :: IORef (GlobalContainer a) -> Int -> IO Int
globalContainerCopyCtor globalContainer itemRef = do
  container <- readIORef globalContainer
  let item = items container V.! itemRef
      container' = GlobalContainer { items = V.snoc (items container) item } -- New item is being added as copy of existing item.
  writeIORef globalContainer container'
  return $ (length . items $ container') - 1




-- Get an item indicated by given reference.
globalContainerGet :: IORef (GlobalContainer a) -> Int -> IO a
globalContainerGet globalContainer ref = do
  container <- readIORef globalContainer
  return $ (items container) V.! ref




-- Update existing entry indicated by a reference with given item.
globalContainerUpdate :: IORef (GlobalContainer a) -> Int -> a -> IO ()
globalContainerUpdate globalContainer ref item = do
  container <- readIORef globalContainer
  let container' = GlobalContainer { items = (items container) V.// [(ref, item)] } -- V.// is an update with a list of items
  writeIORef globalContainer container'



