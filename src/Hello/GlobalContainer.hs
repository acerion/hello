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

import Hello.Utils


data GlobalContainer a = GlobalContainer
  { items :: [a]
  }

mkGlobalContainer :: GlobalContainer a
mkGlobalContainer = GlobalContainer { items = [] }



-- Add given element. Return reference to it.
globalContainerPut :: IORef (GlobalContainer a) -> a -> IO Int
globalContainerPut globalContainer item = do
  container <- readIORef globalContainer
  let container' = GlobalContainer $ items container ++ [ item ]
  writeIORef globalContainer container'
  return ((length . items $ container') - 1)




-- Add new default element. Return reference to it.
-- Second argument is a constructor of default element.
globalContainerCtor :: IORef (GlobalContainer a) -> a -> IO Int
globalContainerCtor globalContainer defaultMaker = do
  container <- readIORef globalContainer
  let container' = GlobalContainer $ items container ++ [ defaultMaker ]
  writeIORef globalContainer container'
  return ((length . items $ container') - 1)




-- Copy given item, add it to container as new item. Return reference to new
-- item.
globalContainerCopyCtor :: IORef (GlobalContainer a) -> Int -> IO Int
globalContainerCopyCtor globalContainer itemRef = do
  container <- readIORef globalContainer
  let item = items container !! itemRef
      container' = GlobalContainer $ items container ++ [ item ] -- New item is being added as copy of existing item.
  writeIORef globalContainer container'
  return ((length . items $ container') - 1)




-- Get an element indicated by given reference.
globalContainerGet :: IORef (GlobalContainer a) -> Int -> IO a
globalContainerGet globalContainer ref = do
  container <- readIORef globalContainer
  return $ (items container) !! ref




-- Update existing entry indicated by a reference with given element.
globalContainerUpdate :: IORef (GlobalContainer a) -> Int -> a -> IO ()
globalContainerUpdate globalContainer ref item = do
  container <- readIORef globalContainer
  let container' = GlobalContainer $ listReplaceElem (items container) item ref
  writeIORef globalContainer container'



