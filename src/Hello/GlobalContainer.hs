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
  , globalContainerGet
  , globalContainerUpdate
  )
where




import Prelude
--import Debug.Trace

import Data.IORef

import Hello.Utils




-- Add given element. Return reference to it.
globalContainerPut :: IORef [a] -> a -> IO Int
globalContainerPut globalContainer item = do
  old <- readIORef globalContainer
  let new = old ++ [ item ]
  writeIORef globalContainer new
  return (length new - 1)




-- Add new default element. Return reference to it.
-- Second argument is a constructor of default element.
globalContainerCtor :: IORef [a] -> a -> IO Int
globalContainerCtor globalContainer defaultMaker = do
  old <- readIORef globalContainer
  let new = old ++ [ defaultMaker ]
  writeIORef globalContainer new
  return (length new - 1)




-- Get an element indicated by given reference.
globalContainerGet :: IORef [a] -> Int -> IO a
globalContainerGet globalContainer ref = do
  list <- readIORef globalContainer
  return $ list !! ref




-- Update existing entry indicated by a reference with given element.
globalContainerUpdate :: IORef [a] -> Int -> a -> IO ()
globalContainerUpdate globalContainer ref item = do
  old <- readIORef globalContainer
  let new = listReplaceElem old item ref
  writeIORef globalContainer new



