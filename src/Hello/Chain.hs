{-
Copyright (C) 2021 Kamil Ignacak acerion@wp.pl

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



module Hello.Chain
  (
    Chain (..)
  , chainLength
  , chainAny
  )
where




data Chain a b
  = Datum a
  | Link (Chain a b) b (Chain a b)
  deriving (Show, Eq)




chainLength :: (Chain a b) -> Int
chainLength chain = length' chain 0




length' (Link (Datum _) _ remainder) acc = length' remainder (acc + 1)
length' (Datum _)                    acc =                   (acc + 1)




chainAny :: (a -> Bool) -> (Chain a b) -> Bool
chainAny pred (Datum compound)                             = pred compound
chainAny pred (Link (Datum compound) combinator remainder) = (pred compound) || (chainAny pred remainder)






{-
ch41 =                                                                             Datum "one"
ch42 =                                                     Link (Datum "two") '+' (Datum "one")
ch43 =                           Link (Datum "three") '-' (Link (Datum "two") '+' (Datum "one"))
ch44 = Link (Datum "four") '/'  (Link (Datum "three") '-' (Link (Datum "two") '+' (Datum "one")))
-}




