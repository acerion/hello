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




{-# LANGUAGE OverloadedStrings #-}




module HelloUtils (
    tripletFst
  , tripletSnd
  , tripletThrd
  , takeEnclosed
  , skipEnclosed
  ) where




import qualified Data.Text as T




tripletFst  (x, _, _) = x
tripletSnd  (_, y, _) = y
tripletThrd (_, _, z) = z




takeEnclosed :: T.Text -> T.Text -> T.Text -> Bool -> (Maybe T.Text, T.Text)
takeEnclosed text opening closing omitDelimiters = if T.isPrefixOf opening text
                                                   then (Just taken, T.drop len text)
                                                   else (Nothing, text)
  where len = T.length opening + T.length (fst pair) + if T.isPrefixOf closing (snd pair) then T.length closing else 0
        pair = T.breakOn closing (T.drop (T.length opening) text)
        enclosed = snd pair
        taken = if omitDelimiters
                then (T.splitOn closing ((T.splitOn opening text) !! 1) !! 0)
                else T.take len text




skipEnclosed :: T.Text -> T.Text -> T.Text -> T.Text
skipEnclosed text opening closing = snd $ takeEnclosed text opening closing True




