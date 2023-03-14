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

This file is derived from dillo-3.0.5/src/styleengine.cc.
Copyright assignments from the file:
 * Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




{-# LANGUAGE ScopedTypeVariables #-}




module Hello.Css.StyleNode
  (
    StyleNode (..)
  , defaultStyleNode

  , updateOrAddHint
  , inheritHints
  , hintsSize
  )
where




import qualified Data.Sequence as S

import Hello.Css.Declaration
import Hello.Css.Parser.Property




data StyleNode = StyleNode
  { mainDeclSet      :: CssDeclarationSet
  , importantDeclSet :: CssDeclarationSet

   -- Non-CSS hint for styling a node. "non-css" because it comes from HTML
   -- tags and attributes instead of CSS style info.
  , nonCssDeclSet    :: CssDeclarationSet
  } deriving (Show)

defaultStyleNode :: StyleNode
defaultStyleNode = StyleNode defaultCssDeclarationSet defaultCssDeclarationSet defaultCssDeclarationSet




-- Update or add a non-Css hint in given style node.
updateOrAddHint :: StyleNode -> CssProperty -> StyleNode
updateOrAddHint node prop = node
  { nonCssDeclSet = declarationsSetUpdateOrAdd (nonCssDeclSet node) (CssDeclaration prop False) }




-- Inherit non-CSS hints from current element's parent to current element.
--
-- TODO: check order of arguments to 'append' function. Comment in C++ says
-- "original declListNonCss have precedence", which suggests that current
-- element's hints should have precenence.
--
-- The order is most probably correct because 'current' will overwrite
-- (update) any existing declarations in 'inherited', so 'current' will have
-- precedence.
--
-- TODO: consider just calling 'declarationsSetAppend parent this'. It will
-- work well if current is empty or non-empty, and if parent is empty or
-- non-empty.
--
-- Test the function with
-- test_data/html/attribute/cellpadding/table_cellpadding.html
inheritHints :: StyleNode -> StyleNode -> StyleNode
inheritHints parentNode currentNode = inheritedAndCurrent
  where
    inheritedAndCurrent = currentNode { nonCssDeclSet = if (S.length . items $ currentHints) > 0
                                                        then declarationsSetAppend inheritedHints currentHints
                                                        else inheritedHints
                                      }
    inheritedHints = nonCssDeclSet parentNode -- "copy constructor"
    currentHints   = nonCssDeclSet currentNode




-- Get size of non-CSS declaration set (non-CSS hints) in given node.
hintsSize :: StyleNode -> Int
hintsSize node = S.length . items . nonCssDeclSet $ node


