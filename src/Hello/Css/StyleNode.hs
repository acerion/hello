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
  )
where




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
updateOrAddHint node property = node
  { nonCssDeclSet = declarationsSetUpdateOrAdd (nonCssDeclSet node) (CssDeclaration property False) }


