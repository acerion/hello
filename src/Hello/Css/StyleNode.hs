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

This file is derived from dillo-3.0.5/src/styleengine.cc.
Copyright assignments from the file:
 * Copyright 2008-2009 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}




module Hello.Css.StyleNode
  (
    StyleNode (..)
  , defaultCssStyleNodesStack
  )
where





import Hello.Css.Parser




data StyleNode = StyleNode
  {
    mainDeclSet      :: CssDeclarationSet
  , importantDeclSet :: CssDeclarationSet
  , nonCssDeclSet    :: CssDeclarationSet
  }




defaultCssStyleNodesStack :: [StyleNode]
defaultCssStyleNodesStack = []



