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

This file is derived from dillo-3.0.5/src/css.hh.
Copyright assignment from css.cc:
 Copyright 2008-2014 Johannes Hofmann <Johannes.Hofmann@gmx.de>
-}




module Hello.Css.Declaration
  (
    CssDeclaration (..)
  , defaultDeclaration
  )
where




--import Debug.Trace

import Hello.Css.Parser.Property




-- https://www.w3.org/TR/css-syntax-3/#declaration: "Conceptually,
-- declarations are a particular instance of associating a property or
-- descriptor name with a value. Syntactically, a declaration has a name, a
-- value consisting of a list of component values, and an important flag
-- which is initially unset."
--
-- Also https://www.w3.org/TR/css-syntax-3/#syntax-description: "Declarations
-- are separated by semicolons." (this is useful to know when parsing a
-- declaration).
data CssDeclaration = CssDeclaration
  { property  :: CssProperty
  , important :: Bool
  } deriving (Show, Eq)




defaultDeclaration :: CssDeclaration
defaultDeclaration = CssDeclaration
  { property  = CssPropertyInvalid -- TODO: somewhere there is a code that does not set property2 field.
  , important = False
  }




