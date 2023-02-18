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
  , CssDeclarationSet (..)
  , CssDeclarationSets

  , defaultDeclaration
  , defaultCssDeclarationSet
  , declarationsSetUpdateOrAdd
  , declarationsSetAppend
  )
where




--import Debug.Trace

import Data.Data (toConstr)
import qualified Data.Sequence as S

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




-- The isSafe flag compilcates this data type. I have to declare a new "Set"
-- type that is a wrapper around list of declarations + that one boolean
-- flag.
data CssDeclarationSet = CssDeclarationSet
  { isSafe :: Bool
  , items  :: S.Seq CssDeclaration
  } deriving (Show, Eq)




type CssDeclarationSets = (CssDeclarationSet, CssDeclarationSet)




defaultCssDeclarationSet :: CssDeclarationSet
defaultCssDeclarationSet = CssDeclarationSet
  { isSafe = True
  , items  = S.fromList []
  }




declarationsSetUpdateOrAdd :: CssDeclarationSet -> CssDeclaration -> CssDeclarationSet
declarationsSetUpdateOrAdd declSet decl =
  case S.findIndexL predicate ix of
    Just idx -> CssDeclarationSet {items = S.update idx decl ix, isSafe = newSafe declSet decl}
    Nothing  -> CssDeclarationSet {items = ix S.|> decl,         isSafe = newSafe declSet decl}
  where
    -- Use 'toConstr' to compare constructors, but values without passed to constructors.
    -- https://stackoverflow.com/questions/47861648/a-general-way-of-comparing-constructors-of-two-terms-in-haskell
    predicate :: CssDeclaration -> Bool
    predicate x = (toConstr . property $ x) == (toConstr . property $ decl)

    ix = items declSet

    -- TODO: 'background image' can be also set in value of
    -- CssPropertyBackground property. Expand the function to cover that case
    -- too.
    newSafe :: CssDeclarationSet -> CssDeclaration -> Bool
    newSafe declSet' decl' = isSafe declSet' && case property decl' of
                                                  CssPropertyDisplay _         -> False
                                                  CssPropertyBackgroundImage _ -> False
                                                  _                            -> True




{-
Merge values from incoming into target, return result of merging

I can't use a concatenation operator because the merging is not that
simple: it has to use declarationsSetUpdateOrAdd function.
-}
declarationsSetAppend :: CssDeclarationSet -> CssDeclarationSet -> CssDeclarationSet
declarationsSetAppend target incoming = if S.null . items $ incoming
                                        then target
                                        else declarationsSetAppend (declarationsSetUpdateOrAdd target iHead) iTail
  where
    iHead = S.index (items incoming) 0
    iTail = incoming {items = S.drop 1 (items incoming)}





