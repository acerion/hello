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




module Hello.Utils.Parser
  (
    Parser (..)
  )
where




import Control.Applicative (Alternative(..))




-- The function's return value has type "Maybe (state, value)", which is
-- different from usual "(state, Maybe value)" type found in code written
-- thus far. The new style is more in line with usual type of Haskell-based
-- parsers.
newtype Parser state result = Parser { runParser :: state -> Maybe (state, result) }





instance Functor (Parser state) where
  -- 'fun' function is applied to parser's result, so we have to run the
  -- parser first in order to have some result to which 'fun' will be
  -- applied.
  fmap fun parser = Parser parserFn
    where
      parserFn state = case runParser parser state of
                         Just (state', result') -> Just (state', fun result')
                         _                      -> Nothing




instance Applicative (Parser state) where

  -- pure :: a -> f a
  -- pure :: result -> Parser (state, result)
  -- 'pure' can wrap data, but also functions.
  pure x = Parser $ \state -> Just (state, x)

  -- f (a -> b) -> f a -> f b
  -- Parser (a -> b) -> Parser a -> Parser b

{-
  (Parser runParser1) <*> (Parser runParser2) =
    Parser $ \state ->
                   do
                     -- As you can see in definition of 'pure', 'pure' called
                     -- on some function 'fun' will wrap the function in
                     -- Parser. We can get this function back by running
                     -- the parser containing the function.
                     (state', fun)    <- runParser1 state

                     -- First parser gave us function 'fun'. Second parser
                     -- will give us a data, on which we can run 'fun'.
                     (state'', value) <- runParser2 state'

                     -- Now call the function 'fun' obtained by running a
                     -- first parser on data obtained by running a second
                     -- parser.
                     Just (state'', fun value)
-}

  -- In this implementation of <*> the first step is the same as in above
  -- implementation of <*>: run first parser to get a function out of it
  -- (this is done by first call to runParser).
  --
  -- The second step here is similar to second step above in that it runs a
  -- second parser with updated "state'". However the second parser is not a
  -- verbatim second parser: it is modified by "fmap fun". Since a parser is
  -- a functor, we can call fmap over it, and we do so here.
  --
  -- What is the result of "fmap fun parser2"? Per definition of fmap for
  -- Parser, the result is another parser (a parser with modifed parsing
  -- function). When executing the modified parsing function through
  -- runParser, the modified parsing function will produce new (state,
  -- result) pair, and then apply given 'fun' to the result.
  parser1 <*> parser2 = Parser $ \state -> case runParser parser1 state of
                                             Just (state', fun) -> runParser (fmap fun parser2) state'
                                             Nothing            -> Nothing




instance Alternative (Parser state) where
  empty = Parser $ const Nothing

  -- Run first (left) parser, see if it succeeds. If it does succeed, then we
  -- don't have to run the second parser because one of parsers already
  -- succeeded.
  --
  -- If the first parser doesn't succeed, run the second one and return the
  -- result of running it - it will be either success or failure, but we
  -- can't do anything much about it.
  parser1 <|> parser2 = Parser $ \state -> case runParser parser1 state of
                                             Just (state1, value1) -> Just (state1, value1)
                                             Nothing -> case runParser parser2 state of
                                                          Just (state2, value2) -> Just (state2, value2)
                                                          Nothing               -> Nothing




instance Monad (Parser state) where

  -- (>>=) :: m a      -> (a -> m b)      -> m b
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  parser >>= fn = Parser $ \ state ->
                             -- We need to extract the "a" value that sits
                             -- inside of "Parser state"/"m a". We do this by
                             -- executing the parser.
                             case runParser parser state of
                               -- Now we run the (a -> m b) function. Notice
                               -- that we run it only on "value". "value" == "a".
                               --
                               -- Probably to not lose information from
                               -- "state'", we combine "(fn value)" and
                               -- "state'" in one expression.
                               --
                               -- Compiler doesn't allow us to return just
                               -- the result of "(fn value)", we have to do
                               -- something more with it.
                               Just (state', value) -> runParser (fn value) state'
                               Nothing              -> Nothing


