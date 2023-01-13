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

This file is derived from dillo-3.0.5/src/html.cc.
Copyright assignments from that file:
Copyright (C) 2005-2007 Jorge Arellano Cid <jcid@dillo.org>
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Html.Entity
  (
  -- Only for tests
    htmlEntityToIsoCode
  , EntityParser (..)
  )
where




import Prelude
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Read as T.R
import qualified Data.Map as M




data EntityParser = EntityParser {
    entityIsoCode :: Maybe Int
  , remainder :: T.Text
  } deriving (Eq, Show)




parserDefault :: EntityParser
parserDefault = EntityParser { entityIsoCode = Nothing, remainder = "" }




-- Entities list from the HTML 4.01 DTD.
-- (String, octal) pairs.
--
-- TODO: why this table is using octals?
--
-- TODO: newer standards of HTML have more entities. Update the table.
--
-- TODO: some entities allow not adding terminating ";". Don't report error
-- for such cases.
gEntities :: M.Map T.Text Int
gEntities = M.fromList [
  ("AElig",   0o306),     ("Aacute",   0o301),     ("Acirc",   0o302),     ("Agrave",  0o300),
  ("Alpha",   0o1621),    ("Aring",    0o305),     ("Atilde",  0o303),     ("Auml",    0o304),
  ("Beta",    0o1622),    ("Ccedil",   0o307),     ("Chi",     0o1647),    ("Dagger",  0o20041),
  ("Delta",   0o1624),    ("ETH",      0o320),     ("Eacute",  0o311),     ("Ecirc",   0o312),
  ("Egrave",  0o310),     ("Epsilon",  0o1625),    ("Eta",     0o1627),    ("Euml",    0o313),
  ("Gamma",   0o1623),    ("Iacute",   0o315),     ("Icirc",   0o316),     ("Igrave",  0o314),
  ("Iota",    0o1631),    ("Iuml",     0o317),     ("Kappa",   0o1632),    ("Lambda",  0o1633),
  ("Mu",      0o1634),    ("Ntilde",   0o321),     ("Nu",      0o1635),    ("OElig",   0o522),
  ("Oacute",  0o323),     ("Ocirc",    0o324),     ("Ograve",  0o322),     ("Omega",   0o1651),
  ("Omicron", 0o1637),    ("Oslash",   0o330),     ("Otilde",  0o325),     ("Ouml",    0o326),
  ("Phi",     0o1646),    ("Pi",       0o1640),    ("Prime",   0o20063),   ("Psi",     0o1650),
  ("Rho",     0o1641),    ("Scaron",   0o540),     ("Sigma",   0o1643),    ("THORN",   0o336),
  ("Tau",     0o1644),    ("Theta",    0o1630),    ("Uacute",  0o332),     ("Ucirc",   0o333),
  ("Ugrave",  0o331),     ("Upsilon",  0o1645),    ("Uuml",    0o334),     ("Xi",      0o1636),
  ("Yacute",  0o335),     ("Yuml",     0o570),     ("Zeta",    0o1626),    ("aacute",  0o341),
  ("acirc",   0o342),     ("acute",    0o264),     ("aelig",   0o346),     ("agrave",  0o340),
  ("alefsym", 0o20465),   ("alpha",    0o1661),    ("amp",     38),        ("and",     0o21047),
  ("ang",     0o21040),   ("aring",    0o345),     ("asymp",   0o21110),   ("atilde",  0o343),
  ("auml",    0o344),     ("bdquo",    0o20036),   ("beta",    0o1662),    ("brvbar",  0o246),
  ("bull",    0o20042),   ("cap",      0o21051),   ("ccedil",  0o347),     ("cedil",   0o270),
  ("cent",    0o242),     ("chi",      0o1707),    ("circ",    0o1306),    ("clubs",   0o23143),
  ("cong",    0o21105),   ("copy",     0o251),     ("crarr",   0o20665),   ("cup",     0o21052),
  ("curren",  0o244),     ("dArr",     0o20723),   ("dagger",  0o20040),   ("darr",    0o20623),
  ("deg",     0o260),     ("delta",    0o1664),    ("diams",   0o23146),   ("divide",  0o367),
  ("eacute",  0o351),     ("ecirc",    0o352),     ("egrave",  0o350),     ("empty",   0o21005),
  ("emsp",    0o20003),   ("ensp",     0o20002),   ("epsilon", 0o1665),    ("equiv",   0o21141),
  ("eta",     0o1667),    ("eth",      0o360),     ("euml",    0o353),     ("euro",    0o20254),
  ("exist",   0o21003),   ("fnof",     0o622),     ("forall",  0o21000),   ("frac12",  0o275),
  ("frac14",  0o274),     ("frac34",   0o276),     ("frasl",   0o20104),   ("gamma",   0o1663),
  ("ge",      0o21145),   ("gt",       62),        ("hArr",    0o20724),   ("harr",    0o20624),
  ("hearts",  0o23145),   ("hellip",   0o20046),   ("iacute",  0o355),     ("icirc",   0o356),
  ("iexcl",   0o241),     ("igrave",   0o354),     ("image",   0o20421),   ("infin",   0o21036),
  ("int",     0o21053),   ("iota",     0o1671),    ("iquest",  0o277),     ("isin",    0o21010),
  ("iuml",    0o357),     ("kappa",    0o1672),    ("lArr",    0o20720),   ("lambda",  0o1673),
  ("lang",    0o21451),   ("laquo",    0o253),     ("larr",    0o20620),   ("lceil",   0o21410),
  ("ldquo",   0o20034),   ("le",       0o21144),   ("lfloor",  0o21412),   ("lowast",  0o21027),
  ("loz",     0o22712),   ("lrm",      0o20016),   ("lsaquo",  0o20071),   ("lsquo",   0o20030),
  ("lt",      60),        ("macr",     0o257),     ("mdash",   0o20024),   ("micro",   0o265),
  ("middot",  0o267),     ("minus",    0o21022),   ("mu",      0o1674),    ("nabla",   0o21007),
  ("nbsp",    0o240),     ("ndash",    0o20023),   ("ne",      0o21140),   ("ni",      0o21013),
  ("not",     0o254),     ("notin",    0o21011),   ("nsub",    0o21204),   ("ntilde",  0o361),
  ("nu",      0o1675),    ("oacute",   0o363),     ("ocirc",   0o364),     ("oelig",   0o523),
  ("ograve",  0o362),     ("oline",    0o20076),   ("omega",   0o1711),    ("omicron", 0o1677),
  ("oplus",   0o21225),   ("or",       0o21050),   ("ordf",    0o252),     ("ordm",    0o272),
  ("oslash",  0o370),     ("otilde",   0o365),     ("otimes",  0o21227),   ("ouml",    0o366),
  ("para",    0o266),     ("part",     0o21002),   ("permil",  0o20060),   ("perp",    0o21245),
  ("phi",     0o1706),    ("pi",       0o1700),    ("piv",     0o1726),    ("plusmn",  0o261),
  ("pound",   0o243),     ("prime",    0o20062),   ("prod",    0o21017),   ("prop",    0o21035),
  ("psi",     0o1710),    ("quot",     34),        ("rArr",    0o20722),   ("radic",   0o21032),
  ("rang",    0o21452),   ("raquo",    0o273),     ("rarr",    0o20622),   ("rceil",   0o21411),
  ("rdquo",   0o20035),   ("real",     0o20434),   ("reg",     0o256),     ("rfloor",  0o21413),
  ("rho",     0o1701),    ("rlm",      0o20017),   ("rsaquo",  0o20072),   ("rsquo",   0o20031),
  ("sbquo",   0o20032),   ("scaron",   0o541),     ("sdot",    0o21305),   ("sect",    0o247),
  ("shy",     0o255),     ("sigma",    0o1703),    ("sigmaf",  0o1702),    ("sim",     0o21074),
  ("spades",  0o23140),   ("sub",      0o21202),   ("sube",    0o21206),   ("sum",     0o21021),
  ("sup",     0o21203),   ("sup1",     0o271),     ("sup2",    0o262),     ("sup3",    0o263),
  ("supe",    0o21207),   ("szlig",    0o337),     ("tau",     0o1704),    ("there4",  0o21064),
  ("theta",   0o1670),    ("thetasym", 0o1721),    ("thinsp",  0o20011),   ("thorn",   0o376),
  ("tilde",   0o1334),    ("times",    0o327),     ("trade",   0o20442),   ("uArr",    0o20721),
  ("uacute",  0o372),     ("uarr",     0o20621),   ("ucirc",   0o373),     ("ugrave",  0o371),
  ("uml",     0o250),     ("upsih",    0o1722),    ("upsilon", 0o1705),    ("uuml",    0o374),
  ("weierp",  0o20430),   ("xi",       0o1676),    ("yacute",  0o375),     ("yen",     0o245),
  ("yuml",    0o377),     ("zeta",     0o1666),    ("zwj",     0o20015),   ("zwnj",    0o20014)
  ]




-- TODO: make sure that this function is receiving only a small string of
-- characters from which an entity should be parsed. The entity should be at
-- the very beginning of input string.
--
-- The big parser of whole HTML document will have a big buffer with whole
-- HTML document. You probably don't want to pass this whole document here -
-- you want to pass only a small token that starts with entity to parse. Or
-- maybe you want to pass the whole document. Who knows how the final HTML
-- parser will work.
--
-- TODO: this function looks exactly like a first example of parser from Real
-- World Haskell. This is a good place to learn more from that chapter of
-- RWH.
htmlEntityToIsoCode :: T.Text -> Maybe EntityParser
htmlEntityToIsoCode text =
  case takeAmpersand parserDefault text of
    Nothing -> Nothing
    Just p1 ->
      case takeBody p1 (remainder p1) of
        Nothing -> Nothing
        Just p2 ->
          case takeSemicolon p2 (remainder p2) of
            Nothing -> Nothing
            Just p3 -> Just p3




takeAmpersand :: EntityParser -> T.Text -> Maybe EntityParser
takeAmpersand parser text = if T.isPrefixOf "&" text
                            then Just parser { remainder = T.tail text }
                            else Just parser




takeBody :: EntityParser -> T.Text -> Maybe EntityParser
takeBody parser text = if T.isPrefixOf "#" text
                       then Just (htmlEntityNumberToIsoCode parser (T.tail text))
                       else Just (htmlEntityNameToIsoCode parser text)




takeSemicolon :: EntityParser -> T.Text -> Maybe EntityParser
takeSemicolon parser text = if T.isPrefixOf ";" text
                            then Just (parser { remainder = T.tail text })
                            else Just parser




-- Parse a number into entity code.
--
-- Pass the string without initial "&#" to the function. It should be either
-- "nnnn;" or "xhhhh;" string.
--
-- TODO: original code returned error if iso code was >= 0xFFFF. Verify if
-- this needs to be introduced into this code.
htmlEntityNumberToIsoCode :: EntityParser -> T.Text -> EntityParser
htmlEntityNumberToIsoCode parserArg textArg =
  if T.isPrefixOf "x" text'
  then if T.isPrefixOf "x0x" text'  -- T.R.hexadecimal supports leading 0x, but leading 0x is not valid in numeric entity.
       then parserArg { entityIsoCode = Nothing, remainder = T.drop 3 textArg }
       else numReader T.R.hexadecimal parserArg (T.tail textArg)
  else numReader T.R.decimal parserArg textArg
  where
    text' = T.toLower textArg
    numReader :: T.R.Reader Int -> EntityParser -> T.Text -> EntityParser
    numReader reader parser text =
      case reader text of
        Right pair -> parser { entityIsoCode = Just (if code >= 145 && code <= 151 then replaceQuotes code else code)
                             , remainder = snd pair }
          where code = fst pair
        Left _     -> parser { entityIsoCode = Nothing
                             , remainder = text }




-- TODO: original C code probably allowed also for non-standard entities
-- defined with "<!ENTITY name "value">".
-- This was the code that was getting "name" of enity:
--       while (*++s && (isalnum(*s) || strchr(":_.-", *s))) ;
-- The pred function should take this into consideration.
htmlEntityNameToIsoCode :: EntityParser -> T.Text -> EntityParser
htmlEntityNameToIsoCode parser name = parser { entityIsoCode = M.lookup name' gEntities,
                                               remainder = T.drop (T.length name') name }
  where name' = T.takeWhile predicate name
        predicate :: Char -> Bool
        predicate c = Data.Char.isDigit c || Data.Char.isAsciiUpper c || Data.Char.isAsciiLower c




-- This is MS non-standard "smart quotes" (w1252). Now even deprecated by them!
--
-- SGML for HTML4.01 defines c >= 128 and c <= 159 as UNUSED.
-- TODO: Probably I should remove this hack, and add a HTML warning. --Jcid
replaceQuotes :: Int -> Int
replaceQuotes isoCode = case isoCode of
                          145 -> Data.Char.ord '\''  -- 0x27
                          146 -> Data.Char.ord '\''  -- 0x27
                          147 -> Data.Char.ord '"'   -- 0x22
                          148 -> Data.Char.ord '"'   -- 0x22
                          149 -> 176                 -- 0xb0 ('degrees' sign)
                          150 -> Data.Char.ord '-'   -- 0x2d
                          151 -> Data.Char.ord '-'   -- 0x2d
                          _   -> isoCode
