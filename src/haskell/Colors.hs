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

This file is derived from dillo-3.0.5/src/colors.c.
Copyright assignments from that file:
Copyright (C) 2000-2007 Jorge Arellano Cid <jcid@dillo.org>
-}




{-# LANGUAGE OverloadedStrings #-}




module Colors( colorsStringToColor
             , colorsStringToColorWithDefault

               -- Only for tests
             , colorsVisitedColor
             , colorsDistance2
             , colorsDistance3
             ) where




import Prelude
import Foreign.C.String
import Foreign
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.Read as T.R
import qualified Data.Vector as V




{-
The colors MUST be in alphabetical order and lower case because the code uses
a binary search.
-}
colorsTable = V.fromList [
    ( "aliceblue",            0xf0f8ff )
  , ( "antiquewhite",         0xfaebd7 )
  , ( "aqua",                 0x00ffff )
  , ( "aquamarine",           0x7fffd4 )
  , ( "azure",                0xf0ffff )
  , ( "beige",                0xf5f5dc )
  , ( "bisque",               0xffe4c4 )
  , ( "black",                0x000000 )
  , ( "blanchedalmond",       0xffebcd )
  , ( "blue",                 0x0000ff )
  , ( "blueviolet",           0x8a2be2 )
  , ( "brown",                0xa52a2a )
  , ( "burlywood",            0xdeb887 )
  , ( "cadetblue",            0x5f9ea0 )
  , ( "chartreuse",           0x7fff00 )
  , ( "chocolate",            0xd2691e )
  , ( "coral",                0xff7f50 )
  , ( "cornflowerblue",       0x6495ed )
  , ( "cornsilk",             0xfff8dc )
  , ( "crimson",              0xdc1436 )
  , ( "cyan",                 0x00ffff )
  , ( "darkblue",             0x00008b )
  , ( "darkcyan",             0x008b8b )
  , ( "darkgoldenrod",        0xb8860b )
  , ( "darkgray",             0xa9a9a9 )
  , ( "darkgreen",            0x006400 )
  , ( "darkgrey",             0xa9a9a9 )
  , ( "darkkhaki",            0xbdb76b )
  , ( "darkmagenta",          0x8b008b )
  , ( "darkolivegreen",       0x556b2f )
  , ( "darkorange",           0xff8c00 )
  , ( "darkorchid",           0x9932cc )
  , ( "darkred",              0x8b0000 )
  , ( "darksalmon",           0xe9967a )
  , ( "darkseagreen",         0x8fbc8f )
  , ( "darkslateblue",        0x483d8b )
  , ( "darkslategray",        0x2f4f4f )
  , ( "darkslategrey",        0x2f4f4f )
  , ( "darkturquoise",        0x00ced1 )
  , ( "darkviolet",           0x9400d3 )
  , ( "deeppink",             0xff1493 )
  , ( "deepskyblue",          0x00bfff )
  , ( "dimgray",              0x696969 )
  , ( "dimgrey",              0x696969 )
  , ( "dodgerblue",           0x1e90ff )
  , ( "firebrick",            0xb22222 )
  , ( "floralwhite",          0xfffaf0 )
  , ( "forestgreen",          0x228b22 )
  , ( "fuchsia",              0xff00ff )
  , ( "gainsboro",            0xdcdcdc )
  , ( "ghostwhite",           0xf8f8ff )
  , ( "gold",                 0xffd700 )
  , ( "goldenrod",            0xdaa520 )
  , ( "gray",                 0x808080 )
  , ( "green",                0x008000 )
  , ( "greenyellow",          0xadff2f )
  , ( "grey",                 0x808080 )
  , ( "honeydew",             0xf0fff0 )
  , ( "hotpink",              0xff69b4 )
  , ( "indianred",            0xcd5c5c )
  , ( "indigo",               0x4b0082 )
  , ( "ivory",                0xfffff0 )
  , ( "khaki",                0xf0e68c )
  , ( "lavender",             0xe6e6fa )
  , ( "lavenderblush",        0xfff0f5 )
  , ( "lawngreen",            0x7cfc00 )
  , ( "lemonchiffon",         0xfffacd )
  , ( "lightblue",            0xadd8e6 )
  , ( "lightcoral",           0xf08080 )
  , ( "lightcyan",            0xe0ffff )
  , ( "lightgoldenrodyellow", 0xfafad2 )
  , ( "lightgray",            0xd3d3d3 )
  , ( "lightgreen",           0x90ee90 )
  , ( "lightgrey",            0xd3d3d3 )
  , ( "lightpink",            0xffb6c1 )
  , ( "lightsalmon",          0xffa07a )
  , ( "lightseagreen",        0x20b2aa )
  , ( "lightskyblue",         0x87cefa )
  , ( "lightslategray",       0x778899 )
  , ( "lightslategrey",       0x778899 )
  , ( "lightsteelblue",       0xb0c4de )
  , ( "lightyellow",          0xffffe0 )
  , ( "lime",                 0x00ff00 )
  , ( "limegreen",            0x32cd32 )
  , ( "linen",                0xfaf0e6 )
  , ( "magenta",              0xff00ff )
  , ( "maroon",               0x800000 )
  , ( "mediumaquamarine",     0x66cdaa )
  , ( "mediumblue",           0x0000cd )
  , ( "mediumorchid",         0xba55d3 )
  , ( "mediumpurple",         0x9370db )
  , ( "mediumseagreen",       0x3cb371 )
  , ( "mediumslateblue",      0x7b68ee )
  , ( "mediumspringgreen",    0x00fa9a )
  , ( "mediumturquoise",      0x48d1cc )
  , ( "mediumvioletred",      0xc71585 )
  , ( "midnightblue",         0x191970 )
  , ( "mintcream",            0xf5fffa )
  , ( "mistyrose",            0xffe4e1 )
  , ( "moccasin",             0xffe4b5 )
  , ( "navajowhite",          0xffdead )
  , ( "navy",                 0x000080 )
  , ( "oldlace",              0xfdf5e6 )
  , ( "olive",                0x808000 )
  , ( "olivedrab",            0x6b8e23 )
  , ( "orange",               0xffa500 )
  , ( "orangered",            0xff4500 )
  , ( "orchid",               0xda70d6 )
  , ( "palegoldenrod",        0xeee8aa )
  , ( "palegreen",            0x98fb98 )
  , ( "paleturquoise",        0xafeeee )
  , ( "palevioletred",        0xdb7093 )
  , ( "papayawhip",           0xffefd5 )
  , ( "peachpuff",            0xffdab9 )
  , ( "peru",                 0xcd853f )
  , ( "pink",                 0xffc0cb )
  , ( "plum",                 0xdda0dd )
  , ( "powderblue",           0xb0e0e6 )
  , ( "purple",               0x800080 )
  , ( "red",                  0xff0000 )
  , ( "rosybrown",            0xbc8f8f )
  , ( "royalblue",            0x4169e1 )
  , ( "saddlebrown",          0x8b4513 )
  , ( "salmon",               0xfa8072 )
  , ( "sandybrown",           0xf4a460 )
  , ( "seagreen",             0x2e8b57 )
  , ( "seashell",             0xfff5ee )
  , ( "sienna",               0xa0522d )
  , ( "silver",               0xc0c0c0 )
  , ( "skyblue",              0x87ceeb )
  , ( "slateblue",            0x6a5acd )
  , ( "slategray",            0x708090 )
  , ( "slategrey",            0x708090 )
  , ( "snow",                 0xfffafa )
  , ( "springgreen",          0x00ff7f )
  , ( "steelblue",            0x4682b4 )
  , ( "tan",                  0xd2b48c )
  , ( "teal",                 0x008080 )
  , ( "thistle",              0xd8bfd8 )
  , ( "tomato",               0xff6347 )
  , ( "turquoise",            0x40e0d0 )
  , ( "violet",               0xee82ee )
  , ( "wheat",                0xf5deb3 )
  , ( "white",                0xffffff )
  , ( "whitesmoke",           0xf5f5f5 )
  , ( "yellow",               0xffff00 )
  , ( "yellowgreen",          0x9acd32 )
  ] :: V.Vector (T.Text, Int)




{-
Parse a color string. Case insensitive.

  - If the string begins with # or with 0x, return the color number
    (with 'RGB' expanded to 'RRGGBB').
  - Else search the set of named colors.
  - As a last resort, treat it as bare hex as in the first case.

  Return Value:
     Parsed color if successful,
     defaultColor on error.

  "err" argument:
     0 if a color beginning with '#' is successfully parsed
       or the color is a recognized word.
     1 if the color is bare hex or can't be parsed at all.
     2 if a color beginning with 0[xX] is successfully parsed.
TODO: add returning of "err" flag
-}
colorsStringToColorWithDefault :: T.Text -> Int -> Int
colorsStringToColorWithDefault text defaultColor =
  case colorsStringToColor text of
    Just val -> val
    Nothing  -> defaultColor




{--
Try converting string with hexadecimal value into integer.
Supported formats: "RRGGBB" or "RGB".

Don't pass strings with prefixes ("#RRGGBB" or "0xRGB"). Even tough
T.R.hexadecimal can handle "0x" prefix, for simlicity's sake this function
doesn't support it.
--}
colorsParseHex :: T.Text -> Maybe Int
colorsParseHex text =
  case T.R.hexadecimal text of
    Right pair -> parseByHexFormat (fst pair) (snd pair) text
    Left pair  -> Nothing
  where
    -- TODO: what happens with 'rem' text? Shouldn't we pass it as a token to next parser?
    parseByHexFormat parsed rem text =
      case (T.length text) - (T.length rem) of
        6 -> Just parsed  -- RRGGBB format
        3 -> Just (((parsed .&. 0xf00) `shiftL` 12) .|. ((parsed .&. 0xf00) `shiftL` 8) .|. -- RGB format
                   ((parsed .&. 0x0f0) `shiftL` 8)  .|. ((parsed .&. 0x0f0) `shiftL` 4) .|.
                   ((parsed .&. 0x00f) `shiftL` 4)  .|. ((parsed .&. 0x00f) `shiftL` 0))
        otherwise -> Nothing




-- Naive re-implementation of binary search in C code.
colorsTableSearchName :: V.Vector (T.Text, Int) -> T.Text -> Maybe Int
colorsTableSearchName vec name = binarySearch vec (T.toLower name) 0 ((V.length vec) - 1)
  where
    binarySearch :: V.Vector (T.Text, Int) -> T.Text -> Int -> Int -> Maybe Int
    binarySearch vec name low high =
      if low > high
      then Nothing
      else case compare (fst (vec V.! mid)) name of
             GT -> binarySearch vec name low (mid - 1)
             LT -> binarySearch vec name (mid + 1) high
             EQ -> Just (snd (vec V.! mid))
      where
        mid = (low + high) `div` 2




{-
Parse a color string. Case insensitive.

Just like colorsStringToColorWithDefault, but without "default color"
argument.

TODO: add returning of "err" flag
-}
colorsStringToColor :: T.Text -> Maybe Int
colorsStringToColor text
  | T.length colorName < T.length "red"                        = Nothing -- Shortest bare string with just color name
  | T.index colorName 0 == '#'                                 = colorsParseHex (T.drop 1 colorName)
  | T.isPrefixOf "0x" colorName || T.isPrefixOf "0X" colorName = colorsParseHex (T.drop 2 colorName)
  | otherwise = case colorsTableSearchName colorsTable colorName of
      Just val -> Just val
      Nothing  -> colorsParseHex colorName -- Try for RRGGBB lacking the leading '#'.
  where colorName = T.strip text -- TODO: move the stripping up the call chain.





-- Return: [0-3]
colorsDistance2 :: Int -> Int -> Int
colorsDistance2 c1 c2 = foldl count 0 abses
  where abses = [(abs((c1 .&. 0x0000ff) - (c2 .&. 0x0000ff)) >= 0x000060),
                 (abs((c1 .&. 0x00ff00) - (c2 .&. 0x00ff00)) >= 0x006000),
                 (abs((c1 .&. 0xff0000) - (c2 .&. 0xff0000)) >= 0x600000)]
        count acc p = if p then acc + 1 else acc



-- Return: [0-3] (requires less contrast than colorsDistance2)
colorsDistance3 :: Int -> Int -> Int
colorsDistance3 c1 c2 = foldl count 0 abses
  where abses = [(abs((c1 .&. 0x0000ff) - (c2 .&. 0x0000ff)) >= 0x000040),
                 (abs((c1 .&. 0x00ff00) - (c2 .&. 0x00ff00)) >= 0x004000),
                 (abs((c1 .&. 0xff0000) - (c2 .&. 0xff0000)) >= 0x400000)]
        count acc p = if p then acc + 1 else acc






-- Return a suitable "visited link" color
--
-- Return value:
--   if candidate has good contrast with C_txt, C_lnk and C_bg  -> candidate
--   else another color (from the internal list)
--
-- Tuned with: slashdot.org, paulgraham.com, newsforge.com, linuxjournal.com.
colorsVisitedColor :: Int -> Int -> Int -> Int -> Int
colorsVisitedColor candidate txt lnk bg =
                           -- purple    darkcyan  darkmagenta olive     darkred   coral     black
  let shortList = [candidate, 0x800080, 0x008b8b, 0x8b008b,   0x808000, 0x8b0000, 0xff7f50, 0x000000 ]
      bestScore = 0
      bestColor = candidate
  in
    keepSearching shortList (bestScore, bestColor) txt lnk bg
--    if score >= 7
--    then thisColor
--    else keepSearching
  where
    keepSearching [] bestScoreColor _ _ _          = snd bestScoreColor
    keepSearching (c:cs) bestScoreColor txt lnk bg = if score >= 7
                                                     then c
                                                     else keepSearching cs bestScoreColor2 txt lnk bg
      where
        distanceTxt        = colorsDistance2 txt c
        distanceLink       = colorsDistance2 lnk c
        distanceBackground = colorsDistance2 bg  c
        bestScoreColor2    = if score > fst bestScoreColor then (score, c) else bestScoreColor
        score = x + y + z
          where x = if distanceBackground >= 2 then 4 else 2 * distanceBackground
                y = if distanceTxt + distanceLink >= 2 then 2 else distanceTxt + distanceLink
                z = if colorsDistance3 lnk c >= 1 then 1 else 0

