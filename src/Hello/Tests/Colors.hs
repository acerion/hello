{-
NOTE: This file is under yet-unspecified Free Software license.  The license
may be different than a license for whole "Hello" package.
-}




{-# LANGUAGE OverloadedStrings #-}




module Hello.Tests.Colors
  (
    testsColors
  )
where




import Test.HUnit
import Numeric (showHex)

import Hello.Colors




-- Test data for colorsDistance2.
-- First and second column was generated by callign 'shuf' UNIX tool on
-- Colors.colorsTable.
-- Third column is a result of calling original C function Color_distance2()
-- with the two arguments from first two columns.
-- Fourth column is a result of calling original C function Color_distance3()
-- with the two arguments from first two columns.
distanceXTestData :: [[Int]]
distanceXTestData =
  --  color1    color2    d2 d3
  [ [ 0xb22222, 0x20b2aa, 3, 3 ],
    [ 0x4169e1, 0xf0e68c, 2, 3 ],
    [ 0xbdb76b, 0x1e90ff, 2, 2 ],
    [ 0xffe4c4, 0x808080, 2, 3 ],
    [ 0x00fa9a, 0x00ff00, 1, 1 ],
    [ 0x00ffff, 0xffffe0, 1, 1 ],
    [ 0xf08080, 0xffd700, 1, 2 ],
    [ 0x3cb371, 0x778899, 0, 0 ],
    [ 0xd2691e, 0xafeeee, 2, 2 ],
    [ 0xa0522d, 0xff8c00, 0, 1 ],
    [ 0xffd700, 0x00ffff, 2, 2 ],
    [ 0xdcdcdc, 0xdc1436, 2, 2 ],
    [ 0xfffff0, 0x696969, 3, 3 ],
    [ 0xffefd5, 0xe6e6fa, 0, 0 ],
    [ 0xffffe0, 0x708090, 2, 3 ],
    [ 0xffa500, 0xff6347, 0, 2 ],
    [ 0xffffff, 0xa9a9a9, 0, 3 ],
    [ 0x191970, 0xf5f5f5, 3, 3 ],
    [ 0x00ffff, 0xffe4e1, 1, 1 ],
    [ 0xafeeee, 0x6b8e23, 2, 3 ],
    [ 0xf8f8ff, 0xfffacd, 0, 0 ],
    [ 0xffff00, 0xfff8dc, 1, 1 ],
    [ 0xd3d3d3, 0xb22222, 2, 2 ],
    [ 0xadff2f, 0xf08080, 1, 3 ],
    [ 0x00ff00, 0xffe4c4, 2, 2 ],
    [ 0x778899, 0x00bfff, 2, 2 ],
    [ 0x7cfc00, 0x006400, 2, 2 ],
    [ 0xf5deb3, 0xd3d3d3, 0, 0 ],
    [ 0xffa07a, 0xffff00, 1, 2 ],
    [ 0xfff5ee, 0xb0c4de, 0, 1 ],
    [ 0x00008b, 0x8b008b, 1, 1 ],
    [ 0xb0c4de, 0x87ceeb, 0, 0 ],
    [ 0x5f9ea0, 0x00ff7f, 1, 2 ],
    [ 0x000080, 0xd3d3d3, 2, 3 ],
    [ 0xd8bfd8, 0x7b68ee, 0, 2 ],
    [ 0xff8c00, 0xffa07a, 1, 1 ],
    [ 0x800080, 0x8fbc8f, 1, 1 ],
    [ 0x008b8b, 0xfffaf0, 3, 3 ],
    [ 0x008080, 0xff0000, 3, 3 ],
    [ 0x7fff00, 0xff1493, 3, 3 ],
    [ 0x2e8b57, 0xfaf0e6, 3, 3 ],
    [ 0xfffacd, 0xffb6c1, 0, 1 ],
    [ 0xd2b48c, 0xa9a9a9, 0, 0 ],
    [ 0xf5fffa, 0x00fa9a, 2, 2 ],
    [ 0xdc1436, 0xf0f8ff, 2, 2 ],
    [ 0xfaf0e6, 0xb8860b, 2, 3 ],
    [ 0xa9a9a9, 0xd8bfd8, 0, 0 ],
    [ 0x228b22, 0x48d1cc, 1, 2 ],
    [ 0x9400d3, 0x000080, 1, 2 ],
    [ 0xdb7093, 0x800000, 2, 3 ],
    [ 0xff0000, 0xd2691e, 1, 1 ],
    [ 0xe0ffff, 0x3cb371, 2, 3 ],
    [ 0xadd8e6, 0x00ced1, 1, 1 ],
    [ 0xc0c0c0, 0xff69b4, 0, 1 ],
    [ 0x000000, 0x87cefa, 3, 3 ],
    [ 0xba55d3, 0x008b8b, 1, 2 ],
    [ 0x808000, 0xf8f8ff, 3, 3 ],
    [ 0x808080, 0xadff2f, 1, 2 ],
    [ 0x00ced1, 0xf4a460, 2, 2 ],
    [ 0x1e90ff, 0xfafad2, 2, 2 ],
    [ 0x2f4f4f, 0x008000, 0, 1 ],
    [ 0xffb6c1, 0xcd853f, 1, 1 ],
    [ 0xff1493, 0x90ee90, 2, 2 ],
    [ 0x708090, 0xffc0cb, 1, 2 ],
    [ 0x708090, 0xf5f5dc, 2, 3 ],
    [ 0xee82ee, 0xadd8e6, 0, 2 ],
    [ 0x808080, 0x808000, 1, 1 ],
    [ 0xffe4e1, 0xff00ff, 1, 1 ],
    [ 0xdeb887, 0xfff5ee, 1, 1 ],
    [ 0x556b2f, 0x696969, 0, 0 ],
    [ 0x8fbc8f, 0x00008b, 2, 2 ],
    [ 0x8a2be2, 0xffffff, 2, 2 ],
    [ 0x48d1cc, 0x6a5acd, 1, 1 ],
    [ 0x696969, 0x2f4f4f, 0, 0 ],
    [ 0x4b0082, 0x9370db, 1, 3 ],
    [ 0x9370db, 0xffa500, 2, 2 ],
    [ 0xfafad2, 0x2f4f4f, 3, 3 ],
    [ 0xc71585, 0x4682b4, 2, 2 ],
    [ 0x7b68ee, 0xf5fffa, 2, 2 ],
    [ 0xfff0f5, 0xfff0f5, 0, 0 ],
    [ 0x778899, 0xfaebd7, 2, 2 ],
    [ 0xb8860b, 0xf5deb3, 1, 2 ],
    [ 0xb0e0e6, 0xbdb76b, 1, 1 ],
    [ 0xfffafa, 0xdeb887, 1, 2 ],
    [ 0x20b2aa, 0xffefd5, 1, 1 ],
    [ 0xff4500, 0x778899, 2, 3 ],
    [ 0xff6347, 0x9acd32, 2, 2 ],
    [ 0xffdab9, 0xfffff0, 0, 0 ],
    [ 0x8b4513, 0xf0fff0, 3, 3 ],
    [ 0x0000ff, 0xfdf5e6, 2, 2 ],
    [ 0x8b008b, 0x4b0082, 0, 1 ],
    [ 0x32cd32, 0xf0ffff, 2, 2 ],
    [ 0xfa8072, 0xff00ff, 2, 2 ],
    [ 0xf4a460, 0x8b4513, 1, 3 ],
    [ 0xffe4b5, 0x708090, 2, 2 ],
    [ 0xcd853f, 0x66cdaa, 2, 3 ],
    [ 0xa52a2a, 0x0000cd, 2, 2 ],
    [ 0xffdead, 0xa0522d, 2, 3 ],
    [ 0x87cefa, 0xe0ffff, 0, 1 ],
    [ 0xff00ff, 0x0000ff, 1, 1 ],
    [ 0xf5f5f5, 0x000000, 3, 3 ],
    [ 0xfaebd7, 0xa52a2a, 2, 3 ],
    [ 0xfff8dc, 0x483d8b, 2, 3 ],
    [ 0xe9967a, 0x4169e1, 2, 2 ],
    [ 0xff7f50, 0x9400d3, 3, 3 ],
    [ 0xf0e68c, 0x228b22, 2, 3 ],
    [ 0xfffaf0, 0x191970, 3, 3 ],
    [ 0x6495ed, 0x2e8b57, 1, 1 ],
    [ 0x9932cc, 0xb0e0e6, 1, 1 ],
    [ 0x40e0d0, 0xd2b48c, 1, 2 ],
    [ 0x9acd32, 0x5f9ea0, 1, 1 ],
    [ 0xa9a9a9, 0x8a2be2, 1, 1 ],
    [ 0x006400, 0xdcdcdc, 3, 3 ],
    [ 0x4682b4, 0xff4500, 2, 2 ],
    [ 0x6a5acd, 0xba55d3, 0, 1 ],
    [ 0x483d8b, 0x7cfc00, 2, 2 ],
    [ 0x66cdaa, 0xff7f50, 1, 3 ],
    [ 0xeee8aa, 0x32cd32, 2, 2 ],
    [ 0xf0fff0, 0xfffafa, 0, 0 ],
    [ 0x6b8e23, 0xdda0dd, 2, 2 ],
    [ 0x98fb98, 0xbc8f8f, 1, 1 ],
    [ 0xffebcd, 0xffdab9, 0, 0 ],
    [ 0x2f4f4f, 0xda70d6, 2, 2 ],
    [ 0xe6e6fa, 0x00ffff, 1, 1 ],
    [ 0xda70d6, 0x008080, 1, 2 ],
    [ 0xdaa520, 0x6495ed, 2, 2 ],
    [ 0x00ff7f, 0x556b2f, 1, 3 ],
    [ 0x800000, 0xdaa520, 1, 2 ],
    [ 0xffc0cb, 0xeee8aa, 0, 0 ],
    [ 0xf5f5dc, 0xffebcd, 0, 0 ],
    [ 0xcd5c5c, 0xffdead, 1, 2 ],
    [ 0xff69b4, 0xfa8072, 0, 1 ],
    [ 0x696969, 0xdb7093, 1, 1 ],
    [ 0x90ee90, 0xffe4b5, 1, 1 ],
    [ 0xbc8f8f, 0x9932cc, 0, 1 ],
    [ 0xf0ffff, 0x7fff00, 2, 2 ],
    [ 0xff00ff, 0x800080, 2, 2 ],
    [ 0xf0f8ff, 0xcd5c5c, 2, 2 ],
    [ 0x0000cd, 0xc71585, 1, 2 ],
    [ 0xfdf5e6, 0xee82ee, 1, 1 ],
    [ 0x008000, 0x808080, 2, 2 ],
    [ 0x00bfff, 0x98fb98, 2, 2 ],
    [ 0xd3d3d3, 0x7fffd4, 0, 1 ],
    [ 0x8b0000, 0xe9967a, 2, 3 ],
    [ 0x87ceeb, 0x40e0d0, 0, 1 ],
    [ 0xdda0dd, 0xc0c0c0, 0, 0 ],
    [ 0x7fffd4, 0x8b0000, 2, 2 ] ]


-- Test data for colorsVisitedColor.
-- First four columns were generated by callign 'shuf' UNIX tool on
-- Colors.colorsTable.
-- Fifth column is a result of calling original C function a_Color_vc() with
-- the four arguments from first four columns.
visitedColorTestData :: [[Int]]
visitedColorTestData =
  --  candidate txt       lnk       bg        expected result
  [ [ 0xf5f5f5, 0xb0c4de, 0x708090, 0xff69b4, 0x800080 ],
    [ 0xffa500, 0x87cefa, 0xcd5c5c, 0xc71585, 0xffa500 ],
    [ 0x87cefa, 0xffb6c1, 0xd3d3d3, 0x00ffff, 0x800080 ],
    [ 0xfff0f5, 0xff6347, 0x4169e1, 0x8b0000, 0xfff0f5 ],
    [ 0x90ee90, 0xadff2f, 0xff0000, 0xc0c0c0, 0x8b0000 ],
    [ 0x5f9ea0, 0x9acd32, 0xf5f5dc, 0x708090, 0x8b0000 ],
    [ 0x32cd32, 0xf08080, 0x7b68ee, 0xa9a9a9, 0x32cd32 ],
    [ 0x7cfc00, 0xfff8dc, 0xf4a460, 0xf8f8ff, 0x7cfc00 ],
    [ 0xfffaf0, 0x000000, 0x00bfff, 0x00fa9a, 0x800080 ],
    [ 0xd2b48c, 0xdda0dd, 0xd2b48c, 0xdeb887, 0x8b0000 ],
    [ 0xff00ff, 0xff69b4, 0xf5deb3, 0xfaf0e6, 0x800080 ],
    [ 0xf4a460, 0xf5f5dc, 0xafeeee, 0xcd5c5c, 0x008b8b ],
    [ 0x008080, 0xdeb887, 0x8fbc8f, 0xeee8aa, 0x008080 ],
    [ 0x40e0d0, 0xf0fff0, 0x7fff00, 0xadd8e6, 0x800080 ],
    [ 0x00bfff, 0xd8bfd8, 0xfff8dc, 0xff0000, 0x00bfff ],
    [ 0xffffff, 0xe6e6fa, 0xeee8aa, 0x8a2be2, 0x008b8b ],
    [ 0x7fff00, 0x00ced1, 0xdda0dd, 0x00ff7f, 0x7fff00 ],
    [ 0x6b8e23, 0xafeeee, 0xff6347, 0xffffff, 0x6b8e23 ],
    [ 0x2f4f4f, 0xf5deb3, 0xfff0f5, 0x008080, 0x800080 ],
    [ 0xd2691e, 0xbdb76b, 0xa52a2a, 0xff00ff, 0x008b8b ],
    [ 0x778899, 0x7b68ee, 0x0000ff, 0x66cdaa, 0x8b0000 ],
    [ 0x808000, 0xfaebd7, 0xe0ffff, 0xffebcd, 0x808000 ],
    [ 0xb0c4de, 0xffe4c4, 0x00ff00, 0x000000, 0xb0c4de ],
    [ 0xe9967a, 0xfffaf0, 0xe6e6fa, 0x483d8b, 0xe9967a ],
    [ 0xf0e68c, 0x8b4513, 0xff4500, 0x00008b, 0xf0e68c ],
    [ 0xfafad2, 0x00ff00, 0x66cdaa, 0xe0ffff, 0x800080 ],
    [ 0xffdead, 0x8b008b, 0xfffacd, 0x6495ed, 0x800080 ],
    [ 0xfa8072, 0xbc8f8f, 0xbdb76b, 0xf0ffff, 0x800080 ],
    [ 0xffe4c4, 0x808080, 0x778899, 0xffa500, 0x800080 ],
    [ 0x2f4f4f, 0xffffe0, 0x008b8b, 0xd3d3d3, 0x8b0000 ],
    [ 0x483d8b, 0x48d1cc, 0x008080, 0x8b008b, 0x808000 ],
    [ 0x556b2f, 0xfff5ee, 0xffff00, 0x228b22, 0x8b008b ],
    [ 0xfffacd, 0x40e0d0, 0x4b0082, 0xb0c4de, 0x8b0000 ],
    [ 0x800080, 0xff00ff, 0xfaf0e6, 0xff4500, 0x800080 ],
    [ 0xadd8e6, 0x87ceeb, 0xee82ee, 0xfffafa, 0x800080 ],
    [ 0xd3d3d3, 0x808080, 0xffdab9, 0x2f4f4f, 0xff7f50 ],
    [ 0xf0f8ff, 0xd3d3d3, 0x48d1cc, 0x800080, 0x808000 ],
    [ 0x8a2be2, 0x2e8b57, 0x696969, 0x20b2aa, 0x8a2be2 ],
    [ 0xf5deb3, 0x008b8b, 0xffdead, 0x7cfc00, 0x800080 ],
    [ 0xdeb887, 0x708090, 0x8a2be2, 0x778899, 0x8b0000 ],
    [ 0x1e90ff, 0xffebcd, 0xa9a9a9, 0xffb6c1, 0x800080 ],
    [ 0xfff8dc, 0x008080, 0xff00ff, 0xff1493, 0x008b8b ],
    [ 0x9acd32, 0xffe4e1, 0x9acd32, 0x006400, 0x800080 ],
    [ 0xa52a2a, 0x708090, 0xd8bfd8, 0x000080, 0x808000 ],
    [ 0xff7f50, 0x8b0000, 0xffa07a, 0x8b4513, 0x008b8b ],
    [ 0x7fffd4, 0x4b0082, 0x778899, 0x696969, 0x7fffd4 ],
    [ 0xffd700, 0xfa8072, 0x3cb371, 0xb22222, 0xffd700 ],
    [ 0x0000cd, 0x8a2be2, 0x8b008b, 0xfff0f5, 0x0000cd ],
    [ 0xa9a9a9, 0xff0000, 0x006400, 0xb8860b, 0x800080 ],
    [ 0xee82ee, 0x9932cc, 0x90ee90, 0xafeeee, 0x008b8b ],
    [ 0xe0ffff, 0xfffafa, 0x00ffff, 0xff6347, 0x800080 ],
    [ 0xf5fffa, 0xffffff, 0xffa500, 0xd2691e, 0x800080 ],
    [ 0x8b008b, 0xba55d3, 0xf0ffff, 0x808080, 0x8b0000 ],
    [ 0xf08080, 0xb8860b, 0xe9967a, 0xa9a9a9, 0x8b0000 ],
    [ 0xa0522d, 0xd3d3d3, 0xa0522d, 0x800000, 0x008b8b ],
    [ 0xffebcd, 0xff8c00, 0x00ff7f, 0x778899, 0xffebcd ],
    [ 0xfff5ee, 0x98fb98, 0xffb6c1, 0xff7f50, 0x800080 ],
    [ 0x000000, 0x00ffff, 0xa9a9a9, 0xe6e6fa, 0x000000 ],
    [ 0xfaf0e6, 0xda70d6, 0xfff5ee, 0xffe4e1, 0x800080 ],
    [ 0xda70d6, 0xdb7093, 0xfffff0, 0xfffacd, 0x800080 ],
    [ 0x9400d3, 0x9400d3, 0xb22222, 0x48d1cc, 0xff7f50 ],
    [ 0x3cb371, 0xdaa520, 0x20b2aa, 0xffefd5, 0x800080 ],
    [ 0x6a5acd, 0x2f4f4f, 0xffe4e1, 0xffffe0, 0x6a5acd ],
    [ 0xcd5c5c, 0xadd8e6, 0x87ceeb, 0xe9967a, 0x800080 ],
    [ 0x20b2aa, 0xf0ffff, 0xdc1436, 0xdc1436, 0x20b2aa ],
    [ 0xffdab9, 0x00ff7f, 0xffe4b5, 0xf5f5dc, 0x800080 ],
    [ 0x00ffff, 0x3cb371, 0x000080, 0xda70d6, 0x00ffff ],
    [ 0xdb7093, 0xffa500, 0x6a5acd, 0xf0f8ff, 0xdb7093 ],
    [ 0xcd853f, 0x20b2aa, 0xfffafa, 0x008b8b, 0x800080 ],
    [ 0xff0000, 0x800080, 0x98fb98, 0xf5fffa, 0xff0000 ],
    [ 0xf0fff0, 0xd2691e, 0x696969, 0xdcdcdc, 0x8b0000 ],
    [ 0xf8f8ff, 0xf4a460, 0x00ffff, 0xfff5ee, 0x800080 ],
    [ 0xfaebd7, 0x4682b4, 0xf08080, 0x4169e1, 0xfaebd7 ],
    [ 0x0000ff, 0xf5f5f5, 0x5f9ea0, 0x8fbc8f, 0x0000ff ],
    [ 0xfffafa, 0x2f4f4f, 0x191970, 0xffc0cb, 0x808000 ],
    [ 0x808080, 0x9370db, 0x6495ed, 0xd3d3d3, 0x8b0000 ],
    [ 0xf0ffff, 0x7fffd4, 0xadd8e6, 0x98fb98, 0x008b8b ],
    [ 0xadff2f, 0xfdf5e6, 0xc0c0c0, 0x696969, 0x8b0000 ],
    [ 0xffa07a, 0x00ffff, 0x00008b, 0x00ced1, 0x800080 ],
    [ 0xffb6c1, 0xff7f50, 0x2f4f4f, 0xee82ee, 0x800080 ],
    [ 0xbdb76b, 0xc71585, 0xdeb887, 0x4682b4, 0x8b0000 ],
    [ 0x66cdaa, 0x006400, 0x808000, 0x7b68ee, 0x800080 ],
    [ 0x00ffff, 0x6a5acd, 0x8b0000, 0x808000, 0x00ffff ],
    [ 0xfffff0, 0x808000, 0xffffe0, 0xffdead, 0x800080 ],
    [ 0xff1493, 0x228b22, 0x00ced1, 0xff8c00, 0xff1493 ],
    [ 0x98fb98, 0xcd5c5c, 0xf0fff0, 0xbdb76b, 0x8b0000 ],
    [ 0xff4500, 0x00bfff, 0x800000, 0xb0e0e6, 0xff4500 ],
    [ 0xdc1436, 0xe9967a, 0xffefd5, 0x191970, 0x808000 ],
    [ 0x9370db, 0x0000ff, 0xf0f8ff, 0xf5f5f5, 0x9370db ],
    [ 0xffe4b5, 0xfffacd, 0xba55d3, 0xf0fff0, 0x800080 ],
    [ 0xc0c0c0, 0xffa07a, 0xbc8f8f, 0x9acd32, 0x000000 ],
    [ 0x8fbc8f, 0xff4500, 0xf5fffa, 0xfafad2, 0x800080 ],
    [ 0x4169e1, 0xff00ff, 0xdb7093, 0xbc8f8f, 0x8b0000 ],
    [ 0x00008b, 0x00fa9a, 0xff00ff, 0x7fff00, 0x00008b ],
    [ 0xffff00, 0x8fbc8f, 0x2f4f4f, 0xffe4c4, 0x8b0000 ],
    [ 0xffe4e1, 0xc0c0c0, 0x0000cd, 0xdaa520, 0x800080 ],
    [ 0xdaa520, 0xf5fffa, 0xb0e0e6, 0xa52a2a, 0x008b8b ],
    [ 0x6495ed, 0x483d8b, 0xffc0cb, 0x00ff00, 0x6495ed ],
    [ 0xd3d3d3, 0x5f9ea0, 0xfdf5e6, 0x708090, 0x8b0000 ],
    [ 0x708090, 0x778899, 0x000000, 0xf5deb3, 0x800080 ],
    [ 0x000080, 0xff1493, 0x6b8e23, 0x87cefa, 0x000080 ],
    [ 0xb0e0e6, 0xffd700, 0xffffff, 0xffe4b5, 0x800080 ],
    [ 0xd8bfd8, 0xffdab9, 0xd3d3d3, 0x008000, 0x800080 ],
    [ 0x008000, 0xd2b48c, 0x008000, 0xdb7093, 0x8b0000 ],
    [ 0xffefd5, 0xfffff0, 0x808080, 0xff00ff, 0x800080 ],
    [ 0x008b8b, 0x6495ed, 0x800080, 0xf0e68c, 0x808000 ],
    [ 0x8b4513, 0x6b8e23, 0xfaebd7, 0xdda0dd, 0x8b0000 ],
    [ 0xfdf5e6, 0x696969, 0x32cd32, 0xd8bfd8, 0x8b0000 ],
    [ 0x8b0000, 0x1e90ff, 0x7fffd4, 0x556b2f, 0x8b0000 ],
    [ 0x4682b4, 0xee82ee, 0xadff2f, 0x0000cd, 0x808000 ],
    [ 0xdda0dd, 0xf8f8ff, 0xff69b4, 0xa0522d, 0x008b8b ],
    [ 0xff8c00, 0xfaf0e6, 0xff1493, 0x4b0082, 0xff8c00 ],
    [ 0xba55d3, 0xb0e0e6, 0x9370db, 0x6b8e23, 0x008b8b ],
    [ 0x87ceeb, 0x66cdaa, 0xffe4c4, 0xffdab9, 0x800080 ],
    [ 0xbc8f8f, 0xcd853f, 0x9400d3, 0x9370db, 0x8b0000 ],
    [ 0xafeeee, 0x000080, 0xfa8072, 0x2f4f4f, 0xafeeee ],
    [ 0xc71585, 0xa52a2a, 0xdaa520, 0x00ffff, 0xc71585 ],
    [ 0x228b22, 0xf0f8ff, 0x87cefa, 0x00bfff, 0x800080 ],
    [ 0xb22222, 0xeee8aa, 0x228b22, 0x9400d3, 0x008b8b ],
    [ 0x708090, 0xb22222, 0x708090, 0x0000ff, 0x008b8b ],
    [ 0x4b0082, 0xffc0cb, 0xffebcd, 0xadff2f, 0x4b0082 ],
    [ 0xff6347, 0x00008b, 0xf8f8ff, 0xfaebd7, 0xff6347 ],
    [ 0x800000, 0xf0e68c, 0xf0e68c, 0x32cd32, 0x800000 ],
    [ 0x808080, 0xffefd5, 0xfafad2, 0x7fffd4, 0x008b8b ],
    [ 0x00fa9a, 0x7fff00, 0x4682b4, 0x40e0d0, 0x808000 ],
    [ 0xdcdcdc, 0x800000, 0xc71585, 0xcd853f, 0x000000 ],
    [ 0xa9a9a9, 0xfafad2, 0xffd700, 0xfdf5e6, 0x800080 ],
    [ 0x7b68ee, 0xffdead, 0x808080, 0xfa8072, 0x7b68ee ],
    [ 0x00ced1, 0x008000, 0x9932cc, 0xd2b48c, 0x8b0000 ],
    [ 0x9932cc, 0xdc1436, 0xff8c00, 0xf08080, 0x800080 ],
    [ 0xff69b4, 0xfff0f5, 0xfffaf0, 0x808080, 0x8b0000 ],
    [ 0xffffe0, 0x778899, 0x1e90ff, 0x90ee90, 0x008b8b ],
    [ 0x00ff00, 0xa9a9a9, 0x556b2f, 0xffff00, 0x800080 ],
    [ 0x191970, 0x556b2f, 0xb0c4de, 0x5f9ea0, 0x8b0000 ],
    [ 0x2e8b57, 0xa9a9a9, 0xdcdcdc, 0xfffaf0, 0x2e8b57 ],
    [ 0x696969, 0x32cd32, 0xcd853f, 0x3cb371, 0x8b0000 ],
    [ 0x00ff7f, 0x696969, 0xb8860b, 0x1e90ff, 0x00ff7f ],
    [ 0x006400, 0x4169e1, 0xd2691e, 0x9932cc, 0x006400 ],
    [ 0xf5f5dc, 0xe0ffff, 0x483d8b, 0xfff8dc, 0x008b8b ],
    [ 0x778899, 0xffff00, 0x8b4513, 0xf4a460, 0x800080 ],
    [ 0x696969, 0x7cfc00, 0xda70d6, 0xffa07a, 0x800080 ],
    [ 0xffc0cb, 0xdcdcdc, 0x00fa9a, 0xfffff0, 0x800080 ],
    [ 0xe6e6fa, 0xffe4b5, 0x40e0d0, 0x2e8b57, 0xe6e6fa ],
    [ 0xeee8aa, 0x0000cd, 0xff7f50, 0xba55d3, 0x000000 ],
    [ 0xff00ff, 0x90ee90, 0x2e8b57, 0x87ceeb, 0xff00ff ],
    [ 0xb8860b, 0xa0522d, 0x7cfc00, 0xffd700, 0x800080 ],
    [ 0x48d1cc, 0x191970, 0xf5f5f5, 0x6a5acd, 0xff7f50 ] ]




-- Pass distanceXTestData list to this function.
--
-- On success return empty string.
-- On failure return string representation of 'c1' integer in a row,
-- for which test failed.
--
-- distFun function calculating distance between colors
-- expectedIndex index of column in which 'expected' value for given distFun is
-- cs test data
distanceXTest :: (Int -> Int -> Int) -> Int -> [[Int]] -> String
distanceXTest _       _ []                 = ""
distanceXTest distFun expectedIndex (c:cs) = if expected /= distFun c1 c2
                                             then c1Str
                                             else distanceXTest distFun expectedIndex cs
  where
    c1       = c !! 0
    c2       = c !! 1
    expected = c !! expectedIndex
    c1Str    = showHex c1 ""




-- Pass visitedColorTestData list to this function.
--
-- On success return empty string.
-- On failure return string representation of 'candidate' integer in a row,
-- for which test failed.
visitedColorTest :: [[Int]] -> String
visitedColorTest []     = ""
visitedColorTest (c:cs) = if expected /= colorsVisitedColor candidate txt lnk bg
                          then candidateStr
                          else visitedColorTest cs
  where
    candidate    = c !! 0
    txt          = c !! 1
    lnk          = c !! 2
    bg           = c !! 3
    expected     = c !! 4
    candidateStr = showHex candidate ""




colorsTestCases :: [Test]
colorsTestCases = [
  -- If some error is found, test function returns non-empty string with
  -- representation of integer from first column in a row, for which the test
  -- failed.
  --
  -- These three tests are mostly (only?) to ensure that Haskell
  -- implementations behave in the same way as original C implementations.
    TestCase (do
                 assertEqual "distance 2" "" (distanceXTest colorsDistance2 2 distanceXTestData))
  , TestCase (do
                 assertEqual "distance 3" "" (distanceXTest colorsDistance3 3 distanceXTestData))
  , TestCase (do
                 assertEqual "visited colors" "" (visitedColorTest visitedColorTestData))


  -- Plain color name to integer. Use colors with similar names.
  , TestCase (assertEqual "c_01" (Just 0xff0000) (colorsStringToColor "red"))
  , TestCase (assertEqual "c_02" (Just 0x8b0000) (colorsStringToColor "darkred"))
  , TestCase (assertEqual "c_03" (Just 0xcd5c5c) (colorsStringToColor "indianred"))
  , TestCase (assertEqual "c_04" (Just 0x4b0082) (colorsStringToColor "indigo"))
  , TestCase (assertEqual "c_05" (Just 0x008000) (colorsStringToColor "green"))
  , TestCase (assertEqual "c_06" (Just 0xadff2f) (colorsStringToColor "greenyellow"))

  -- Plain color name to integer. Use different cases.
  , TestCase (assertEqual "c_10" (Just 0xff0000) (colorsStringToColor "Red"))
  , TestCase (assertEqual "c_11" (Just 0x4b0082) (colorsStringToColor "indigO"))
  , TestCase (assertEqual "c_12" (Just 0x008000) (colorsStringToColor "greEn"))

  -- Plain color name to integer. Use colors from beginning and end of array.
  , TestCase (assertEqual "c_21" (Just 0xf0f8ff) (colorsStringToColor "aliceblue"))
  , TestCase (assertEqual "c_22" (Just 0xfaebd7) (colorsStringToColor "antiquewhite"))
  , TestCase (assertEqual "c_23" (Just 0xffff00) (colorsStringToColor "yellow"))
  , TestCase (assertEqual "c_24" (Just 0x9acd32) (colorsStringToColor "yellowgreen"))

  -- Plain color name to integer. Mistyped names.
  , TestCase (assertEqual "c_30" Nothing (colorsStringToColor "redd"))
  , TestCase (assertEqual "c_31" Nothing (colorsStringToColor "dark red"))
  , TestCase (assertEqual "c_32" Nothing (colorsStringToColor "indjanred"))
  , TestCase (assertEqual "c_33" Nothing (colorsStringToColor "indi-go"))
  , TestCase (assertEqual "c_34" Nothing (colorsStringToColor "redreen"))
  , TestCase (assertEqual "c_35" Nothing (colorsStringToColor "grenyellow"))


  -- TODO: large parts of the testcases below could be generated by
  -- QuickCheck. I was lazy and started copy-pasting the cases with string
  -- names from above, but why not generate the texts automatically?


  -- #RRGGBB or 0xRRGGBB format. Or RRGGBB as a fallback
  , TestCase (assertEqual "c_40" (Just 0xff0000) (colorsStringToColor "0XfF0000"))
  , TestCase (assertEqual "c_41" (Just 0x8b0000) (colorsStringToColor "0x8b0000"))
  , TestCase (assertEqual "c_42" (Just 0xcd5c5c) (colorsStringToColor "0xcd5c5C"))

  , TestCase (assertEqual "c_43" (Just 0x4b0082) (colorsStringToColor "#4b0082"))
  , TestCase (assertEqual "c_44" (Just 0x008000) (colorsStringToColor "#008000"))
  , TestCase (assertEqual "c_45" (Just 0xadff2f) (colorsStringToColor "#Adff2f"))

  , TestCase (assertEqual "c_46" (Just 0xf0f8ff) (colorsStringToColor "f0f8ff"))
  , TestCase (assertEqual "c_47" (Just 0xfaebd7) (colorsStringToColor "FAEbD7"))
  , TestCase (assertEqual "c_48" (Just 0x9acd32) (colorsStringToColor "9acd32"))


  -- #RGB or 0xRGB format. Or RGB as a fallback
  , TestCase (assertEqual "c_50" (Just 0xaa0000) (colorsStringToColor "0xA00"))
  , TestCase (assertEqual "c_51" (Just 0x00bbcc) (colorsStringToColor "0X0bC"))
  , TestCase (assertEqual "c_52" (Just 0x1100dd) (colorsStringToColor "0x10d"))

  , TestCase (assertEqual "c_53" (Just 0x330000) (colorsStringToColor "#300"))
  , TestCase (assertEqual "c_54" (Just 0x0099dd) (colorsStringToColor "#09d"))
  , TestCase (assertEqual "c_55" (Just 0xee00aa) (colorsStringToColor "#E0a"))

  , TestCase (assertEqual "c_56" (Just 0xcc0088) (colorsStringToColor "c08"))
  , TestCase (assertEqual "c_57" (Just 0x00ee33) (colorsStringToColor "0E3"))
  , TestCase (assertEqual "c_58" (Just 0x770011) (colorsStringToColor "701"))


  -- colorsStringToColorWithDefault: valid inputs
  , TestCase (assertEqual "c_60" 0x0000ff (colorsStringToColorWithDefault "blue"     0x000001))
  , TestCase (assertEqual "c_61" 0xffa500 (colorsStringToColorWithDefault "ORangE"   0x000001))

  , TestCase (assertEqual "c_62" 0x991ced (colorsStringToColorWithDefault "#991cED"  0x000001))
  , TestCase (assertEqual "c_63" 0x55ff33 (colorsStringToColorWithDefault "#5f3"     0x000001))

  , TestCase (assertEqual "c_64" 0xace000 (colorsStringToColorWithDefault "0xacE000" 0x000001))
  , TestCase (assertEqual "c_65" 0xffee77 (colorsStringToColorWithDefault "0XfE7"    0x000001))

  , TestCase (assertEqual "c_66" 0xffff11 (colorsStringToColorWithDefault "FF1"      0x000001))
  , TestCase (assertEqual "c_67" 0x0022cc (colorsStringToColorWithDefault "02c"      0x000001))


  -- colorsStringToColorWithDefault: invalid inputs, leading to returning a fallback value.
  , TestCase (assertEqual "c_70" 0x000001 (colorsStringToColorWithDefault "strange"  0x000001))
  , TestCase (assertEqual "c_71" 0x000001 (colorsStringToColorWithDefault "oval"     0x000001))

  , TestCase (assertEqual "c_72" 0x000001 (colorsStringToColorWithDefault "#21"      0x000001))
  , TestCase (assertEqual "c_73" 0x000001 (colorsStringToColorWithDefault "#21z"     0x000001))

  , TestCase (assertEqual "c_74" 0x000001 (colorsStringToColorWithDefault "0x21"     0x000001))
  , TestCase (assertEqual "c_75" 0x000001 (colorsStringToColorWithDefault "0x21z"    0x000001))

  , TestCase (assertEqual "c_76" 0x000001 (colorsStringToColorWithDefault "[0a"      0x000001))
  , TestCase (assertEqual "c_77" 0x000001 (colorsStringToColorWithDefault "???"      0x000001))

  ]





testsColors :: IO String
testsColors = do
  testCounts <- runTestTT (TestList colorsTestCases)
  if errors testCounts + failures testCounts == 0
    then return ""
    else return "[EE] testsColors failed"

