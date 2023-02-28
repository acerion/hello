/*
 * File: css.cc
 *
 * Copyright 2008-2014 Johannes Hofmann <Johannes.Hofmann@gmx.de>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 */

#include "css.hh"

CssLengthType cpp_cssLengthType(CssLength len)
{
   return (CssLengthType) len.length_type;
}

float cpp_cssLengthValue(CssLength l)
{
   return (float) l.length_value;
}

CssLength cpp_cssCreateLength(float v, CssLengthType t)
{
   CssLength retv = { .length_value = (double) v, .length_type = (int) t };
   return retv;
}

