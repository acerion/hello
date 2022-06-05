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

#include <sys/time.h>
#include <stdio.h>
#include "../dlib/dlib.h"
#include "msg.h"
#include "css.hh"





c_css_declaration_set_t * declarationListNew(void)
{
   c_css_declaration_set_t * set = (c_css_declaration_set_t *) calloc(1, sizeof (c_css_declaration_set_t));
   set->c_is_safe = true;
   for (int i = 0; i < DECLARATIONS_COUNT_IN_SET; i++) {
      set->c_declarations[i] = (c_css_declaration_t *) calloc(1, sizeof (c_css_declaration_t));
      set->c_declarations[i]->c_value = (c_css_value_t *) calloc(1, sizeof (c_css_value_t));
   }

   return set;
}

c_css_declaration_set_t * declarationListNew(const c_css_declaration_set_t * inDeclList)
{
   c_css_declaration_set_t * out = declarationListNew();

   memcpy(out->c_declarations, inDeclList->c_declarations, DECLARATIONS_COUNT_IN_SET * sizeof (c_css_declaration_set_t));
   out->c_is_safe = inDeclList->c_is_safe;

   for (int i = 0; i < out->c_declarations_size; i++) {
      c_css_declaration_t * decl = out->c_declarations[i];
      switch (decl->c_value->c_type_tag) {
      case CssDeclarationValueTypeSTRING:
      case CssDeclarationValueTypeSYMBOL:
         decl->c_value->c_text_val = dStrdup (decl->c_value->c_text_val);
         break;
      default:
         break;
      }
   }

   return out;
}




CssLengthType cpp_cssLengthType(CssLength len)
{
   return (CssLengthType) (len.length_bits & 0x07);
}




float cpp_cssLengthValue(CssLength l)
{
   switch (cpp_cssLengthType(l)) {
   case CSS_LENGTH_TYPE_PX:
      return (float) (l.length_bits >> 3);
   case CSS_LENGTH_TYPE_NONE:
   case CSS_LENGTH_TYPE_MM:
   case CSS_LENGTH_TYPE_EM:
   case CSS_LENGTH_TYPE_EX:
   case CSS_LENGTH_TYPE_PERCENTAGE:
   case CSS_LENGTH_TYPE_RELATIVE:
      return  ((float)(l.length_bits & ~7)) / (1 << 15);
   case CSS_LENGTH_TYPE_AUTO:
      return 0.0;
   default:
      assert(false);
      return 0.0;
   }
}



CssLength cpp_cssCreateLength(float v, CssLengthType t)
{
   static const int CSS_LENGTH_FRAC_MAX = (1 << (32 - 15 - 1)) - 1;
   static const int CSS_LENGTH_INT_MAX = (1 << (32 - 4)) - 1;
   int iv;
   CssLength retv = {};

   switch (t) {
   case CSS_LENGTH_TYPE_PX:
      iv = lout::misc::roundInt(v);
      if (iv > CSS_LENGTH_INT_MAX)
         iv = CSS_LENGTH_INT_MAX;
      else if (iv < -CSS_LENGTH_INT_MAX)
         iv = -CSS_LENGTH_INT_MAX;

      retv.length_bits = iv << 3 | t;
      return retv;

   case CSS_LENGTH_TYPE_NONE:
   case CSS_LENGTH_TYPE_MM:
   case CSS_LENGTH_TYPE_EM:
   case CSS_LENGTH_TYPE_EX:
   case CSS_LENGTH_TYPE_PERCENTAGE:
   case CSS_LENGTH_TYPE_RELATIVE:
      if (v > CSS_LENGTH_FRAC_MAX)
         v = CSS_LENGTH_FRAC_MAX;
      else if (v < -CSS_LENGTH_FRAC_MAX)
         v = -CSS_LENGTH_FRAC_MAX;

      retv.length_bits = ((int) (v * (1 << 15)) & ~7 ) | t;
      return retv;

   case CSS_LENGTH_TYPE_AUTO:
      retv.length_bits = t;
      return retv;

   default:
      assert(false);
      retv.length_bits = CSS_LENGTH_TYPE_AUTO;
      return retv;
   }
}

