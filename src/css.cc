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




