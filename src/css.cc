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
#include "html_common.hh"
#include "css.hh"

using namespace dw::core::style;


static void alloc_rules_list(c_css_rules_list_t ** list);
static void alloc_rules_map(c_css_rules_map_t ** map);
static void alloc_sheet(c_css_style_sheet_t ** sheet);




static void css_value_copy(c_css_value_t * dest, c_css_value_t * src);




static c_css_style_sheet_t * g_user_agent_sheet;
static bool g_user_agent_sheet_initialized;




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

static void alloc_rules_map(c_css_rules_map_t ** map)
{
   (*map) = (c_css_rules_map_t *) calloc(1, sizeof (c_css_rules_map_t));
   for (int r = 0; r < RULES_MAP_SIZE; r++) {
      alloc_rules_list(&(*map)->c_rules_lists[r]);
   }
}

static void alloc_rules_list(c_css_rules_list_t ** list)
{
   static unsigned long size = 0;
   (*list) = (c_css_rules_list_t *) calloc(1, sizeof (c_css_rules_list_t));
   for (int r = 0; r < RULES_LIST_SIZE; r++) {
      (*list)->c_rules[r] = (c_css_rule_t *) calloc(1, sizeof (c_css_rule_t));
      (*list)->c_rules[r]->c_cached_complex_selector = (c_css_cached_complex_selector_t *) calloc(1, sizeof (c_css_cached_complex_selector_t));
      size += sizeof (c_css_cached_complex_selector_t);
   }

#if 0
   fprintf(stderr, "size = %lu MB (%d * %d)\n",
           size / (1024 * 1024),
           RULES_LIST_SIZE, sizeof (c_css_cached_complex_selector_t));
   if (size > 1000 * 1024 * 1024) {
      exit(0);
   }
#endif
}

static void alloc_sheet(c_css_style_sheet_t ** sheet)
{
   (*sheet) = (c_css_style_sheet_t *) calloc(1, sizeof (c_css_style_sheet_t));

   alloc_rules_map(&(*sheet)->c_rules_by_id);
   alloc_rules_map(&(*sheet)->c_rules_by_class);

   for (int j = 0; j < css_style_sheet_n_tags; j++) {
      alloc_rules_list(&(*sheet)->c_rules_by_type[j]);
   }

   alloc_rules_list(&(*sheet)->c_rules_by_any_element);
}

c_css_context_t * c_css_context_new(void)
{
   c_css_context_t * context = (c_css_context_t *) calloc(1, sizeof (c_css_context_t));

   context->c_rule_position = 0;

   memset(context->c_sheets, 0, sizeof (context->c_sheets));
   for (int order = 0; order < CSS_PRIMARY_ORDER_SIZE; order++) {
      if (CSS_PRIMARY_USER_AGENT == order) {
         if (!g_user_agent_sheet_initialized) {
            alloc_sheet(&context->c_sheets[order]);
            g_user_agent_sheet = context->c_sheets[order];
            g_user_agent_sheet_initialized = true;
         } else {
            /* Each context shares the same, single user agent sheet. */
            context->c_sheets[order] = g_user_agent_sheet;
         }
      } else {
         alloc_sheet(&context->c_sheets[order]);
      }
   }

   context->c_match_cache = (c_css_match_cache_t *) calloc(1, sizeof (c_css_match_cache_t));
   hll_matchCacheSetSize(context->c_match_cache, context->c_sheets[CSS_PRIMARY_USER_AGENT]->c_required_match_cache); // Initially the size is zero.

   return context;
}

/**
 * \brief Apply a CSS context to a property list.
 *
 * The stylesheets in the context are applied one after the other
 * in the ordering defined by CSS 2.1.
 * Stylesheets that are applied later can overwrite properties set
 * by previous stylesheets.
 * This allows e.g. user styles to overwrite author styles.
 */
void css_context_apply_css_context(c_css_context_t * context,
                                   c_css_declaration_set_t * mergedDeclList,
                                   int doc_tree_ref,
                                   c_doctree_node_t * dtn,
                                   c_css_declaration_lists_t * declLists) {

   static struct timeval accumulated;
   static bool initialized = false;
   if (!initialized) {
      timerclear(&accumulated);
      initialized = true;
   }

   struct timeval start;
   timerclear(&start);
   gettimeofday(&start, NULL);


   hll_cssStyleSheetApplyStyleSheet(context->c_sheets[CSS_PRIMARY_USER_AGENT], mergedDeclList, doc_tree_ref, dtn, context->c_match_cache);

   hll_cssStyleSheetApplyStyleSheet(context->c_sheets[CSS_PRIMARY_USER], mergedDeclList, doc_tree_ref, dtn, context->c_match_cache);

   if (declLists->nonCss)
      hll_declarationListAppend(mergedDeclList, declLists->nonCss);

   hll_cssStyleSheetApplyStyleSheet(context->c_sheets[CSS_PRIMARY_AUTHOR], mergedDeclList, doc_tree_ref, dtn, context->c_match_cache);

   if (declLists->main) {
      hll_declarationListAppend(mergedDeclList, declLists->main);
   }

   hll_cssStyleSheetApplyStyleSheet(context->c_sheets[CSS_PRIMARY_AUTHOR_IMPORTANT], mergedDeclList, doc_tree_ref, dtn, context->c_match_cache);

   if (declLists->important) {
      hll_declarationListAppend(mergedDeclList, declLists->important);
   }

   hll_cssStyleSheetApplyStyleSheet(context->c_sheets[CSS_PRIMARY_USER_IMPORTANT], mergedDeclList, doc_tree_ref, dtn, context->c_match_cache);

   struct timeval stop;
   timerclear(&stop);
   gettimeofday(&stop, NULL);

   struct timeval diff;
   timerclear(&diff);
   timersub(&stop, &start, &diff);

   struct timeval old_accumulated = accumulated;
   timeradd(&diff, &old_accumulated, &accumulated);

   //fprintf(stderr, "MEAS: TIME: %ld.%06ld seconds\n", diff.tv_sec, diff.tv_usec);
   //fprintf(stderr, "MEAS: ACCUMULATED TIME: %ld.%02ld seconds\n", accumulated.tv_sec, accumulated.tv_usec / (1000 * 10));
}


