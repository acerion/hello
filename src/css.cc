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

#include <stdio.h>
#include "../dlib/dlib.h"
#include "msg.h"
#include "html_common.hh"
#include "css.hh"

using namespace dw::core::style;




static void match_cache_set_size(c_css_match_cache_t * match_cache, int new_size);




/* c_css_selector_t methods. */
static bool css_selector_matches(c_css_selector_t * selector, Doctree * dt, const c_doctree_node_t * dtn, int sim_sel_idx, Combinator comb, c_css_match_cache_t * match_cache);
static int css_selector_get_required_match_cache(c_css_selector_t * selector);
static bool css_selector_has_pseudo_class(c_css_selector_t * selector);
static void css_selector_print(FILE * file, c_css_selector_t * selector);




static void css_value_copy(c_css_value_t * dest, c_css_value_t * src);
static bool on_combinator_descendant(c_css_selector_t * selector, Doctree * docTree, const c_doctree_node_t * dtn, int sim_sel_idx, Combinator comb, c_css_match_cache_t * match_cache);

static void css_rule_print_pretty(FILE * file, const c_css_rule_t * rule);

static void css_simple_selector_print_compact(FILE * file, const c_css_simple_selector_t * sim_sel);
static void css_simple_selector_print_pretty(FILE * file, c_css_simple_selector_t * selector);

static void css_selector_print_compact(FILE * file, const c_css_selector_t * selector);
static void css_selector_print_pretty(FILE * file, c_css_selector_t * selector);

static void css_declaration_print_pretty(FILE * file, c_css_declaration_t * declaration);

static void css_declaration_set_print_pretty(FILE * file, c_css_declaration_set_t * decl_set);




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

void css_declaration_set_add_or_update_declaration(c_css_declaration_set_t * decl_set, CssDeclarationProperty property, c_css_value_t value)
{
   c_css_declaration_t * decl = new c_css_declaration_t;
   decl->c_property = property;
   decl->c_value = (c_css_value_t *) calloc(1, sizeof (c_css_value_t));
   css_value_copy(decl->c_value, &value);

   hll_declarationListAddOrUpdateDeclaration(decl_set, decl);
}


void css_value_copy(c_css_value_t * dest, c_css_value_t * src)
{
   dest->c_type_tag = src->c_type_tag;
   dest->c_int_val  = src->c_int_val;
   dest->c_bg_pos_x = src->c_bg_pos_x;
   dest->c_bg_pos_y = src->c_bg_pos_y;
   if (src->c_type_tag == CssDeclarationValueTypeSTRING || src->c_type_tag == CssDeclarationValueTypeSYMBOL) {
      dest->c_text_val = strdup(src->c_text_val);
   }
}


/**
 * \brief Return whether selector matches at a given node in the document tree.
 */
bool css_selector_matches(c_css_selector_t * selector, Doctree * docTree, const c_doctree_node_t * dtn, int sim_sel_idx, Combinator comb, c_css_match_cache_t * match_cache)
{
   if (sim_sel_idx < 0) {
      return true;
   }

   switch (comb) {
      case CssSelectorCombinatorNone:
         break;
      case CssSelectorCombinatorChild:
         dtn = docTree->parent(dtn);
         break;
      case CssSelectorCombinatorAdjacentSibling:
         dtn = docTree->sibling(dtn);
         break;
      case CssSelectorCombinatorDescendant:
         dtn = docTree->parent(dtn);
         return on_combinator_descendant(selector, docTree, dtn, sim_sel_idx, comb, match_cache);
      default:
         return false; // \todo implement other combinators
   }

   struct c_css_simple_selector_t * sim_sel = selector->c_simple_selector_list[sim_sel_idx];
   if (!dtn) {
      return false;
   }
   if (!hll_simpleSelectorMatches(sim_sel, dtn)) {
      return false;
   }

   // tail recursion should be optimized by the compiler
   return css_selector_matches(selector, docTree, dtn, sim_sel_idx - 1, (Combinator) sim_sel->c_combinator, match_cache);
}

bool on_combinator_descendant(c_css_selector_t * selector, Doctree * docTree, const c_doctree_node_t * dtn, int sim_sel_idx, Combinator comb, c_css_match_cache_t * match_cache)
{
   int * match_cache_entry = &match_cache->c_cache_items[selector->c_match_case_offset + sim_sel_idx];
   struct c_css_simple_selector_t * sim_sel = selector->c_simple_selector_list[sim_sel_idx];

   for (const c_doctree_node_t * dtn2 = dtn; dtn2 && dtn2->c_unique_num > *match_cache_entry; dtn2 = docTree->parent(dtn2)) {
      if (hll_simpleSelectorMatches(sim_sel, dtn2)
          && css_selector_matches(selector, docTree, dtn2, sim_sel_idx - 1, (Combinator) sim_sel->c_combinator, match_cache)) {
         return true;
      }
   }

   if (dtn) { // remember that it didn't match to avoid future tests
      *match_cache_entry = dtn->c_unique_num;
   }

   return false;
}

bool css_selector_has_pseudo_class(c_css_selector_t * selector)
{
   for (int i = 0; i < selector->c_simple_selector_list_size; i++)
      if (selector->c_simple_selector_list[i]->c_selector_pseudo_class_size > 0) // Remember that C/C++ code can use only first pseudo class
         return true;
   return false;
}

// Implemented as getRequiredMatchCache in CssParser.hs
int css_selector_get_required_match_cache(c_css_selector_t * selector)
{
   return selector->c_match_case_offset + selector->c_simple_selector_list_size;
}

c_css_rule_t * css_rule_new(c_css_selector_t * selector, c_css_declaration_set_t * decl_set, int rule_position)
{
   assert (selector->c_simple_selector_list_size > 0);

   c_css_rule_t * rule = (c_css_rule_t *) calloc(1, sizeof (c_css_rule_t));

   //css_selector_print_compact(stderr, selector);

   rule->c_selector = selector;
   rule->c_decl_set = decl_set;
   rule->c_position = rule_position;
   rule->c_specificity = hll_selectorSpecificity(selector);

   //fprintf(stderr, "\n\n\n");

   return rule;
}

bool css_rule_is_safe(const c_css_rule_t * rule)
{
   return !css_selector_has_pseudo_class(rule->c_selector) || rule->c_decl_set->c_is_safe;
}

/**
 * \brief Apply a stylesheet to a list of declarations.
 *
 * The declarations (list property+value) are set as defined by the rules in
 * the stylesheet that match at the given node in the document tree.
 */
void css_style_sheet_apply_style_sheet(c_css_style_sheet_t * style_sheet, FILE * file, c_css_declaration_set_t * decl_set, Doctree *docTree, const c_doctree_node_t *dtn, c_css_match_cache_t * match_cache)
{
   static const int maxLists = 32;
   const c_css_rules_list_t * rules_lists[maxLists];
   int numLists = 0;
   int index[maxLists] = {0};

   if (dtn->c_element_selector_id) {
      rules_lists[numLists] = hll_rulesMapGetList(style_sheet->c_id_rules, dtn->c_element_selector_id);
      if (rules_lists[numLists]) {
         numLists++;
      }
   }

   for (int i = 0; i < dtn->c_element_selector_class_size; i++) {
      if (i >= maxLists - 4) {
         MSG_WARN("Maximum number of classes per element exceeded.\n");
         break;
      }

      rules_lists[numLists] = hll_rulesMapGetList(style_sheet->c_class_rules, dtn->c_element_selector_class[i]);
      if (rules_lists[numLists]) {
         numLists++;
      }
   }

   rules_lists[numLists] = style_sheet->c_element_rules[dtn->c_html_element_idx];
   if (rules_lists[numLists])
      numLists++;

   rules_lists[numLists] = style_sheet->c_any_element_rules;
   if (rules_lists[numLists])
      numLists++;

   // Apply potentially matching rules from rules_lists[0-numLists] with
   // ascending specificity.
   // If specificity is equal, rules are applied in order of appearance.
   //  Each rules_list is sorted already.
   while (true) {
      int minSpec = 1 << 30;
      int minPos = 1 << 30;
      int minSpecIndex = -1;

      for (int i = 0; i < numLists; i++) {
         const c_css_rules_list_t *rl = rules_lists[i];

         if (rl && rl->c_rules_size > index[i] &&
            (rl->c_rules[index[i]]->c_specificity < minSpec ||
             (rl->c_rules[index[i]]->c_specificity == minSpec &&
              rl->c_rules[index[i]]->c_position < minPos))) {

            minSpec = rl->c_rules[index[i]]->c_specificity;
            minPos = rl->c_rules[index[i]]->c_position;
            minSpecIndex = i;
         }
      }

      if (minSpecIndex >= 0) {
         c_css_rule_t * rule = rules_lists[minSpecIndex]->c_rules[index[minSpecIndex]];

         /* Apply CSS rule. */
         if (css_selector_matches(rule->c_selector, docTree, dtn, rule->c_selector->c_simple_selector_list_size - 1, CssSelectorCombinatorNone, match_cache)) {
            hll_declarationListAppend(decl_set, rule->c_decl_set);
         }

         css_rule_print_pretty(file, rule);

         index[minSpecIndex]++;
      } else {
         break;
      }
   }
}



void alloc_sheet(c_css_style_sheet_t ** sheet)
{
   (*sheet) = (c_css_style_sheet_t *) calloc(1, sizeof (c_css_style_sheet_t));

   (*sheet)->c_id_rules =  (c_css_rules_map_t *) calloc(1, sizeof (c_css_rules_map_t));
   (*sheet)->c_class_rules = (c_css_rules_map_t *) calloc(1, sizeof (c_css_rules_map_t));
   for (int j = 0; j < css_style_sheet_n_tags; j++) {
      (*sheet)->c_element_rules[j] = (c_css_rules_list_t *) calloc(1, sizeof (c_css_rules_list_t));
   }
   (*sheet)->c_any_element_rules = (c_css_rules_list_t *) calloc(1, sizeof (c_css_rules_list_t));
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

   memset(&context->c_match_cache, 0, sizeof (context->c_match_cache));
   match_cache_set_size(&context->c_match_cache, context->c_sheets[CSS_PRIMARY_USER_AGENT]->c_required_match_cache); // Initially the size is zero.

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
                                   c_css_declaration_set_t * mergedDeclList, Doctree *docTree,
                                   c_doctree_node_t * dtn,
                                   c_css_declaration_set_t * declList,
                                   c_css_declaration_set_t * declListImportant,
                                   c_css_declaration_set_t * declListNonCss) {

   static int i = 0;
   char path[20] = { 0 };
   snprintf(path, sizeof (path), "/tmp/css_rules_%04d", i);
   FILE * file = fopen(path, "w");
   i++;

   fprintf(file, "CSS_PRIMARY_USER_AGENT\n");
   css_style_sheet_apply_style_sheet(context->c_sheets[CSS_PRIMARY_USER_AGENT], file, mergedDeclList, docTree, dtn, &context->c_match_cache);

   fprintf(file, "CSS_PRIMARY_USER\n");
   css_style_sheet_apply_style_sheet(context->c_sheets[CSS_PRIMARY_USER], file, mergedDeclList, docTree, dtn, &context->c_match_cache);

   if (declListNonCss)
      hll_declarationListAppend(mergedDeclList, declListNonCss);

   fprintf(file, "CSS_PRIMARY_AUTHOR\n");
   css_style_sheet_apply_style_sheet(context->c_sheets[CSS_PRIMARY_AUTHOR], file, mergedDeclList, docTree, dtn, &context->c_match_cache);

   if (declList)
      hll_declarationListAppend(mergedDeclList, declList);

   fprintf(file, "CSS_PRIMARY_AUTHOR_IMPORTANT\n");
   css_style_sheet_apply_style_sheet(context->c_sheets[CSS_PRIMARY_AUTHOR_IMPORTANT], file, mergedDeclList, docTree, dtn, &context->c_match_cache);

   if (declListImportant)
      hll_declarationListAppend(mergedDeclList, declListImportant);

   fprintf(file, "CSS_PRIMARY_USER_IMPORTANT\n");
   css_style_sheet_apply_style_sheet(context->c_sheets[CSS_PRIMARY_USER_IMPORTANT], file, mergedDeclList, docTree, dtn, &context->c_match_cache);

   fclose(file);
}

void css_context_add_rule(c_css_context_t * context, c_css_rule_t * rule, CssPrimaryOrder order)
{
   // TODO: should we do the increment even if we go into first branch of the
   // if/else below?
   context->c_rule_position++;

   if ((order == CSS_PRIMARY_AUTHOR || order == CSS_PRIMARY_AUTHOR_IMPORTANT) && !css_rule_is_safe(rule)) {
      MSG_WARN ("Ignoring unsafe author style that might reveal browsing history\n");
   } else {

      // Set match cache offset of selector (css_selector_set_match_cache_offset(c_css_selector_t * selector, int offset))
      if (rule->c_selector->c_match_case_offset == -1) {
         const int offset = context->c_match_cache.c_cache_items_size;
         rule->c_selector->c_match_case_offset = offset;
      }

      const int new_size = css_selector_get_required_match_cache(rule->c_selector);
      if (new_size > context->c_match_cache.c_cache_items_size) {
         match_cache_set_size(&context->c_match_cache, new_size);
      }

      hll_addRuleToStyleSheet(context->c_sheets[order], rule);
   }
}

CssLength cssCreateLength(float val, CssLengthType t)
{
   CssLength cssLength;
   cssLength.bits = hll_cssCreateLength(val, t);
   return cssLength;
}
CssLengthType cssLengthType(CssLength cssLength)
{
   return hll_cssLengthType(cssLength.bits);
}
float cssLengthValue(CssLength cssLength)
{
   return hll_cssLengthValue(cssLength.bits);
}


void match_cache_set_size(c_css_match_cache_t * match_cache, int new_size)
{
   for (int i = match_cache->c_cache_items_size; i < new_size; i++) {
      match_cache->c_cache_items[i] = -1;
   }
   match_cache->c_cache_items_size = new_size;
}

/* ===================================== */


void print_string_array_with_len_flat(FILE * file, char * const * arr, int size, const char * name)
{
   fprintf(file, "%s size = %d, array ptr = %p, ", name, size, arr);
   fprintf(file, "%s = [", name);
   for (int i = 0; i < size; i++) {
      const bool comma = size > 0 && i < size - 1;
      fprintf(file, "arr ptr[%d] %p = %s%s", i, arr[i], arr[i], comma ? ", " : "");
   }
   fprintf(file, "], ");

   return;
}

void print_string_array_with_len(FILE * file, char * const * arr, int size, const char * name)
{
   fprintf(file, "%s = [", name);
   for (int i = 0; i < size; i++) {
      const bool comma = size > 0 && i < size - 1;
      fprintf(file, "%s%s", arr[i], comma ? ", " : "");
   }
   fprintf(file, "]\n");
   fprintf(file, "%s size = %d\n", name, size);

#if 1
   fprintf(file, "    array ptr = %p, [", arr);
   for (int i = 0; i < size; i++) {
      const bool comma = size > 0 && i < size - 1;
      fprintf(file, "%p%s", arr[i], comma ? ", " : "");
   }
   fprintf(file, "]\n");
#endif

   return;
}

void css_simple_selector_print_flat(FILE * file, const c_css_simple_selector_t * sim_sel)
{
   fprintf(file, "C: simSel: ");
   print_string_array_with_len_flat(file, sim_sel->c_selector_pseudo_class, sim_sel->c_selector_pseudo_class_size, "pseudo class");
   fprintf(file, "id = '%s', ", sim_sel->c_selector_id);
   print_string_array_with_len_flat(file, sim_sel->c_selector_class, sim_sel->c_selector_class_size, "class");
   fprintf(file, "element = %d, ", sim_sel->c_selector_element);
   fprintf(file, "combinator = %d", sim_sel->c_combinator);
   fprintf(file, "\n");

   return;
}


void css_simple_selector_print_compact(FILE * file, const c_css_simple_selector_t * sim_sel)
{
   fprintf(file, "C: simSel:\n");
   print_string_array_with_len(file, sim_sel->c_selector_pseudo_class, sim_sel->c_selector_pseudo_class_size, "pseudo class");
   fprintf(file, "id = '%s'\n", sim_sel->c_selector_id);
   print_string_array_with_len(file, sim_sel->c_selector_class, sim_sel->c_selector_class_size, "class");
   fprintf(file, "element = %d\n", sim_sel->c_selector_element);
   fprintf(file, "combinator = %d\n", sim_sel->c_combinator);
   fprintf(file, "\n");

   return;
}

void css_rule_print_pretty(FILE * file, const c_css_rule_t * rule)
{
   fprintf(file, "    Rule: Begin\n");
   css_selector_print_pretty(file, rule->c_selector);
   if (nullptr != rule->c_decl_set) {
      fprintf(file, "        Rule Declarations (%d) {\n", rule->c_decl_set->c_declarations_size);
      css_declaration_set_print_pretty(file, rule->c_decl_set);
   } else {
         fprintf(file, "        Rule Declarations (0) {\n");
   }
   fprintf(file, "        Rule Declarations }\n");
   fprintf(file, "    Rule: End\n");
   fprintf(file, "    Rule: ---------------------------\n");
}


__attribute__((unused)) void css_selector_print_compact(FILE * file, const c_css_selector_t * selector)
{
   fprintf(file, "C: Selector:\n");
   fprintf(file, "simple selectors count: %d\n", selector->c_simple_selector_list_size);
   for (int i = 0; i < selector->c_simple_selector_list_size; i++) {
      css_simple_selector_print_compact(file, selector->c_simple_selector_list[i]);
   }
}



void css_selector_print_pretty(FILE * file, c_css_selector_t * selector)
{
   //fprintf(file, "        Rule SelectorList: Begin\n");
   fprintf(file, "        Rule SelectorList: %d simple selectors\n", selector->c_simple_selector_list_size);
   for (int i = 0; i < selector->c_simple_selector_list_size; i++) {
      css_simple_selector_print_pretty(file, selector->c_simple_selector_list[i]);

      if (i < selector->c_simple_selector_list_size - 1) {
         switch (selector->c_simple_selector_list[i + 1]->c_combinator) {
            case CssSelectorCombinatorChild:
               fprintf (file, "                Rule SelectorList: combinator > \n");
               break;
            case CssSelectorCombinatorDescendant:
               fprintf (file, "                Rule SelectorList: combinator \" \" \n");
               break;
            case CssSelectorCombinatorAdjacentSibling:
               fprintf (file, "                Rule SelectorList: combinator + \n");
               break;
            default:
               fprintf (file, "                Rule SelectorList: combinator ?\n");
               break;
         }
      }
   }
   //fprintf(file, "        Rule SelectorList: End\n");
   //fprintf(file, "        Rule SelectorList: --------\n");

   //fprintf (file, "\n");
}

void css_simple_selector_print_pretty(FILE * file, c_css_simple_selector_t * selector)
{
   fprintf(file, "            Rule SimpleSelector: ");

   if (selector->c_selector_element == CssSimpleSelectorElementAny) {
      fprintf(file, "Element ANY\n");
   } else if (selector->c_selector_element >= 0) {
      fprintf(file, "Element [%s]\n", a_Html_tag_name(selector->c_selector_element));
   }

   if (selector->c_selector_pseudo_class_size > 0) {
      fprintf(file, "            Rule SimpleSelector: selector = Pseudo class [");
      for (int i = 0; i < selector->c_selector_pseudo_class_size; i++) {
         if (0 == i) {
            fprintf(file, "%s", selector->c_selector_pseudo_class[i]);
         } else {
            fprintf(file, " %s", selector->c_selector_pseudo_class[i]);
         }
      }
      fprintf(file, "]\n");
   }
   if (selector->c_selector_id) {
      fprintf(file, "            Rule SimpleSelector: selector = ID %s\n", selector->c_selector_id);
   }
   if (selector->c_selector_class_size) {
      fprintf(file, "            Rule SimpleSelector: selector = class [");
      for (int i = 0; i < selector->c_selector_class_size; i++)
         fprintf (file, ".%s", selector->c_selector_class[i]);
      fprintf (file, "]\n");
   }
}

void css_declaration_set_print_pretty(FILE * file, c_css_declaration_set_t * decl_set)
{
   for (int i = 0; i < decl_set->c_declarations_size; i++) {
      css_declaration_print_pretty(file, decl_set->c_declarations[i]);
   }
}


void css_declaration_print_pretty(FILE * file, c_css_declaration_t * declaration)
{
   if (declaration->c_important) {
      fprintf(file, "important = true\n");
   }
   switch (declaration->c_value->c_type_tag) {
   case CssDeclarationValueTypeSTRING:
   case CssDeclarationValueTypeSYMBOL:
   case CssDeclarationValueTypeURI:
      if (declaration->c_property == -1) {
         fprintf(file, "ERROR: property == -1 (A)\n"); // TODO: unset property. Maybe this happens on parse error?
      } else {
         fprintf (file, "            Rule: Declaration: property = '%s', value = [%s]\n", hll_cssPropertyNameString(declaration->c_property), declaration->c_value->c_text_val);
      }
      break;
   case CssDeclarationValueTypeBACKGROUND_POSITION:
      if (declaration->c_property == -1) {
         fprintf(file, "ERROR: property == -1 (B)\n"); // TODO: unset property. Maybe this happens on parse error?
      } else {
         fprintf (file, "            Rule: Declaration: property = '%s', posValue = %d / %d\n",
                  hll_cssPropertyNameString(declaration->c_property),
                  declaration->c_value->c_bg_pos_x,
                  declaration->c_value->c_bg_pos_y);
      }
      break;
   default:
      if (declaration->c_property == -1) {
         fprintf(file, "ERROR: property == -1 (C)\n"); // TODO: unset property. Maybe this happens on parse error?
      } else {
         fprintf (file, "            Rule: Declaration: property = '%s', value = %d\n", hll_cssPropertyNameString(declaration->c_property), declaration->c_value->c_int_val);
      }
      break;
   }
}
