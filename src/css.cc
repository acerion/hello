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

void cssValueCopy(c_css_value_t * dest, c_css_value_t * src);

void printCssDeclaration(c_css_declaration_t * declaration, FILE * file)
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


c_css_declaration_set_t * declarationListNew(void)
{
   c_css_declaration_set_t * set = (c_css_declaration_set_t *) calloc(1, sizeof (c_css_declaration_set_t));
   set->c_is_safe = true;
   set->c_declarations = (c_css_declaration_t *) calloc(DECLARATIONS_COUNT_IN_SET, sizeof (c_css_declaration_t));
   for (int i = 0; i < DECLARATIONS_COUNT_IN_SET; i++) {
      set->c_declarations[i].c_value = (c_css_value_t *) calloc(1, sizeof (c_css_value_t));
   }

   return set;
}

c_css_declaration_set_t * declarationListNew(const c_css_declaration_set_t * inDeclList)
{
   c_css_declaration_set_t * out = declarationListNew();

   memcpy(out->c_declarations, inDeclList->c_declarations, DECLARATIONS_COUNT_IN_SET * sizeof (c_css_declaration_set_t));
   out->c_is_safe = inDeclList->c_is_safe;

   for (int i = 0; i < out->c_declarations_count; i++) {
      c_css_declaration_t * decl = &out->c_declarations[i];
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

void declarationListAddOrUpdateDeclaration(c_css_declaration_set_t * declList, CssDeclarationProperty property, c_css_value_t value)
{
   c_css_declaration_t * decl = new c_css_declaration_t;
   decl->c_property = property;
   decl->c_value = (c_css_value_t *) calloc(1, sizeof (c_css_value_t));
   cssValueCopy(decl->c_value, &value);

   hll_declarationListAddOrUpdateDeclaration(declList, decl);
}


void cssValueCopy(c_css_value_t * dest, c_css_value_t * src)
{
   dest->c_type_tag = src->c_type_tag;
   dest->c_int_val  = src->c_int_val;
   dest->c_bg_pos_x = src->c_bg_pos_x;
   dest->c_bg_pos_y = src->c_bg_pos_y;
   if (src->c_type_tag == CssDeclarationValueTypeSTRING || src->c_type_tag == CssDeclarationValueTypeSYMBOL) {
      dest->c_text_val = strdup(src->c_text_val);
   }
}


void declarationListPrint(c_css_declaration_set_t * declList, FILE * file)
{
   for (int i = 0; i < declList->c_declarations_count; i++)
      printCssDeclaration(&declList->c_declarations[i], file);
}

/**
 * \brief Return whether selector matches at a given node in the document tree.
 */
bool selector_matches(c_css_selector_t * selector, Doctree *docTree, const c_doctree_node_t * dtn, int simSelIdx, Combinator comb, MatchCache *matchCache) {
   assert (dtn);

   if (simSelIdx < 0) {
      return true;
   }

   struct c_css_simple_selector_t *simpleSelector = selector->c_simple_selector_list[simSelIdx];

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
         {
            dtn = docTree->parent(dtn);
            int *matchCacheEntry = &matchCache->arr[selector->c_match_case_offset + simSelIdx];

#if 0
            for (size_t z = 0; z < matchCache->size; z++) {
               fprintf(stderr, "matchCache->arr[%zd] = %d\n", z, matchCache->arr[z]);
            }
            fprintf(stderr, "\n");
#endif

            for (const c_doctree_node_t *n = dtn; n && n->c_unique_num > *matchCacheEntry; n = docTree->parent (n)) {
               if (hll_simpleSelectorMatches(simpleSelector, n)
                   && selector_matches(selector, docTree, n, simSelIdx - 1, (Combinator) simpleSelector->c_combinator, matchCache)) {
                  return true;
               }
            }

            if (dtn) // remember that it didn't match to avoid future tests
               *matchCacheEntry = dtn->c_unique_num;

            return false;
         }
         break;
      default:
         return false; // \todo implement other combinators
   }

   if (!dtn) {
      return false;
   } else {
      if (!hll_simpleSelectorMatches(simpleSelector, dtn)) {
         return false;
      }
   }

   // tail recursion should be optimized by the compiler
   return selector_matches(selector, docTree, dtn, simSelIdx - 1, (Combinator) simpleSelector->c_combinator, matchCache);
}

bool selectorChecksPseudoClass(c_css_selector_t * selector) {
   for (int i = 0; i < selector->c_simple_selector_list_size; i++)
      if (selector->c_simple_selector_list[i]->c_selector_pseudo_class_size > 0) // Remember that C/C++ code can use only first pseudo class
         return true;
   return false;
}

c_css_simple_selector_t * selectorGetTopSimpleSelector(c_css_selector_t * selector)
{
   return selector->c_simple_selector_list[selector->c_simple_selector_list_size - 1];
}

void selectorSetMatchCacheOffset(c_css_selector_t * selector, int mo)
{
   if (selector->c_match_case_offset == -1) {
      selector->c_match_case_offset = mo;
   }
}

int selectorGetRequiredMatchCache(c_css_selector_t * selector)
{
   return selector->c_match_case_offset + selector->c_simple_selector_list_size;
}

/**
 * \brief Return the specificity of the selector.
 *
 * The specificity of a CSS selector is defined in
 * http://www.w3.org/TR/CSS21/cascade.html#specificity
 */
int selectorSpecificity(c_css_selector_t * selector) {
   int spec = 0;

   for (int i = 0; i < selector->c_simple_selector_list_size; i++)
      spec += cssSimpleSelectorSpecificity(selector->c_simple_selector_list[i]);

   return spec;
}

void printCssSelector(c_css_selector_t * selector, FILE * file) {
   //fprintf(file, "        Rule SelectorList: Begin\n");
   fprintf(file, "        Rule SelectorList: %d simple selectors\n", selector->c_simple_selector_list_size);
   for (int i = 0; i < selector->c_simple_selector_list_size; i++) {
      printCssSimpleSelector(selector->c_simple_selector_list[i], file);

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

/**
 * \brief Return the specificity of the simple selector.
 *
 * The result is used in CssSelector::specificity ().
 */
int cssSimpleSelectorSpecificity(c_css_simple_selector_t * selector)
{
   int spec = 0;

   if (selector->c_selector_id)
      spec += 1 << 20;
   spec += selector->c_selector_class_size << 10;
   if (selector->c_selector_pseudo_class_size > 0) // Remember that C/C++ code can use only first pseudo code.
      spec += 1 << 10;
   if (selector->c_selector_element != CssSimpleSelectorElementAny)
      spec += 1;

   return spec;
}

void printCssSimpleSelector(c_css_simple_selector_t * selector, FILE * file)
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

CssRule::CssRule(c_css_selector_t * selector, c_css_declaration_set_t * declList, int rulePosition)
{
   assert (selector->c_simple_selector_list_size > 0);

   this->selector = selector;
   this->declList = declList;
   this->position = rulePosition;
   this->specificity = selectorSpecificity(selector);
}

void CssRule::apply_css_rule(FILE * file, c_css_declaration_set_t * outDeclList, Doctree *docTree, const c_doctree_node_t * dtn, MatchCache *matchCache) const {
   if (selector_matches(selector, docTree, dtn, selector->c_simple_selector_list_size - 1, CssSelectorCombinatorNone, matchCache)) {
      hll_declarationListAppend(outDeclList, this->declList);
   }

   this->printCssRule(file);
}

void CssRule::printCssRule (FILE * file) const {

   fprintf(file, "    Rule: Begin\n");
   printCssSelector(selector, file);
   if (nullptr != this->declList) {
      fprintf(file, "        Rule Declarations (%d) {\n", declList->c_declarations_count);
      declarationListPrint(declList, file);
   } else {
         fprintf(file, "        Rule Declarations (0) {\n");
   }
   fprintf(file, "        Rule Declarations }\n");
   fprintf(file, "    Rule: End\n");
   fprintf(file, "    Rule: ---------------------------\n");
}

/*
 * \brief Insert rule with increasing specificity.
 *
 * If two rules have the same specificity, the one that was added later
 * will be added behind the others.
 * This gives later added rules more weight.
 */
void CssStyleSheet::RuleList::insert (CssRule *rule) {
   increase ();
   int i = size () - 1;

   while (i > 0 && rule->specificity < get (i - 1)->specificity) {
      *getRef (i) = get (i - 1);
      i--;
   }

   *getRef (i) = rule;
}

/**
 * \brief Insert a rule into CssStyleSheet.
 *
 * To improve matching performance the rules are organized into
 * rule lists based on the topmost simple selector of their selector.
 */
void CssStyleSheet::addRule (CssRule *rule) {
   c_css_simple_selector_t *top = selectorGetTopSimpleSelector(rule->selector);
   RuleList *ruleList = NULL;
   lout::object::ConstString *string;

   if (nullptr != top->c_selector_id) {
      string = new lout::object::ConstString(top->c_selector_id);
      ruleList = idTable.get (string);
      if (ruleList == NULL) {
         ruleList = new RuleList ();
         idTable.put (string, ruleList);
      } else {
         delete string;
      }
   } else if (top->c_selector_class_size > 0) {
      string = new lout::object::ConstString (top->c_selector_class[0]);
      ruleList = classTable.get (string);
      if (ruleList == NULL) {
         ruleList = new RuleList;
         classTable.put (string, ruleList);
      } else {
         delete string;
      }
   } else if (top->c_selector_element >= 0 && top->c_selector_element < ntags) {
      ruleList = &elementTable[top->c_selector_element];
   } else if (top->c_selector_element == CssSimpleSelectorElementAny) {
      ruleList = &anyTable;
   }

   if (ruleList) {
      ruleList->insert (rule);
      if (selectorGetRequiredMatchCache(rule->selector) > requiredMatchCache)
         requiredMatchCache = selectorGetRequiredMatchCache(rule->selector);
   } else {
      assert (top->c_selector_element == CssSimpleSelectorElementNone);
   }
}

/**
 * \brief Apply a stylesheet to a list of declarations.
 *
 * The declarations (list property+value) are set as defined by the rules in
 * the stylesheet that match at the given node in the document tree.
 */
void CssStyleSheet::apply_style_sheet(FILE * file, c_css_declaration_set_t * declList, Doctree *docTree, const c_doctree_node_t *dtn, MatchCache *matchCache) const {
   static const int maxLists = 32;
   const RuleList *ruleList[maxLists];
   int numLists = 0, index[maxLists] = {0};

   if (dtn->c_element_selector_id) {
      lout::object::ConstString idString (dtn->c_element_selector_id);

      ruleList[numLists] = idTable.get (&idString);
      if (ruleList[numLists])
         numLists++;
   }

   for (int i = 0; i < dtn->c_element_selector_class_size; i++) {
      if (i >= maxLists - 4) {
         MSG_WARN("Maximum number of classes per element exceeded.\n");
         break;
      }

      lout::object::ConstString classString (dtn->c_element_selector_class[i]);

      ruleList[numLists] = classTable.get (&classString);
      if (ruleList[numLists])
         numLists++;
   }

   ruleList[numLists] = &elementTable[dtn->c_html_element_idx];
   if (ruleList[numLists])
      numLists++;

   ruleList[numLists] = &anyTable;
   if (ruleList[numLists])
      numLists++;

   // Apply potentially matching rules from ruleList[0-numLists] with
   // ascending specificity.
   // If specificity is equal, rules are applied in order of appearance.
   //  Each ruleList is sorted already.
   while (true) {
      int minSpec = 1 << 30;
      int minPos = 1 << 30;
      int minSpecIndex = -1;

      for (int i = 0; i < numLists; i++) {
         const RuleList *rl = ruleList[i];

         if (rl && rl->size () > index[i] &&
            (rl->get(index[i])->specificity < minSpec ||
             (rl->get(index[i])->specificity == minSpec &&
              rl->get(index[i])->position < minPos))) {

            minSpec = rl->get(index[i])->specificity;
            minPos = rl->get(index[i])->position;
            minSpecIndex = i;
         }
      }

      if (minSpecIndex >= 0) {
         CssRule *rule = ruleList[minSpecIndex]->get (index[minSpecIndex]);
         rule->apply_css_rule(file, declList, docTree, dtn, matchCache);
         index[minSpecIndex]++;
      } else {
         break;
      }
   }
}

CssStyleSheet CssContext::userAgentSheet;

CssContext::CssContext () {
   rulePosition = 0;
   matchCacheSetSize(&this->matchCache, userAgentSheet.requiredMatchCache);
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
void CssContext::apply_css_context(c_css_declaration_set_t * mergedDeclList, Doctree *docTree,
                                   c_doctree_node_t * dtn,
                                   c_css_declaration_set_t * declList,
                                   c_css_declaration_set_t * declListImportant,
                                   c_css_declaration_set_t * declListNonCss) {

   CssContext * context = this;
   static int i = 0;
   char path[20] = { 0 };
   snprintf(path, sizeof (path), "/tmp/css_rules_%04d", i);
   FILE * file = fopen(path, "w");
   i++;

   userAgentSheet.apply_style_sheet(file, mergedDeclList, docTree, dtn, &context->matchCache);

   fprintf(file, "CSS_PRIMARY_USER\n");
   sheet[CSS_PRIMARY_USER].apply_style_sheet(file, mergedDeclList, docTree, dtn, &context->matchCache);

   if (declListNonCss)
      hll_declarationListAppend(mergedDeclList, declListNonCss);

   fprintf(file, "CSS_PRIMARY_AUTHOR\n");
   sheet[CSS_PRIMARY_AUTHOR].apply_style_sheet(file, mergedDeclList, docTree, dtn, &context->matchCache);

   if (declList)
      hll_declarationListAppend(mergedDeclList, declList);

   fprintf(file, "CSS_PRIMARY_AUTHOR_IMPORTANT\n");
   sheet[CSS_PRIMARY_AUTHOR_IMPORTANT].apply_style_sheet(file, mergedDeclList, docTree, dtn, &context->matchCache);

   if (declListImportant)
      hll_declarationListAppend(mergedDeclList, declListImportant);

   fprintf(file, "CSS_PRIMARY_USER_IMPORTANT\n");
   sheet[CSS_PRIMARY_USER_IMPORTANT].apply_style_sheet(file, mergedDeclList, docTree, dtn, &context->matchCache);

   fclose(file);
}

void addRuleToContext(CssContext * context, CssRule * rule, CssPrimaryOrder order)
{
   if ((order == CSS_PRIMARY_AUTHOR || order == CSS_PRIMARY_AUTHOR_IMPORTANT) && !rule->isSafe ()) {
      MSG_WARN ("Ignoring unsafe author style that might reveal browsing history\n");
   } else {
      selectorSetMatchCacheOffset(rule->selector, context->matchCache.size);
      const size_t newSize = selectorGetRequiredMatchCache(rule->selector);
      if (newSize > context->matchCache.size)
         matchCacheSetSize(&context->matchCache, newSize);

      if (order == CSS_PRIMARY_USER_AGENT) {
         context->userAgentSheet.addRule (rule);
      } else {
         context->sheet[order].addRule (rule);
      }
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


void matchCacheSetSize(MatchCache * matchCache, size_t newSize)
{
   for (size_t i = matchCache->size; i < newSize; i++) {
      matchCache->arr[i] = -1;
   }
   matchCache->size = newSize;
}


void printStringArrayWithLenFlat(FILE * file, char * const * arr, int size, const char * name)
{
   fprintf(file, "%s size = %d, ", name, size);
   fprintf(file, "%s = [", name);
   for (int i = 0; i < size; i++) {
      fprintf(file, "%s ", arr[i]);
   }
   fprintf(file, "], ");

   return;
}

void printCssSimpleSelectorFlat(FILE * file, const c_css_simple_selector_t * sim_sel)
{
   fprintf(file, "C: simSel: ");
   printStringArrayWithLenFlat(file, sim_sel->c_selector_pseudo_class, sim_sel->c_selector_pseudo_class_size, "pseudo class");
   fprintf(file, "id = '%s', ", sim_sel->c_selector_id);
   printStringArrayWithLenFlat(file, sim_sel->c_selector_class, sim_sel->c_selector_class_size, "class");
   fprintf(file, "element = %d, ", sim_sel->c_selector_element);
   fprintf(file, "combinator = %d", sim_sel->c_combinator);
   fprintf(file, "\n");

   return;
}
