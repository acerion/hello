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

void printCssDeclaration(CssDeclaration * declaration, FILE * file)
{
   switch (declaration->value.type) {
   case CssDeclarationValueTypeSTRING:
   case CssDeclarationValueTypeSYMBOL:
   case CssDeclarationValueTypeURI:
      fprintf (file, "            Rule: Declaration: property = '%s', value = [%s]\n", hll_cssPropertyNameString(declaration->property), declaration->value.strVal);
      break;
   case CssDeclarationValueTypeBACKGROUND_POSITION:
      if (declaration->value.posVal) {
         fprintf (file, "            Rule: Declaration: property = '%s', posValue = %d / %d\n", hll_cssPropertyNameString(declaration->property),
                  declaration->value.posVal->posX, declaration->value.posVal->posY);
      } else {
         fprintf (file, "            Rule: Declaration: property = '%s', posValue = unknown\n", hll_cssPropertyNameString(declaration->property));
      }
      break;
   default:
      fprintf (file, "            Rule: Declaration: property = '%s', value = %d\n", hll_cssPropertyNameString(declaration->property), declaration->value.intVal);
      break;
   }
}

CssDeclartionList::CssDeclartionList (const CssDeclartionList & declList, bool deep) :
   lout::misc::SimpleVector <CssDeclaration> (declList)
{
   isSafe = declList.isSafe;
   if (deep) {
      for (int i = 0; i < size (); i++) {
         CssDeclaration * decl = getRef(i);
         switch (decl->value.type) {
         case CssDeclarationValueTypeSTRING:
         case CssDeclarationValueTypeSYMBOL:
               decl->value.strVal = dStrdup (decl->value.strVal);  // TODO: there seems to be a mistake: string is duplicated onto itself
               break;
            default:
               break;
         }
      }
      ownerOfStrings = true;
   } else {
      ownerOfStrings = false;
   }
}

CssDeclartionList::~CssDeclartionList () {
   if (ownerOfStrings)
      for (int i = 0; i < size (); i++)
         getRef (i)->free ();
}

/**
 * \brief Set property to a given name and type.
 */
void CssDeclartionList::updateOrAddDeclaration(CssDeclarationProperty property, CssDeclarationValue value) {
   CssDeclaration * decl;

   if (property == CSS_PROPERTY_DISPLAY || property == CSS_PROPERTY_BACKGROUND_IMAGE)
      isSafe = false;

   for (int i = 0; i < size (); i++) {
      decl = getRef (i);

      if (decl->property == property) {
         if (ownerOfStrings)
            decl->free ();
         decl->value = value;
         return;
      }
   }

   increase ();
   decl = getRef (size () - 1);
   decl->property = property;
   decl->value = value;
}

/**
 * \brief Merge properties into argument property list.
 */
void CssDeclartionList::appendDeclarationsToArg(CssDeclartionList * declList) {
   for (int i = 0; i < size (); i++) {
      CssDeclarationValue value = getRef (i)->value;

      if (declList->ownerOfStrings &&
          (getRef (i)->value.type == CssDeclarationValueTypeSTRING ||
           getRef (i)->value.type == CssDeclarationValueTypeSYMBOL))
         value.strVal = strdup(value.strVal);

      value.type = getRef (i)->value.type;
      declList->updateOrAddDeclaration(getRef(i)->property, value);
   }
}

void printCssDeclartionList(CssDeclartionList * declList, FILE * file) {
   for (int i = 0; i < declList->size(); i++)
      printCssDeclaration(declList->getRef(i), file);
}

CssSelector::CssSelector () {
   refCount = 0;
   matchCacheOffset = -1;
   selectorListSize++;
   struct CombinatorAndSelector * cs = &selectorList[selectorListSize - 1];

   cs->combinator = CssSelectorCombinatorNone;
   cs->simpleSelector = new CssSimpleSelector ();
}

CssSelector::~CssSelector () {
   for (int i = selectorListSize - 1; i >= 0; i--)
      delete selectorList[i].simpleSelector;
}

/**
 * \brief Return whether selector matches at a given node in the document tree.
 */
bool CssSelector::full_selector_matches(Doctree *docTree, const DoctreeNode *node,
                          int i, Combinator comb, MatchCache *matchCache) {
   int *matchCacheEntry;
   assert (node);

   if (i < 0)
      return true;

   struct CombinatorAndSelector *cs = &selectorList[i];
   CssSimpleSelector *sel = cs->simpleSelector;

   switch (comb) {
      case CssSelectorCombinatorNone:
         break;
      case CssSelectorCombinatorChild:
         node = docTree->parent (node);
         break;
      case CssSelectorCombinatorAdjacentSibling:
         node = docTree->sibling (node);
         break;
      case CssSelectorCombinatorDescendant:
         node = docTree->parent (node);
         matchCacheEntry = matchCache->getRef(matchCacheOffset + i);

         for (const DoctreeNode *n = node;
              n && n->num > *matchCacheEntry; n = docTree->parent (n))
            if (simple_selector_matches(sel, n) &&
                full_selector_matches(docTree, n, i - 1, cs->combinator, matchCache))
               return true;

         if (node) // remember that it didn't match to avoid future tests
            *matchCacheEntry = node->num;

         return false;
         break;
      default:
         return false; // \todo implement other combinators
   }

   if (!node || !simple_selector_matches(sel, node))
      return false;

   // tail recursion should be optimized by the compiler
   return full_selector_matches(docTree, node, i - 1, cs->combinator, matchCache);
}

void cssSelectorAddSimpleSelector(CssSelector * selector, Combinator c) {
   assert (selector->matchCacheOffset == -1);
   selector->selectorListSize++;
   struct CombinatorAndSelector * cs = &selector->selectorList[selector->selectorListSize - 1];

   cs->combinator = c;
   cs->simpleSelector = new CssSimpleSelector ();
}

bool CssSelector::checksPseudoClass () {
   for (int i = 0; i < selectorListSize; i++)
      if (nullptr != selectorList[i].simpleSelector->selector_pseudo_class)
         return true;
   return false;
}

/**
 * \brief Return the specificity of the selector.
 *
 * The specificity of a CSS selector is defined in
 * http://www.w3.org/TR/CSS21/cascade.html#specificity
 */
int CssSelector::specificity () {
   int spec = 0;

   for (int i = 0; i < selectorListSize; i++)
      spec += cssSimpleSelectorSpecificity(selectorList[i].simpleSelector);

   return spec;
}

void CssSelector::printCssSelector(FILE * file) {
   //fprintf(file, "        Rule SelectorList: Begin\n");
   fprintf(file, "        Rule SelectorList: %d simple selectors\n", selectorListSize);
   for (int i = 0; i < selectorListSize; i++) {
      printCssSimpleSelector(selectorList[i].simpleSelector, file);

      if (i < selectorListSize - 1) {
         switch (selectorList[i + 1].combinator) {
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
               fprintf (file, "                Rule SelectorList: combinator ? \n");
               break;
         }
      }
   }
   //fprintf(file, "        Rule SelectorList: End\n");
   //fprintf(file, "        Rule SelectorList: --------\n");

   //fprintf (file, "\n");
}

void setSimpleSelector(CssSimpleSelector * selector, CssSelectorType type, const char *value) {

   switch (type) {
   case CssSelectorType::CLASS:
         snprintf(selector->selector_class[selector->selector_class_size], sizeof (selector->selector_class[selector->selector_class_size]), "%s", value);
         selector->selector_class_size++;
         break;
   case CssSelectorType::PSEUDO_CLASS:
         if (selector->selector_pseudo_class == NULL)
            selector->selector_pseudo_class = dStrdup(value);
         break;
   case CssSelectorType::ID:
         if (selector->selector_id == NULL)
            selector->selector_id = dStrdup(value);
         break;
      default:
         break;
   }
}

/**
 * \brief Return whether simple selector matches at a given node of
 *        the document tree.
 */
bool simple_selector_matches(CssSimpleSelector * selector, const DoctreeNode *n) {
   assert (n);
   if (selector->selector_element != CssSimpleSelectorElementAny && selector->selector_element != n->html_element_idx)
      return false;
   if (selector->selector_pseudo_class != NULL &&
      (n->pseudo == NULL || dStrAsciiCasecmp (selector->selector_pseudo_class, n->pseudo) != 0))
      return false;
   if (selector->selector_id != NULL && (n->element_id == NULL || dStrAsciiCasecmp (selector->selector_id, n->element_id) != 0))
      return false;
   for (int i = 0; i < selector->selector_class_size; i++) {
      bool found = false;
      if (n->element_class != NULL) {
         for (int j = 0; j < n->element_class->size (); j++) {
            if (dStrAsciiCasecmp (selector->selector_class[i], n->element_class->get(j)) == 0) {
               found = true;
               break;
            }
         }
      }
      if (! found)
         return false;
   }

   return true;
}

/**
 * \brief Return the specificity of the simple selector.
 *
 * The result is used in CssSelector::specificity ().
 */
int cssSimpleSelectorSpecificity(CssSimpleSelector * selector)
{
   int spec = 0;

   if (selector->selector_id)
      spec += 1 << 20;
   spec += selector->selector_class_size << 10;
   if (selector->selector_pseudo_class)
      spec += 1 << 10;
   if (selector->selector_element != CssSimpleSelectorElementAny)
      spec += 1;

   return spec;
}

void printCssSimpleSelector(CssSimpleSelector * selector, FILE * file)
{
   fprintf(file, "            Rule SimpleSelector: ");

   if (selector->selector_element == CssSimpleSelectorElementAny) {
      fprintf(file, "Element ANY\n");
   } else if (selector->selector_element >= 0) {
      fprintf(file, "Element [%s]\n", a_Html_tag_name(selector->selector_element));
   }

   if (selector->selector_pseudo_class) {
      fprintf(file, "            Rule SimpleSelector: selector = Pseudo class [%s]\n", selector->selector_pseudo_class);
   }
   if (selector->selector_id) {
      fprintf(file, "            Rule SimpleSelector: selector = ID %s\n", selector->selector_id);
   }
   if (selector->selector_class_size) {
      fprintf(file, "            Rule SimpleSelector: selector = class [");
      for (int i = 0; i < selector->selector_class_size; i++)
         fprintf (file, ".%s", selector->selector_class[i]);
      fprintf (file, "]\n");
   }
}

CssRule::CssRule (CssSelector *selector, CssDeclartionList * declList, int pos) {
   assert (selector->selectorListSize > 0);

   this->selector = selector;
   this->selector->ref ();
   this->declList = declList;
   this->position = position;
   this->specificity = selector->specificity ();
}

CssRule::~CssRule () {
   selector->unref ();
}

void CssRule::apply_css_rule(FILE * file, CssDeclartionList * outDeclList, Doctree *docTree,
                     const DoctreeNode *node, MatchCache *matchCache) const {
   if (selector->full_selector_submatches(docTree, node, matchCache))
      this->declList->appendDeclarationsToArg(outDeclList);

   this->printCssRule(file);
}

void CssRule::printCssRule (FILE * file) const {

   fprintf(file, "    Rule: Begin\n");
   selector->printCssSelector (file);
   if (nullptr != this->declList) {
      fprintf(file, "        Rule Declarations (%d) {\n", declList->size());
      printCssDeclartionList(declList, file);
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
   CssSimpleSelector *top = rule->selector->top ();
   RuleList *ruleList = NULL;
   lout::object::ConstString *string;

   if (nullptr != top->selector_id) {
      string = new lout::object::ConstString(top->selector_id);
      ruleList = idTable.get (string);
      if (ruleList == NULL) {
         ruleList = new RuleList ();
         idTable.put (string, ruleList);
      } else {
         delete string;
      }
   } else if (top->selector_class_size > 0) {
      string = new lout::object::ConstString (top->selector_class[0]);
      ruleList = classTable.get (string);
      if (ruleList == NULL) {
         ruleList = new RuleList;
         classTable.put (string, ruleList);
      } else {
         delete string;
      }
   } else if (top->selector_element >= 0 && top->selector_element < ntags) {
      ruleList = &elementTable[top->selector_element];
   } else if (top->selector_element == CssSimpleSelectorElementAny) {
      ruleList = &anyTable;
   }

   if (ruleList) {
      ruleList->insert (rule);
      if (rule->selector->getRequiredMatchCache () > requiredMatchCache)
         requiredMatchCache = rule->selector->getRequiredMatchCache ();
   } else {
      assert (top->selector_element == CssSimpleSelectorElementNone);
      delete rule;
   }
}

/**
 * \brief Apply a stylesheet to a list of declarations.
 *
 * The declarations (list property+value) are set as defined by the rules in
 * the stylesheet that match at the given node in the document tree.
 */
void CssStyleSheet::apply_style_sheet(CssDeclartionList * declList, Doctree *docTree,
                                      const DoctreeNode *node, MatchCache *matchCache) const {
   static const int maxLists = 32;
   const RuleList *ruleList[maxLists];
   int numLists = 0, index[maxLists] = {0};

   if (node->element_id) {
      lout::object::ConstString idString (node->element_id);

      ruleList[numLists] = idTable.get (&idString);
      if (ruleList[numLists])
         numLists++;
   }

   if (node->element_class) {
      for (int i = 0; i < node->element_class->size (); i++) {
         if (i >= maxLists - 4) {
            MSG_WARN("Maximum number of classes per element exceeded.\n");
            break;
         }

         lout::object::ConstString classString (node->element_class->get(i));

         ruleList[numLists] = classTable.get (&classString);
         if (ruleList[numLists])
            numLists++;
      }
   }

   ruleList[numLists] = &elementTable[node->html_element_idx];
   if (ruleList[numLists])
      numLists++;

   ruleList[numLists] = &anyTable;
   if (ruleList[numLists])
      numLists++;

   static int i = 0;
   char path[20] = { 0 };
   snprintf(path, sizeof (path), "/tmp/css_rules_%04d", i);
   FILE * file = fopen(path, "w");
   i++;


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
         rule->apply_css_rule(file, declList, docTree, node, matchCache);
         index[minSpecIndex]++;
      } else {
         break;
      }
   }

   fclose(file);

}

CssStyleSheet CssContext::userAgentSheet;

CssContext::CssContext () {
   pos = 0;
   matchCache.setSize (userAgentSheet.getRequiredMatchCache (), -1);
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
void CssContext::apply_css_context(CssDeclartionList * mergedDeclList, Doctree *docTree,
                                   DoctreeNode * node,
                                   CssDeclartionList * declList,
                                   CssDeclartionList * declListImportant,
                                   CssDeclartionList * declListNonCss) {

   userAgentSheet.apply_style_sheet(mergedDeclList, docTree, node, &matchCache);

   sheet[CSS_PRIMARY_USER].apply_style_sheet(mergedDeclList, docTree, node, &matchCache);

   if (declListNonCss)
        declListNonCss->appendDeclarationsToArg(mergedDeclList);

   sheet[CSS_PRIMARY_AUTHOR].apply_style_sheet(mergedDeclList, docTree, node, &matchCache);

   if (declList)
        declList->appendDeclarationsToArg(mergedDeclList);

   sheet[CSS_PRIMARY_AUTHOR_IMPORTANT].apply_style_sheet(mergedDeclList, docTree, node, &matchCache);

   if (declListImportant)
        declListImportant->appendDeclarationsToArg(mergedDeclList);

   sheet[CSS_PRIMARY_USER_IMPORTANT].apply_style_sheet(mergedDeclList, docTree, node, &matchCache);
}

void CssContext::addRule (CssSelector *sel, CssDeclartionList * declList,
                          CssPrimaryOrder order) {

   if (declList->size () > 0) {
      CssRule *rule = new CssRule (sel, declList, pos++);

      if ((order == CSS_PRIMARY_AUTHOR ||
           order == CSS_PRIMARY_AUTHOR_IMPORTANT) &&
           !rule->isSafe ()) {
         MSG_WARN ("Ignoring unsafe author style that might reveal browsing history\n");
         delete rule;
      } else {
         rule->selector->setMatchCacheOffset(matchCache.size ());
         if (rule->selector->getRequiredMatchCache () > matchCache.size ())
            matchCache.setSize (rule->selector->getRequiredMatchCache (), -1);

         if (order == CSS_PRIMARY_USER_AGENT) {
            userAgentSheet.addRule (rule);
         } else { 
            sheet[order].addRule (rule);
         }
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
