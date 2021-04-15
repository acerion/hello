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

void CssDeclaration::printCssDeclaration(FILE * file)
{
   switch (this->value.type) {
   case CssDeclarationValueTypeSTRING:
   case CssDeclarationValueTypeSYMBOL:
   case CssDeclarationValueTypeURI:
      fprintf (file, "            Rule: Declaration: property = '%s', value = [%s]\n", hll_cssPropertyNameString(this->property), this->value.strVal);
      break;
   default:
      fprintf (file, "            Rule: Declaration: property = '%s', value = %d\n", hll_cssPropertyNameString(this->property), this->value.intVal);
      break;
   }
}

CssDeclartionList::CssDeclartionList (const CssDeclartionList & declList, bool deep) :
   lout::misc::SimpleVector <CssDeclaration> (declList)
{
   refCount = 0;
   safe = declList.safe;
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
      safe = false;

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

void CssDeclartionList::printCssDeclartionList (FILE * file) {
   //fprintf(stderr, "        Rule: %d Declarations Begin\n", size());
   for (int i = 0; i < size(); i++)
      getRef (i)->printCssDeclaration (file);
   //fprintf(stderr, "        Rule: Declarations End\n");
}

CssSelector::CssSelector () {
   struct CombinatorAndSelector *cs;

   refCount = 0;
   matchCacheOffset = -1;
   selectorList.increase ();
   cs = selectorList.getRef (selectorList.size () - 1);

   cs->combinator = COMB_NONE;
   cs->simpleSelector = new CssSimpleSelector ();
}

CssSelector::~CssSelector () {
   for (int i = selectorList.size () - 1; i >= 0; i--)
      delete selectorList.getRef (i)->simpleSelector;
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

   struct CombinatorAndSelector *cs = selectorList.getRef (i);
   CssSimpleSelector *sel = cs->simpleSelector;

   switch (comb) {
      case COMB_NONE:
         break;
      case COMB_CHILD:
         node = docTree->parent (node);
         break;
      case COMB_ADJACENT_SIBLING:
         node = docTree->sibling (node);
         break;
      case COMB_DESCENDANT:
         node = docTree->parent (node);
         matchCacheEntry = matchCache->getRef(matchCacheOffset + i);

         for (const DoctreeNode *n = node;
              n && n->num > *matchCacheEntry; n = docTree->parent (n))
            if (sel->simple_selector_matches(n) &&
                full_selector_matches(docTree, n, i - 1, cs->combinator, matchCache))
               return true;

         if (node) // remember that it didn't match to avoid future tests
            *matchCacheEntry = node->num;

         return false;
         break;
      default:
         return false; // \todo implement other combinators
   }

   if (!node || !sel->simple_selector_matches(node))
      return false;

   // tail recursion should be optimized by the compiler
   return full_selector_matches(docTree, node, i - 1, cs->combinator, matchCache);
}

void CssSelector::addSimpleSelector (Combinator c) {
   struct CombinatorAndSelector *cs;

   assert (matchCacheOffset == -1);
   selectorList.increase ();
   cs = selectorList.getRef (selectorList.size () - 1);

   cs->combinator = c;
   cs->simpleSelector = new CssSimpleSelector ();
}

bool CssSelector::checksPseudoClass () {
   for (int i = 0; i < selectorList.size (); i++)
      if (selectorList.getRef (i)->simpleSelector->getSelectorPseudoClass ())
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

   for (int i = 0; i < selectorList.size (); i++)
      spec += selectorList.getRef (i)->simpleSelector->specificity ();

   return spec;
}

void CssSelector::printCssSelector(FILE * file) {
   //fprintf(file, "        Rule SelectorList: Begin\n");
   fprintf(file, "        Rule SelectorList: %d simple selectors\n", selectorList.size());
   for (int i = 0; i < selectorList.size (); i++) {
      selectorList.getRef (i)->simpleSelector->printCssSimpleSelector (file);

      if (i < selectorList.size () - 1) {
         switch (selectorList.getRef (i + 1)->combinator) {
            case COMB_CHILD:
               fprintf (file, "                Rule SelectorList: combinator > \n");
               break;
            case COMB_DESCENDANT:
               fprintf (file, "                Rule SelectorList: combinator \" \" \n");
               break;
            case COMB_ADJACENT_SIBLING:
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

CssSimpleSelector::CssSimpleSelector () {}

CssSimpleSelector::~CssSimpleSelector () {
   for (int i = 0; i < selector_class.size (); i++)
      dFree (selector_class.get (i));
   dFree (selector_id);
   dFree (selector_pseudo_class);
}

void CssSimpleSelector::setSelector(CssSelectorType type, const char *value) {

   switch (type) {
   case CssSelectorType::CLASS:
      //fprintf(stderr, "======= Appending selector CLASS: '%s'\n", value);
         selector_class.increase ();
         selector_class.set (selector_class.size () - 1, dStrdup (value));
         //fprintf(stderr, "======= Updated CLASS:\n");
         for (int i = 0; i < selector_class.size(); i++) {
            //fprintf(stderr, "======= Updated contents %d = '%s'\n", i, selector_class.get(i));
         }
         break;
   case CssSelectorType::PSEUDO_CLASS:
      //fprintf(stderr, "======= Appending selector PSEUDO_CLASS: '%s'\n", value);
         if (selector_pseudo_class == NULL)
            selector_pseudo_class = dStrdup(value);

         break;
   case CssSelectorType::ID:
      //fprintf(stderr, "======= Appending selector ID: '%s'\n", value);
         if (selector_id == NULL)
            selector_id = dStrdup(value);
         break;
      default:
         break;
   }

   if (selector_class.size() > 2) {
      fprintf(stderr, "========== selector_class size = %d\n", selector_class.size());
   }

   if (selector_class.size() && selector_pseudo_class) {
      fprintf(stderr, "========== selector_class + pseudo class\n");
   }
   if (selector_class.size() && selector_id) {
      fprintf(stderr, "========== selector_class + id\n");
   }
   if (selector_pseudo_class && selector_id) {
      fprintf(stderr, "========== selector_pseudo_class + id\n");
   }
   //fprintf(stderr, "=======\n");
}

/**
 * \brief Return whether simple selector matches at a given node of
 *        the document tree.
 */
bool CssSimpleSelector::simple_selector_matches(const DoctreeNode *n) {
   assert (n);
   if (selector_element != ELEMENT_ANY && selector_element != n->html_element_idx)
      return false;
   if (selector_pseudo_class != NULL &&
      (n->pseudo == NULL || dStrAsciiCasecmp (selector_pseudo_class, n->pseudo) != 0))
      return false;
   if (selector_id != NULL && (n->id == NULL || dStrAsciiCasecmp (selector_id, n->id) != 0))
      return false;
   for (int i = 0; i < selector_class.size (); i++) {
      bool found = false;
      if (n->klass != NULL) {
         for (int j = 0; j < n->klass->size (); j++) {
            if (dStrAsciiCasecmp (selector_class.get(i), n->klass->get(j)) == 0) {
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
int CssSimpleSelector::specificity () {
   int spec = 0;

   if (selector_id)
      spec += 1 << 20;
   spec += selector_class.size() << 10;
   if (selector_pseudo_class)
      spec += 1 << 10;
   if (selector_element != ELEMENT_ANY)
      spec += 1;

   return spec;
}

void CssSimpleSelector::printCssSimpleSelector(FILE * file)
{
   //fprintf(file, "                Rule SimpleSelector: Begin\n");
   fprintf(file, "            Rule SimpleSelector: ");

   if (selector_element == ELEMENT_ANY) {
      fprintf(file, "Element ANY\n");
   } else if (selector_element >= 0) {
      fprintf(file, "Element [%s]\n", a_Html_tag_name(selector_element));
   }

   if (selector_pseudo_class) {
      fprintf(file, "            Rule SimpleSelector: selector = Pseudo class [%s]\n", selector_pseudo_class);
   }
   if (selector_id) {
      fprintf(file, "            Rule SimpleSelector: selector = ID %s\n", selector_id);
   }
   if (selector_class.size()) {
      fprintf(file, "            Rule SimpleSelector: selector = class [");
      for (int i = 0; i < selector_class.size (); i++)
         fprintf (file, ".%s", selector_class.get (i));
      fprintf (file, "]\n");
   }
   //fprintf(file, "\n               Rule SimpleSelector: End\n");
   //fprintf(file, "\n            SimpleSelector: --------------\n");
}

CssRule::CssRule (CssSelector *selector, CssDeclartionList * declList, int pos) {
   assert (selector->size () > 0);

   this->selector = selector;
   this->selector->ref ();
   this->declList = declList;
   this->declList->ref ();
   this->pos = pos;
   spec = selector->specificity ();
}

CssRule::~CssRule () {
   selector->unref ();
   declList->unref ();
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
      declList->printCssDeclartionList(file);
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

   while (i > 0 && rule->specificity () < get (i - 1)->specificity ()) {
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

   if (top->getSelectorId ()) {
      string = new lout::object::ConstString (top->getSelectorId ());
      ruleList = idTable.get (string);
      if (ruleList == NULL) {
         ruleList = new RuleList ();
         idTable.put (string, ruleList);
      } else {
         delete string;
      }
   } else if (top->getSelectorClass () && top->getSelectorClass ()->size () > 0) {
      string = new lout::object::ConstString (top->getSelectorClass ()->get (0));
      ruleList = classTable.get (string);
      if (ruleList == NULL) {
         ruleList = new RuleList;
         classTable.put (string, ruleList);
      } else {
         delete string;
      }
   } else if (top->getSelectorElement () >= 0 && top->getSelectorElement () < ntags) {
      ruleList = &elementTable[top->getSelectorElement()];
   } else if (top->getSelectorElement() == CssSimpleSelector::ELEMENT_ANY) {
      ruleList = &anyTable;
   }

   if (ruleList) {
      ruleList->insert (rule);
      if (rule->selector->getRequiredMatchCache () > requiredMatchCache)
         requiredMatchCache = rule->selector->getRequiredMatchCache ();
   } else {
      assert (top->getSelectorElement() == CssSimpleSelector::ELEMENT_NONE);
      delete rule;
   }
}

/**
 * \brief Apply a stylesheet to a property list.
 *
 * The properties are set as defined by the rules in the stylesheet that
 * match at the given node in the document tree.
 */
void CssStyleSheet::apply_style_sheet(CssDeclartionList * declList, Doctree *docTree,
                        const DoctreeNode *node, MatchCache *matchCache) const {
   static const int maxLists = 32;
   const RuleList *ruleList[maxLists];
   int numLists = 0, index[maxLists] = {0};

   if (node->id) {
      lout::object::ConstString idString (node->id);

      ruleList[numLists] = idTable.get (&idString);
      if (ruleList[numLists])
         numLists++;
   }

   if (node->klass) {
      for (int i = 0; i < node->klass->size (); i++) {
         if (i >= maxLists - 4) {
            MSG_WARN("Maximum number of classes per element exceeded.\n");
            break;
         }

         lout::object::ConstString classString (node->klass->get (i));

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
            (rl->get(index[i])->specificity () < minSpec ||
             (rl->get(index[i])->specificity () == minSpec &&
              rl->get(index[i])->position () < minPos))) {

            minSpec = rl->get(index[i])->specificity ();
            minPos = rl->get(index[i])->position ();
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
void CssContext::apply_css_context(CssDeclartionList * declList, Doctree *docTree,
         DoctreeNode *node,
         CssDeclartionList *tagStyle, CssDeclartionList *tagStyleImportant,
         CssDeclartionList *nonCssHints) {

   userAgentSheet.apply_style_sheet(declList, docTree, node, &matchCache);

   sheet[CSS_PRIMARY_USER].apply_style_sheet(declList, docTree, node, &matchCache);

   if (nonCssHints)
        nonCssHints->appendDeclarationsToArg(declList);

   sheet[CSS_PRIMARY_AUTHOR].apply_style_sheet(declList, docTree, node, &matchCache);

   if (tagStyle)
        tagStyle->appendDeclarationsToArg(declList);

   sheet[CSS_PRIMARY_AUTHOR_IMPORTANT].apply_style_sheet(declList, docTree, node,
                                                         &matchCache);

   if (tagStyleImportant)
        tagStyleImportant->appendDeclarationsToArg(declList);

   sheet[CSS_PRIMARY_USER_IMPORTANT].apply_style_sheet(declList, docTree, node, &matchCache);
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
