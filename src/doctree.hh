#ifndef __DOCTREE_HH__
#define __DOCTREE_HH__

#include "lout/misc.hh"

class DoctreeNode {
   public:
      DoctreeNode *parent;
      DoctreeNode *sibling;
      DoctreeNode *lastChild;
      int num; // unique ascending id
      int html_element_idx; /* Index to html.cc::Tags */
      lout::misc::SimpleVector<char*> * element_class;
      const char *pseudo;
      const char *element_id;

      DoctreeNode () {
         parent = NULL;
         sibling = NULL;
         lastChild = NULL;
         element_class = NULL;
         pseudo = NULL;
         element_id = NULL;
         html_element_idx = 0;
      };

      ~DoctreeNode () {
         dFree ((void*) element_id);
         while (lastChild) {
            DoctreeNode *n = lastChild;
            lastChild = lastChild->sibling;
            delete n;
         }
         if (element_class) {
            for (int i = 0; i < element_class->size (); i++)
               dFree (element_class->get(i));
            delete element_class;
         }
      }
};

/**
 * \brief HTML document tree interface.
 *
 * The Doctree class defines the interface to the parsed HTML document tree
 * as it is used for CSS selector matching.
 */
class Doctree {
   private:
      DoctreeNode *topNode;
      DoctreeNode *rootNode;
      int num;

   public:
      Doctree () {
         rootNode = new DoctreeNode;
         topNode = rootNode;
         num = 0;
      };

      ~Doctree () {
         delete rootNode;
      };

      DoctreeNode *push () {
         DoctreeNode *dn = new DoctreeNode ();
         dn->parent = topNode;
         dn->sibling = dn->parent->lastChild;
         dn->parent->lastChild = dn;
         dn->num = num++;
         topNode = dn;
         return dn;
      };

      void pop () {
         assert (topNode != rootNode); // never pop the root node
         topNode = topNode->parent;
      };

      inline DoctreeNode *top () {
         if (topNode != rootNode)
            return topNode;
         else
            return NULL;
      };

      inline DoctreeNode *parent (const DoctreeNode *node) {
         if (node->parent != rootNode)
            return node->parent;
         else
            return NULL;
      };

      inline DoctreeNode *sibling (const DoctreeNode *node) {
         return node->sibling;
      };
};

#endif
