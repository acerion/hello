#ifndef __DOCTREE_HH__
#define __DOCTREE_HH__

#include "lout/misc.hh"
#include "Hello/hello.h"



c_doctree_node_t * DoctreeNodeNew(c_doctree_node_t * root);
void DoctreeNodeDelete(c_doctree_node_t * dtn);
void DoctreeNodePrint(FILE * file, const c_doctree_node_t * dtn);

/**
 * \brief HTML document tree interface.
 *
 * The Doctree class defines the interface to the parsed HTML document tree
 * as it is used for CSS selector matching.
 */
class Doctree {
   private:
      c_doctree_node_t *topNode;
      c_doctree_node_t *rootNode;
      int num;

   public:
      Doctree () {
         rootNode = DoctreeNodeNew(NULL);
         rootNode->c_root_node = rootNode;
         topNode = rootNode;
         num = 0;
      };

      ~Doctree () {
         delete rootNode;
      };

      c_doctree_node_t *push () {
         c_doctree_node_t * dtn = DoctreeNodeNew(this->rootNode);
         dtn->c_parent = topNode;
         dtn->c_sibling = dtn->c_parent->c_last_child;
         dtn->c_parent->c_last_child = dtn;
         dtn->c_unique_num = num++;
         topNode = dtn;
         return dtn;
      };

      void pop () {
         assert (topNode != rootNode); // never pop the root node
         topNode = topNode->c_parent;
      };

      inline c_doctree_node_t *top () {
         if (topNode != rootNode)
            return topNode;
         else
            return NULL;
      };

};

#endif
