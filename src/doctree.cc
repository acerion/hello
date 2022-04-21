#include <stdlib.h>
#include "doctree.hh"

void DoctreeNodeDelete(c_doctree_node_t * dtn) {
   free(dtn->c_element_selector_id);

   while (dtn->c_last_child) {
      c_doctree_node_t *n = dtn->c_last_child;
      dtn->c_last_child = dtn->c_last_child->c_sibling;
      DoctreeNodeDelete(n);
   }
   for (int i = 0; i < dtn->c_element_selector_class_size; i++) {
      free(dtn->c_element_selector_class[i]);
   }
}

void DoctreeNodePrint(FILE * file, const c_doctree_node_t * dtn)
{
   fprintf(file,
           "C: dtn: "
           "html elem idx = %d, "
           "pseudo class = '%s', "
           "id = '%s', "
           "class size = %d, "
           "class = [",
           dtn->c_html_element_idx,
           dtn->c_element_selector_pseudo_class,
           dtn->c_element_selector_id,
           dtn->c_element_selector_class_size);
   for (int i = 0; i < dtn->c_element_selector_class_size; i++) {
      fprintf(file, "%s ", dtn->c_element_selector_class[i]);
   }
   fprintf(file, "]\n");
}


c_doctree_node_t * doctreePushNode(c_doctree_t * doctree)
{
   c_doctree_node_t * dtn = hll_doctreeNodeNew();
   dtn->c_root_node = doctree->c_root_node;
   dtn->c_parent = doctree->c_top_node;
   dtn->c_sibling = dtn->c_parent->c_last_child;
   dtn->c_parent->c_last_child = dtn;
   dtn->c_unique_num = doctree->c_num_nodes++;
   doctree->c_top_node = dtn;
   return dtn;
}

c_doctree_node_t * doctreeGetTopNode(c_doctree_t * doctree)
{
   if (doctree->c_top_node != doctree->c_root_node)
      return doctree->c_top_node;
   else
      return NULL;
}

void doctreePopNode(c_doctree_t * doctree)
{
   assert (doctree->c_top_node != doctree->c_root_node); // never pop the root node
   doctree->c_top_node = doctree->c_top_node->c_parent;
}

c_doctree_t * doctreeCtor(void)
{
   c_doctree_t * doctree = (c_doctree_t *) calloc(1, sizeof (c_doctree_t));
   doctree->c_root_node = hll_doctreeNodeNew();
   doctree->c_root_node->c_root_node = doctree->c_root_node;
   doctree->c_top_node = doctree->c_root_node;
   doctree->c_num_nodes = 0;

   return doctree;
}

void doctreeDtor(c_doctree_t * doctree)
{
   free(doctree->c_root_node);
}


