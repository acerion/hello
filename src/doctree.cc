#include <stdlib.h>
#include "doctree.hh"

c_doctree_node_t * DoctreeNodeNew() {
   c_doctree_node_t * dtn = (c_doctree_node_t *) calloc(1, sizeof (c_doctree_node_t));
   return dtn;
};

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
