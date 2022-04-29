#include <stdlib.h>
#include "doctree.hh"
#include "html_common.hh"

/*
  Root node is not placed in doctree::c_nodes_array[], it exists as a
  separate member of doctree.

  At the same time c_top_node_num functions (most of the time) as an index to
  doctree::c_nodes_array[]. So when a top node is a root node, the value of
  c_top_node_num must be special.

  This constant is the special value of c_top_node_num.
*/
#define ROOT_NODE_NUM (-1)



void DoctreeNodeDelete(c_doctree_node_t * dtn) {
   free(dtn->c_element_selector_id);
#if 0
   while (dtn->c_last_child) {
      c_doctree_node_t *n = dtn->c_last_child;
      dtn->c_last_child = dtn->c_last_child->c_sibling;
      DoctreeNodeDelete(n);
   }
   for (int i = 0; i < dtn->c_element_selector_class_size; i++) {
      free(dtn->c_element_selector_class[i]);
   }
#endif
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


int doctreePushNode(c_doctree_t * doctree, int element_idx)
{
   fprintf(stderr, "====== push element %d/%s\n", element_idx, a_Html_tag_name(element_idx));
   // fprintf(stderr, "====== push element %s, before push the top node is %s\n", a_Html_tag_name(element_idx), a_Html_tag_name(doctree->c_top_node->c_html_element_idx));

   int this_num = doctree->c_num_nodes;


   /* Allocate. */
   c_doctree_node_t * dtn = hll_doctreeNodeNew();
   fprintf(stderr, "new doctree node = %lu\n", (long unsigned) dtn);


   /* Set properties. */
   dtn->c_unique_num = this_num;
   dtn->c_html_element_idx = element_idx;


   /* Insert into array. */
   doctree->c_nodes_array[this_num] = dtn;


   /* Set relations. */
   c_doctree_node_t * parent = NULL;
   if (doctree->c_top_node_num == ROOT_NODE_NUM) {
      /* This is a first real element in html document, placed in the tree
         under a root element. */
      dtn->c_parent_num = ROOT_NODE_NUM;
      parent = doctree->c_root_node;
   } else {
      /* This is an n-th element in html document, placed in the tree under
         some tree node. */
      dtn->c_parent_num = doctree->c_top_node_num;
      parent = doctree->c_nodes_array[dtn->c_parent_num];
   }
   dtn->c_sibling_num = parent->c_last_child_num;
   parent->c_last_child_num = dtn->c_unique_num;


   /* Update doctree. */
   doctree->c_top_node_num = dtn->c_unique_num;
   doctree->c_num_nodes++;


   /* Debug. */
#if 0
   c_doctree_node_t * d_parent = NULL;
   if (0 == dtn->c_unique_num) {
      d_parent = doctree->c_root_node;
   } else {
      d_parent = doctree->c_nodes_array[dtn->c_parent_num];
   }
   fprintf(stderr, "======    its parent is %s\n", a_Html_tag_name(d_parent->c_html_element_idx));

   if (dtn->c_sibling_num > 0) {
      c_doctree_node_t * sibling = doctree->c_nodes_array[dtn->c_sibling_num];
      fprintf(stderr, "======    its sibling is %s\n", a_Html_tag_name(sibling->c_html_element_idx));

      if (sibling->c_sibling_num > 0) {
         c_doctree_node_t * sibling2 = doctree->c_nodes_array[sibling->c_sibling_num];
         fprintf(stderr, "======        its sibling's sibling is %s\n", a_Html_tag_name(sibling2->c_html_element_idx));
         if (sibling2->c_sibling_num > 0) {
            c_doctree_node_t * sibling3 = doctree->c_nodes_array[sibling2->c_sibling_num];
            fprintf(stderr, "======            its sibling's sibling's sibling is %s\n", a_Html_tag_name(sibling3->c_html_element_idx));
         }
      }
   } else {
      fprintf(stderr, "======    its sibling is NONE\n");
   }
#endif
   //fprintf(stderr, "====== push element %s, after push the top node is %s\n\n", a_Html_tag_name(element_idx), a_Html_tag_name(doctree->c_top_node_num->c_html_element_idx));

#if 0
   int i = 0;
   while (doctree->c_nodes_array[i]) {
      c_doctree_node_t * n = doctree->c_nodes_array[i];
      fprintf(stderr, "===\n");
      fprintf(stderr, "=== push node %d\n", i);
      fprintf(stderr, "=== push dtn: unique num = %d\n", n->c_unique_num);
      fprintf(stderr, "=== push dtn: tag name   = '%s'\n", a_Html_tag_name(n->c_html_element_idx));
      fprintf(stderr, "=== push dtn: parent num = %d\n", n->c_parent_num);
      fprintf(stderr, "=== push dtn: parent ptr = %lu\n", n->c_parent_num);
      fprintf(stderr, "===\n");
      i++;
   }
#endif


   return dtn->c_unique_num;
}

c_doctree_node_t * doctreeGetTopNode(c_doctree_t * doctree)
{
   if (doctree->c_top_node_num != ROOT_NODE_NUM)
      return doctree->c_nodes_array[doctree->c_top_node_num];
   else
      return NULL;
}

void doctreePopNode(c_doctree_t * doctree)
{
   assert (doctree->c_top_node_num != ROOT_NODE_NUM); // never pop the root node

   c_doctree_node_t * dtn = doctree->c_nodes_array[doctree->c_top_node_num];

#if 0
   int i = 0;
   while (doctree->c_nodes_array[i]) {
      c_doctree_node_t * n = doctree->c_nodes_array[i];
      fprintf(stderr, "===\n");
      fprintf(stderr, "=== pop node %d\n", i);
      fprintf(stderr, "=== pop dtn: unique num = %d\n", n->c_unique_num);
      fprintf(stderr, "=== pop dtn: tag name   = '%s'\n", a_Html_tag_name(n->c_html_element_idx));
      fprintf(stderr, "=== pop dtn: parent num = %d\n", n->c_parent_num);
      fprintf(stderr, "=== pop dtn: parent ptr = %lu\n", n->c_parent_num);
      fprintf(stderr, "===\n");
      i++;
   }
#endif

   fprintf(stderr, "====== pop element %d/%s\n", dtn->c_html_element_idx, a_Html_tag_name(dtn->c_html_element_idx));
#if 0
   fprintf(stderr, "======    its parent is %s\n", a_Html_tag_name(dtn->c_parent_num->c_html_element_idx));
   if (dtn->c_sibling) {
      fprintf(stderr, "======    its sibling is %s\n", a_Html_tag_name(dtn->c_sibling->c_html_element_idx));
      if (dtn->c_sibling->c_sibling) {
         fprintf(stderr, "======        its sibling's sibling is %s\n", a_Html_tag_name(dtn->c_sibling->c_sibling->c_html_element_idx));
         if (dtn->c_sibling->c_sibling->c_sibling) {
            fprintf(stderr, "======            its sibling's sibling's sibling is %s\n", a_Html_tag_name(dtn->c_sibling->c_sibling->c_sibling->c_html_element_idx));
         }
      }
   } else {
      fprintf(stderr, "======    its sibling is NONE\n");
   }
#endif

   if (0 == dtn->c_unique_num) {
      /* We are popping the element of html document that was added to the
         tree as the first one. What should now remain on top of the doctree
         is a tree's root element. */
      doctree->c_top_node_num = ROOT_NODE_NUM;
   } else {
      doctree->c_top_node_num = dtn->c_parent_num;
   }
   //fprintf(stderr, "====== pop element %s, after pop the top node is %s\n\n", a_Html_tag_name(element_idx), a_Html_tag_name(doctree->c_top_node->c_html_element_idx));

#if 0
   if (element_idx == 42) { /* html element */
      hll_doctreePrint(doctree);
      if (0 != hll_doctreeTest(doctree)) {
         fprintf(stderr, "doctree test has failed\n");
         exit(EXIT_FAILURE);
      }
   }
#endif
}

c_doctree_t * doctreeCtor(void)
{
   c_doctree_t * doctree = (c_doctree_t *) calloc(1, sizeof (c_doctree_t));
   memset(doctree->c_nodes_array, 0, sizeof (doctree->c_nodes_array[0]) * 2048);
   fprintf(stderr, "doctree pointer = %lu\n", (long unsigned) doctree);
   doctree->c_root_node = hll_doctreeNodeNew();
   fprintf(stderr, "root node pointer = %lu\n", (long unsigned) doctree->c_root_node);
   doctree->c_top_node_num = ROOT_NODE_NUM;
   doctree->c_num_nodes = 0;

   return doctree;
}

void doctreeDtor(c_doctree_t * doctree)
{
   free(doctree->c_root_node);
}


