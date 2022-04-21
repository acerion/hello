#ifndef __DOCTREE_HH__
#define __DOCTREE_HH__

#include "lout/misc.hh"
#include "Hello/hello.h"



void DoctreeNodeDelete(c_doctree_node_t * dtn);
void DoctreeNodePrint(FILE * file, const c_doctree_node_t * dtn);


c_doctree_t * doctreeCtor(void);
void doctreeDtor(c_doctree_t * doctree);

c_doctree_node_t * doctreePushNode(c_doctree_t * doctree);
c_doctree_node_t * doctreeGetTopNode(c_doctree_t * doctree);
void doctreePopNode(c_doctree_t * doctree);




#endif
