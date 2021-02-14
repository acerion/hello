#ifndef _H_GIF_
#define _H_GIF_

#include <stdbool.h>


typedef struct hll_Gif {

   /* [1] "23. Graphic Control Extension.", Required version: Gif89. */
   int transparentColorIndexC;
#if 1
   int delayTimeC;
   int userInputFlagC;
   int disposalMethodC;
#endif

} hll_Gif;


#endif /* #ifndef _H_GIF_ */
