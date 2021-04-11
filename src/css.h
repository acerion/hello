#ifndef _CSS_H_
#define _CSS_H_

typedef int CssLength;

typedef enum {
   CSS_LENGTH_TYPE_NONE       = 0,
   CSS_LENGTH_TYPE_PX         = 1,
   CSS_LENGTH_TYPE_MM         = 2, /* "cm", "in", "pt" and "pc" are converted into millimeters. */
   CSS_LENGTH_TYPE_EM         = 3,
   CSS_LENGTH_TYPE_EX         = 4,
   CSS_LENGTH_TYPE_PERCENTAGE = 5,
   CSS_LENGTH_TYPE_RELATIVE   = 6,   /* This does not exist in CSS but is used in HTML */
   CSS_LENGTH_TYPE_AUTO       = 7 /* This can be used as a simple value. */
} CssLengthType;


#endif // #ifndef _CSS_H_
