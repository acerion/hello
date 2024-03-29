#ifndef __HTML_COMMON_HH__
#define __HTML_COMMON_HH__

#include "url.h"
#include "bw.h"

#include "lout/misc.hh"
#include "dw/core.hh"
#include "dw/image.hh"
#include "dw/style.hh"
#include "dw/table.hh"

#include "image.hh"

#include "form.hh"

#include "styleengine.hh"

/*
 * Macros
 */

#define Html2TextBlock(html)     ((Textblock*)(html->dw))
#define Html2Layout(html)        ((Layout*)html->bw->render_layout)
#define Image2DwWidget(Image)    ((Widget*)Image->dw)
#define TopOfParsingStack(html)  (html->stack->getRef(html->stack->size()-1))

// Add a bug-meter message.
#define BUG_MSG(...)                               \
   D_STMT_START {                                  \
         html->bugMessage(__VA_ARGS__);            \
   } D_STMT_END


/*
 * Typedefs
 */

typedef enum {
   DT_NONE,
   DT_UNRECOGNIZED,
   DT_HTML,
   DT_XHTML
} DilloHtmlDocumentType;

typedef enum {
   DILLO_HTML_PARSE_MODE_INIT = 0,
   DILLO_HTML_PARSE_MODE_STASH,
   DILLO_HTML_PARSE_MODE_STASH_AND_BODY,
   DILLO_HTML_PARSE_MODE_VERBATIM,
   DILLO_HTML_PARSE_MODE_BODY,
   DILLO_HTML_PARSE_MODE_PRE
} DilloHtmlParseMode;

typedef enum {
   DILLO_HTML_TABLE_MODE_NONE,  /* no table at all */
   DILLO_HTML_TABLE_MODE_TOP,   /* outside of <tr> */
   DILLO_HTML_TABLE_MODE_TR,    /* inside of <tr>, outside of <td> */
   DILLO_HTML_TABLE_MODE_TD     /* inside of <td> */
} DilloHtmlTableMode;

typedef enum {
   DILLO_HTML_TABLE_BORDER_SEPARATE,
   DILLO_HTML_TABLE_BORDER_COLLAPSE
} DilloHtmlTableBorderMode;

typedef enum {
   HTML_LIST_NONE,
   HTML_LIST_UNORDERED,
   HTML_LIST_ORDERED
} DilloHtmlListMode;

typedef enum {
   IN_NONE        = 0,
   IN_HTML        = 1 << 0,
   IN_HEAD        = 1 << 1,
   IN_BODY        = 1 << 2,
   IN_FORM        = 1 << 3,
   IN_SELECT      = 1 << 4,
   IN_OPTION      = 1 << 5,
   IN_OPTGROUP    = 1 << 6,
   IN_TEXTAREA    = 1 << 7,
   IN_BUTTON      = 1 << 8,
   IN_MAP         = 1 << 9,
   IN_PRE         = 1 << 10,
   IN_LI          = 1 << 11,
   IN_MEDIA       = 1 << 12,
   IN_META_HACK   = 1 << 13,
   IN_EOF         = 1 << 14,
} DilloHtmlProcessingState;

/*
 * Data Structures
 */

typedef struct {
   DilloUrl *url;
   DilloImage *image;
} DilloHtmlImage;


typedef struct {
   DilloHtmlTableMode table_mode;
   DilloHtmlTableBorderMode table_border_mode;
   bool cell_text_align_set;

   dw::Table * table_widget;
} TableContext;

typedef struct {
   TableContext table_context;
   DilloHtmlParseMode parse_mode;
   bool display_none;
   DilloHtmlListMode list_type;
   int list_number;

   /* TagInfo index for the tag that's being processed */
   int tag_idx;

   dw::core::Widget *textblock;

   /* This is used to align list items (especially in enumerated lists) */
   dw::core::Widget *ref_list_item;

   /* This is used for list items etc; if it is set to TRUE, breaks
      have to be "handed over" (see Html_add_indented and
      Html_eventually_pop_dw). */
   bool hand_over_break;
} DilloHtmlState;

/*
 * Classes
 */

class DilloHtml {
private:
   class HtmlLinkReceiver: public dw::core::Layout::LinkReceiver {
   public:
      DilloHtml *html;

      bool enter (dw::core::Widget *widget, int link, int img, int x, int y);
      bool press (dw::core::Widget *widget, int link, int img, int x, int y,
                  dw::core::EventButton *event);
      bool click (dw::core::Widget *widget, int link, int img, int x, int y,
                  dw::core::EventButton *event);
   };
   HtmlLinkReceiver linkReceiver;

public:  //BUG: for now everything is public

   BrowserWindow *bw;
   DilloUrl *page_url, *base_url;
   dw::core::Widget *dw;    /* this is duplicated in the stack */

   /* -------------------------------------------------------------------*/
   /* Variables required at parsing time                                 */
   /* -------------------------------------------------------------------*/
   char *Start_Buf;
   int Start_Ofs;
   char *content_type, *charset;
   bool stop_parser;

   size_t CurrOfs, OldOfs, OldLine;

   c_html_doctype_t doctype;

   /* vector of remote CSS resources, as given by the LINK element */
   lout::misc::SimpleVector<DilloUrl*> *cssUrls;

   lout::misc::SimpleVector<DilloHtmlState> *stack;
   StyleEngine *styleEngine;

   int InFlags; /* tracks which elements we are in */

   Dstr *Stash;
   bool StashSpace;

   int pre_column;        /* current column, used in PRE tags with tabs */
   bool PreFirstChar;     /* used to skip the first CR or CRLF in PRE tags */
   bool PrevWasCR;        /* Flag to help parsing of "\r\n" in PRE tags */
   bool PrevWasOpenTag;   /* Flag to help deferred parsing of white space */
   bool InVisitedLink;    /* used to 'contrast_visited_colors' */
   bool ReqTagClose;      /* Flag to help handling bad-formed HTML */
   bool TagSoup;          /* Flag to enable the parser's cleanup functions */
   bool loadCssFromStash; /* current stash content should be loaded as CSS */

   /* element counters: used for validation purposes.
    * ATM they're used as three state flags {0,1,>1} */
   unsigned char Num_HTML, Num_HEAD, Num_BODY, Num_TITLE;

   char attr_data[1024];       /* Buffer for attribute value */

   int32_t non_css_link_color; /* as provided by link attribute in BODY */
   int32_t non_css_visited_color; /* as provided by vlink attribute in BODY */
   int32_t visited_color; /* as computed according to CSS */

   /* -------------------------------------------------------------------*/
   /* Variables required after parsing (for page functionality)          */
   /* -------------------------------------------------------------------*/
   lout::misc::SimpleVector<DilloHtmlForm*> *forms;
   lout::misc::SimpleVector<DilloHtmlInput*> *inputs_outside_form;
   lout::misc::SimpleVector<DilloUrl*> *links;
   lout::misc::SimpleVector<DilloHtmlImage*> *images;
   dw::ImageMapsList maps;

private:
   void freeParseData();
   void initDw();  /* Used by the constructor */

public:
   DilloHtml(BrowserWindow *bw, const DilloUrl *url, const char *content_type);
   ~DilloHtml();
   void bugMessage(const char *format, ... );
   void connectSignals(dw::core::Widget *dw);
   void write(char *Buf, int BufSize, int Eof);
   int getCurrLineNumber();
   void finishParsing(int ClientKey);
   int formNew(DilloHtmlMethod method, const DilloUrl *action,
               DilloHtmlEnc enc, const char *charset);
   DilloHtmlForm *getCurrentForm ();
   bool unloadedImages();
   void loadImages (const DilloUrl *pattern);
   void addCssUrl(const DilloUrl *url);
};

/*
 * Parser functions
 */

const char * a_Html_tag_name(int index);

const char * html_attribute_get_value(const char * document_rem, int tagsize, const char * attr_name);
char * html_attribute_get_value_with_default(const char * document_rem, int tagsize, const char * attr_name, const char *def);

DilloUrl *a_Html_url_new(DilloHtml *html,
                         const char *url_str, const char *base_url,
                         int use_base_url);

void a_Html_common_image_attrs(DilloHtml *html, const char *tag, int tagsize);
DilloImage *a_Html_image_new(DilloHtml *html, const char *tag, int tagsize);

char *a_Html_parse_entities(DilloHtml *html, const char *token, int toksize);
void a_Html_pop_tag(DilloHtml *html, int TagIdx);
void a_Html_stash_init(DilloHtml *html);
int32_t a_Html_color_parse(DilloHtml *html, const char *str,
                           int32_t default_color);
CssLength html_parse_attribute_width_or_height(const char * attr_value);
void a_Html_tag_set_align_attr(c_html_doctype_t * doctype, StyleEngine * styleEngine, const char * tag, int tagsize);
bool a_Html_tag_set_valign_attr(DilloHtml *html,
                                const char *tag, int tagsize);

void a_Html_load_stylesheet(DilloHtml *html, DilloUrl *url);

#endif /* __HTML_COMMON_HH__ */
