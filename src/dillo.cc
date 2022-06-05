/*
 * Dillo web browser
 *
 * Copyright 1999-2007 Jorge Arellano Cid <jcid@dillo.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <locale.h>
#include <errno.h>
#include <math.h>

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/fl_ask.H>
#include <FL/fl_draw.H>

#include "msg.h"
#include "paths.hh"
#include "uicmd.hh"

#include "prefs.h"
#include "prefsparser.hh"
#include "keys.hh"
#include "bw.h"
#include "misc.h"
#include "history.h"

#include "dns.h"
#include "web.hh"
#include "IO/Url.h"
#include "IO/mime.h"
#include "capi.h"
#include "dicache.h"
#include "cookies.h"
#include "domain.h"
#include "auth.h"
#include "css.hh"
#include "styleengine.hh"

#include "lout/debug.hh"
#include "dw/fltkcore.hh"
#include "dw/textblock.hh"

/*
 * Command line options structure
 */
typedef enum {
   DILLO_CLI_NONE          = 0,
   DILLO_CLI_XID           = 1 << 0,
   DILLO_CLI_FULLWINDOW    = 1 << 1,
   DILLO_CLI_HELP          = 1 << 2,
   DILLO_CLI_VERSION       = 1 << 3,
   DILLO_CLI_LOCAL         = 1 << 4,
   DILLO_CLI_GEOMETRY      = 1 << 5,
   DILLO_CLI_ERROR         = 1 << 15,
} OptID;

typedef struct {
   const char *shortopt;
   const char *longopt;
   int opt_argc; /* positive: mandatory, negative: optional */
   OptID id;
   const char *help;
} CLI_options;

static const CLI_options Options[] = {
   {"-f", "--fullwindow", 0, DILLO_CLI_FULLWINDOW,
    "  -f, --fullwindow       Start in full window mode: hide address bar,\n"
    "                         navigation buttons, menu, and status bar."},
   {"-g", "-geometry",    1, DILLO_CLI_GEOMETRY,
    "  -g, -geometry GEO      Set initial window position where GEO is\n"
    "                         WxH[{+-}X{+-}Y]"},
   {"-h", "--help",       0, DILLO_CLI_HELP,
    "  -h, --help             Display this help text and exit."},
   {"-l", "--local",      0, DILLO_CLI_LOCAL,
    "  -l, --local            Don't load images or stylesheets, or follow\n"
    "                         redirections, for these FILEs or URLs."},
   {"-v", "--version",    0, DILLO_CLI_VERSION,
    "  -v, --version          Display version info and exit."},
   {"-x", "--xid",        1, DILLO_CLI_XID,
    "  -x, --xid XID          Open first Dillo window in an existing\n"
    "                         window whose window ID is XID."},
   {NULL, NULL, 0, DILLO_CLI_NONE, NULL}
};


void css_length_test(void);

/*
 * SIGCHLD handling ----------------------------------------------------------
 */

/*
 * Avoid our children (Dpid) to become zombies. :-)
 * Notes:
 *   . We let sigaction block SIGCHLD while in the handler.
 *   . Portability is not simple. e.g.
 *       http://www.faqs.org/faqs/unix-faq/faq/part3/section-13.html
 *       man sigaction, waitpid
 */
static void raw_sigchld2(int signum)
{
   pid_t pid;
   int status;

   while (1) {
      pid = waitpid(-1, &status, WNOHANG);
      if (pid > 0) {
         if (WIFEXITED(status))        /* normal exit */
            printf("[dpid]: terminated normally (%d)\n", WEXITSTATUS(status));
         else if (WIFSIGNALED(status)) /* terminated by signal */
            printf("[dpid]: terminated by signal %d\n", WTERMSIG(status));
      } else if (pid == 0 || errno == ECHILD) {
         break;
      } else {
         if (errno == EINTR)
            continue;
         perror("waitpid");
         break;
      }
   }
   ++signum; /* compiler happiness */
}

/*
 * Establish SIGCHLD handler
 */
static void est_sigchld(void)
{
   struct sigaction sigact;
   sigset_t set;

   (void) sigemptyset(&set);
   sigact.sa_handler = raw_sigchld2; /* our custom handler */
   sigact.sa_mask = set;             /* no aditional signal blocks */
   sigact.sa_flags = SA_NOCLDSTOP;   /* ignore stop/resume states */
   if (sigaction(SIGCHLD, &sigact, NULL) == -1) {
      perror("sigaction");
      exit(1);
   }
}

//----------------------------------------------------------------------------

/*
 * Print help text generated from the options structure
 */
static void printHelp(const char *cmdname, const CLI_options *options)
{
   printf("Usage: %s [OPTION]... [--] [URL|FILE]...\n"
          "Options:\n", cmdname);
   while (options && options->help) {
      printf("%s\n", options->help);
      options++;
   }
   printf("  URL                    URL to browse.\n"
          "  FILE                   Local FILE to view.\n"
          "\n");
}

/*
 * Return the maximum number of option arguments
 */
static int numOptions(const CLI_options *options)
{
   int i, max;

   for (i = 0, max = 0; options[i].shortopt; i++)
      if (abs(options[i].opt_argc) > max)
         max = abs(options[i].opt_argc);
   return max;
}

/*
 * Get next command line option.
 */
static OptID getCmdOption(const CLI_options *options, int argc, char **argv,
                           char **opt_argv, int *idx)
{
   typedef enum { O_SEARCH, O_FOUND, O_NOTFOUND, O_DONE } State;
   OptID opt_id = DILLO_CLI_NONE;
   int i = 0;
   State state = O_SEARCH;

   if (*idx >= argc) {
      state = O_DONE;
   } else {
      state = O_NOTFOUND;
      for (i = 0; options[i].shortopt; i++) {
         if (strcmp(options[i].shortopt, argv[*idx]) == 0 ||
             strcmp(options[i].longopt, argv[*idx]) == 0) {
            state = O_FOUND;
            ++*idx;
            break;
         }
      }
   }
   if (state == O_FOUND) {
      int n_arg = options[i].opt_argc;
      opt_id = options[i].id;
      /* Find the required/optional arguments of the option */
      for (i = 0; *idx < argc && i < abs(n_arg) && argv[*idx][0] != '-'; i++)
         opt_argv[i] = argv[(*idx)++];
      opt_argv[i] = NULL;

      /* Optional arguments have opt_argc < 0 */
      if (i < n_arg) {
         fprintf(stderr, "Option %s requires %d argument%s\n",
                 argv[*idx-i-1], n_arg, (n_arg == 1) ? "" : "s");
         opt_id = DILLO_CLI_ERROR;
      }
   }
   if (state == O_NOTFOUND) {
      if (strcmp(argv[*idx], "--") == 0)
         (*idx)++;
      else if (argv[*idx][0] == '-') {
         fprintf(stderr, "Command line option \"%s\" not recognized.\n",
                 argv[*idx]);
         opt_id = DILLO_CLI_ERROR;
      }
   }
   return opt_id;
}

/*
 * Set FL_NORMAL_LABEL to interpret neither symbols (@) nor shortcuts (&),
 * and FL_FREE_LABELTYPE to interpret shortcuts.
 */
static void custLabelDraw(const Fl_Label* o, int X, int Y, int W, int H,
                          Fl_Align align)
{
   const int interpret_symbols = 0;

   fl_draw_shortcut = 0;
   fl_font(o->font, o->size);
   fl_color((Fl_Color)o->color);
   fl_draw(o->value, X, Y, W, H, align, o->image, interpret_symbols);
}

static void custLabelMeasure(const Fl_Label* o, int& W, int& H)
{
   const int interpret_symbols = 0;

   fl_draw_shortcut = 0;
   fl_font(o->font, o->size);
   fl_measure(o->value, W, H, interpret_symbols);
}

static void custMenuLabelDraw(const Fl_Label* o, int X, int Y, int W, int H,
                              Fl_Align align)
{
   const int interpret_symbols = 0;

   fl_draw_shortcut = 1;
   fl_font(o->font, o->size);
   fl_color((Fl_Color)o->color);
   fl_draw(o->value, X, Y, W, H, align, o->image, interpret_symbols);
}

static void custMenuLabelMeasure(const Fl_Label* o, int& W, int& H)
{
   const int interpret_symbols = 0;

   fl_draw_shortcut = 1;
   fl_font(o->font, o->size);
   fl_measure(o->value, W, H, interpret_symbols);
}

/*
 * Tell the user if default/pref fonts can't be found.
 */
static void checkFont(const char *name, const char *type)
{
   if (! dw::fltk::FltkFont::fontExists(name))
      MSG_WARN("preferred %s font \"%s\" not found.\n", type, name);
}

static void checkPreferredFonts()
{
   checkFont(prefs.font_sans_serif, "sans-serif");
   checkFont(prefs.font_serif, "serif");
   checkFont(prefs.font_monospace, "monospace");
   checkFont(prefs.font_cursive, "cursive");
   checkFont(prefs.font_fantasy, "fantasy");
}

/*
 * Set UI color. 'color' is an 0xrrggbb value, whereas 'default_val' is a fltk
 * color (index 0-0xFF, or 0xrrggbb00).
 */
static void setUIColorWdef(Fl_Color idx, int32_t color, Fl_Color default_val)
{
   if (color != -1)
      Fl::set_color(idx, color << 8);
   else if (default_val != 0xFFFFFFFF)
      Fl::set_color(idx, default_val);
}

static void setColors()
{
   /* The main background is a special case because Fl::background() will
    * set the "gray ramp", which is a set of lighter and darker colors based
    * on the main background and used for box edges and such.
    */
   if (prefs.ui_main_bg_color != -1) {
      uchar r = prefs.ui_main_bg_color >> 16,
            g = prefs.ui_main_bg_color >> 8 & 0xff,
            b = prefs.ui_main_bg_color & 0xff;

      Fl::background(r, g, b);
   }

   setUIColorWdef(FL_BACKGROUND2_COLOR, prefs.ui_text_bg_color, 0xFFFFFFFF);
   setUIColorWdef(FL_FOREGROUND_COLOR, prefs.ui_fg_color, 0xFFFFFFFF);
   setUIColorWdef(FL_SELECTION_COLOR, prefs.ui_selection_color,
                  fl_contrast(FL_SELECTION_COLOR, FL_BACKGROUND2_COLOR));
   setUIColorWdef(PREFS_UI_BUTTON_HIGHLIGHT_COLOR,
                  prefs.ui_button_highlight_color,
                  fl_lighter(FL_BACKGROUND_COLOR));
   setUIColorWdef(PREFS_UI_TAB_ACTIVE_BG_COLOR, prefs.ui_tab_active_bg_color,
                  Fl::get_color(FL_BACKGROUND2_COLOR));
   setUIColorWdef(PREFS_UI_TAB_BG_COLOR, prefs.ui_tab_bg_color,
                  Fl::get_color(FL_BACKGROUND_COLOR));
   setUIColorWdef(PREFS_UI_TAB_ACTIVE_FG_COLOR, prefs.ui_tab_active_fg_color,
                  Fl::get_color(FL_FOREGROUND_COLOR));
   setUIColorWdef(PREFS_UI_TAB_FG_COLOR, prefs.ui_tab_fg_color,
                  Fl::get_color(FL_FOREGROUND_COLOR));
}

/*
 * Given a command line argument, build a DilloUrl for it.
 */
static DilloUrl *makeStartUrl(char *str, bool local)
{
   char *url_str, *p;
   DilloUrl *start_url;

   /* Relative path to a local file? */
   p = (*str == '/') ? dStrdup(str) :
                       dStrconcat(Paths::getOldWorkingDir(), "/", str, NULL);

   if (access(p, F_OK) == 0) {
      /* absolute path may have non-URL characters */
      url_str = a_Misc_escape_chars(p, "% #");
      start_url = a_Url_new(url_str + 1, "file:/");
   } else {
      /* Not a file, filter URL string */
      url_str = a_Url_string_strip_delimiters(str);
      start_url = a_Url_new(url_str, NULL);
   }
   dFree(p);
   dFree(url_str);

   if (local)
      a_Url_set_flags(start_url, URL_FLAGS(start_url) | URL_SpamSafe);

   return start_url;
}

/*
 * MAIN
 */
int main(int argc, char **argv)
{
   DBG_OBJ_COLOR("#c0ff80", "dw::*");
   DBG_OBJ_COLOR("#c0c0ff", "dw::fltk::*");
   DBG_OBJ_COLOR("#ffa0a0", "dw::core::*");
   DBG_OBJ_COLOR("#ffe0a0", "dw::core::style::*");

   css_length_test();

   uint_t opt_id;
   uint_t options_got = 0;
   uint32_t xid = 0;
   int idx = 1;
   int xpos = PREFS_GEOMETRY_DEFAULT_XPOS, ypos = PREFS_GEOMETRY_DEFAULT_YPOS,
       width = PREFS_GEOMETRY_DEFAULT_WIDTH,
       height = PREFS_GEOMETRY_DEFAULT_HEIGHT;
   char **opt_argv;
   FILE *fp;

   srand((uint_t)(time(0) ^ getpid()));

   // Some OSes exit dillo without this (not GNU/Linux).
   signal(SIGPIPE, SIG_IGN);
   // Establish our custom SIGCHLD handler
   est_sigchld();

   /* Handle command line options */
   opt_argv = dNew0(char*, numOptions(Options) + 1);
   while ((opt_id = getCmdOption(Options, argc, argv, opt_argv, &idx))) {
      options_got |= opt_id;
      switch (opt_id) {
      case DILLO_CLI_FULLWINDOW:
      case DILLO_CLI_LOCAL:
         break;
      case DILLO_CLI_XID:
      {
         char *end;
         xid = strtol(opt_argv[0], &end, 0);
         if (*end) {
            fprintf(stderr, "XID argument \"%s\" not valid.\n",opt_argv[0]);
            return 2;
         }
         break;
      }
      case DILLO_CLI_GEOMETRY:
         if (!a_Misc_parse_geometry(opt_argv[0],&xpos,&ypos,&width,&height)){
            fprintf(stderr, "geometry argument \"%s\" not valid. Must be of "
                            "the form WxH[{+-}X{+-}Y].\n", opt_argv[0]);
            return 2;
         }
         break;
      case DILLO_CLI_VERSION:
         puts("Dillo version " VERSION);
         return 0;
      case DILLO_CLI_HELP:
         printHelp(argv[0], Options);
         return 0;
      default:
         printHelp(argv[0], Options);
         return 2;
      }
   }
   dFree(opt_argv);

   // set the default values for the preferences
   a_Prefs_init();

   // create ~/.dillo if not present
   Paths::init();

   // initialize default key bindings
   Keys::init();

   // parse dillorc
   if ((fp = Paths::getPrefsFP(PATHS_RC_PREFS))) {
      PrefsParser::parse(fp);
   }
   // parse keysrc
   if ((fp = Paths::getPrefsFP(PATHS_RC_KEYS))) {
      Keys::parse(fp);
   }
   // parse domainrc
   if ((fp = Paths::getPrefsFP(PATHS_RC_DOMAIN))) {
      a_Domain_parse(fp);
      fclose(fp);
   }
   dLib_show_messages(prefs.show_msg);

   // initialize internal modules
   a_Dpi_init();
   a_Dns_init();
   a_Web_init();
   a_Http_init();
   a_Mime_init();
   a_Capi_init();
   a_Dicache_init();
   a_Bw_init();
   a_Cookies_init();
   a_Auth_init();
   a_UIcmd_init();

   dw::Textblock::setPenaltyHyphen (prefs.penalty_hyphen);
   dw::Textblock::setPenaltyHyphen2 (prefs.penalty_hyphen_2);
   dw::Textblock::setPenaltyEmDashLeft (prefs.penalty_em_dash_left);
   dw::Textblock::setPenaltyEmDashRight (prefs.penalty_em_dash_right);
   dw::Textblock::setPenaltyEmDashRight2 (prefs.penalty_em_dash_right_2);
   dw::Textblock::setStretchabilityFactor (prefs.stretchability_factor);

   /* command line options override preferences */
   if (options_got & DILLO_CLI_FULLWINDOW)
      prefs.fullwindow_start = TRUE;
   if (options_got & DILLO_CLI_GEOMETRY) {
       prefs.width = width;
       prefs.height = height;
       prefs.xpos = xpos;
       prefs.ypos = ypos;
   }

   // Sets WM_CLASS hint on X11
   Fl_Window::default_xclass("dillo");

   Fl::scheme(prefs.theme);

   // Disable drag and drop as it crashes on MacOSX
   Fl::dnd_text_ops(0);

   setColors();

   if (!prefs.show_ui_tooltip) {
      Fl::option(Fl::OPTION_SHOW_TOOLTIPS, false);
   }

   // Disable '@' and '&' interpretation in normal labels.
   Fl::set_labeltype(FL_NORMAL_LABEL, custLabelDraw, custLabelMeasure);

   // Use to permit '&' interpretation.
   Fl::set_labeltype(FL_FREE_LABELTYPE,custMenuLabelDraw,custMenuLabelMeasure);

   checkPreferredFonts();

   /* use preferred font for UI */
   Fl_Font defaultFont = dw::fltk::FltkFont::get (prefs.font_sans_serif, 0);
   Fl::set_font(FL_HELVETICA, defaultFont); // this seems to be the
                                            // only way to set the
                                            // default font in fltk1.3

   fl_message_title_default("Dillo: Message");

   // Create a new UI/bw pair
   BrowserWindow *bw = a_UIcmd_browser_window_new(0, 0, xid, NULL);

   /* We need this so that fl_text_extents() in dw/fltkplatform.cc can
    * work when FLTK is configured without XFT and Dillo is opening
    * immediately-available URLs from the cmdline (e.g. about:splash).
    */
   ((Fl_Widget *)bw->ui)->window()->make_current();

   /* Proxy authentication */
   if (prefs.http_proxyuser && !a_Http_proxy_auth()) {
      const char *passwd = a_UIcmd_get_passwd(prefs.http_proxyuser);
      if (passwd) {
         a_Http_set_proxy_passwd(passwd);
      } else {
         MSG_WARN("Not using proxy authentication.\n");
      }
   }

   /* Open URLs/files */
   const bool local = options_got & DILLO_CLI_LOCAL;

   if (idx == argc) {
      /* No URLs/files on cmdline. Send startup screen */
      if (dStrAsciiCasecmp(URL_SCHEME(prefs.start_page), "about") == 0 &&
          strcmp(URL_PATH(prefs.start_page), "blank") == 0)
         a_UIcmd_open_url(bw, NULL); // NULL URL focuses location
      else {
         a_UIcmd_open_url(bw, prefs.start_page);
         a_UIcmd_set_location_text(bw, URL_STR(prefs.start_page));
      }
   } else {
      for (int i = idx; i < argc; i++) {
         DilloUrl *start_url = makeStartUrl(argv[i], local);

         if (i > idx) {
            if (prefs.middle_click_opens_new_tab) {
               /* user must prefer tabs */
               const int focus = 1;
               a_UIcmd_open_url_nt(bw, start_url, focus);
            } else {
               a_UIcmd_open_url_nw(bw, start_url);
            }
         } else {
            a_UIcmd_open_url(bw, start_url);
            a_UIcmd_set_location_text(bw, URL_STR(start_url));
         }
         a_Url_free(start_url);
      }
   }

   Fl::run();

   /*
    * Memory deallocating routines
    * (This can be left to the OS, but we'll do it, with a view to test
    *  and fix our memory management)
    */
   a_Domain_freeall();
   a_Cookies_freeall();
   a_Cache_freeall();
   a_Dicache_freeall();
   a_Http_freeall();
   a_Dns_freeall();
   a_History_freeall();
   a_Prefs_freeall();
   Keys::free();
   Paths::free();
   dw::core::freeall();
   dw::fltk::freeall();
   /* TODO: auth, css */

   //a_Dpi_dillo_exit();
   MSG("Dillo: normal exit!\n");
   return 0;
}




struct cssl {
       int lengthtype;
       float inValue;
       unsigned int cssLength;
} css_length_test_data[] = {
{ .lengthtype = 0, .inValue = 0.000000, .cssLength = 0x00000000 },
//{ .lengthtype = 0, .inValue = 0.000630, .cssLength = 0x00000010 }, // 0.000488
{ .lengthtype = 0, .inValue = 0.012500, .cssLength = 0x00000198 },
{ .lengthtype = 0, .inValue = 0.031250, .cssLength = 0x00000400 },
{ .lengthtype = 0, .inValue = -0.031250, .cssLength = 0xfffffc00 },
{ .lengthtype = 0, .inValue = 0.062500, .cssLength = 0x00000800 },
{ .lengthtype = 0, .inValue = -0.062500, .cssLength = 0xfffff800 },
{ .lengthtype = 0, .inValue = 0.125000, .cssLength = 0x00001000 },
{ .lengthtype = 0, .inValue = -0.125000, .cssLength = 0xfffff000 },
{ .lengthtype = 0, .inValue = 0.187500, .cssLength = 0x00001800 },
{ .lengthtype = 0, .inValue = -0.187500, .cssLength = 0xffffe800 },
{ .lengthtype = 0, .inValue = 0.208330, .cssLength = 0x00001aa8 },
{ .lengthtype = 0, .inValue = 0.250000, .cssLength = 0x00002000 },
{ .lengthtype = 0, .inValue = -0.250000, .cssLength = 0xffffe000 },
{ .lengthtype = 0, .inValue = 0.312500, .cssLength = 0x00002800 },
{ .lengthtype = 0, .inValue = -0.312500, .cssLength = 0xffffd800 },
{ .lengthtype = 0, .inValue = 0.375000, .cssLength = 0x00003000 },
{ .lengthtype = 0, .inValue = -0.375000, .cssLength = 0xffffd000 },
{ .lengthtype = 0, .inValue = 0.416670, .cssLength = 0x00003550 },
{ .lengthtype = 0, .inValue = 0.437500, .cssLength = 0x00003800 },
{ .lengthtype = 0, .inValue = 0.468750, .cssLength = 0x00003c00 },
{ .lengthtype = 0, .inValue = 0.500000, .cssLength = 0x00004000 },
{ .lengthtype = 0, .inValue = -0.500000, .cssLength = 0xffffc000 },
{ .lengthtype = 0, .inValue = 0.531250, .cssLength = 0x00004400 },
{ .lengthtype = 0, .inValue = 0.562500, .cssLength = 0x00004800 },
{ .lengthtype = 0, .inValue = 0.573750, .cssLength = 0x00004970 },
{ .lengthtype = 0, .inValue = 0.625000, .cssLength = 0x00005000 },
{ .lengthtype = 0, .inValue = -0.625000, .cssLength = 0xffffb000 },
{ .lengthtype = 0, .inValue = 0.675000, .cssLength = 0x00005660 },
{ .lengthtype = 0, .inValue = 0.687500, .cssLength = 0x00005800 },
{ .lengthtype = 0, .inValue = 0.750000, .cssLength = 0x00006000 },
{ .lengthtype = 0, .inValue = -0.750000, .cssLength = 0xffffa000 },
{ .lengthtype = 0, .inValue = 0.810000, .cssLength = 0x000067a8 },
{ .lengthtype = 0, .inValue = 0.812500, .cssLength = 0x00006800 },
{ .lengthtype = 0, .inValue = 0.875000, .cssLength = 0x00007000 },
{ .lengthtype = 0, .inValue = 0.916670, .cssLength = 0x00007550 },
{ .lengthtype = 0, .inValue = -0.916670, .cssLength = 0xffff8aa8 },
{ .lengthtype = 0, .inValue = 0.937500, .cssLength = 0x00007800 },
{ .lengthtype = 0, .inValue = -0.937500, .cssLength = 0xffff8800 },
{ .lengthtype = 0, .inValue = 0.962500, .cssLength = 0x00007b30 },
{ .lengthtype = 0, .inValue = 100.000000, .cssLength = 0x00320000 },
{ .lengthtype = 0, .inValue = 10.000000, .cssLength = 0x00050000 },
{ .lengthtype = 0, .inValue = 1.000000, .cssLength = 0x00008000 },
{ .lengthtype = 0, .inValue = -1.000000, .cssLength = 0xffff8000 },
{ .lengthtype = 0, .inValue = 1.027780, .cssLength = 0x00008388 },
{ .lengthtype = 0, .inValue = 1.034480, .cssLength = 0x00008468 },
{ .lengthtype = 0, .inValue = 10.500000, .cssLength = 0x00054000 },
{ .lengthtype = 0, .inValue = 1.062500, .cssLength = 0x00008800 },
{ .lengthtype = 0, .inValue = 1.066670, .cssLength = 0x00008888 },
{ .lengthtype = 0, .inValue = 1.076920, .cssLength = 0x000089d8 },
{ .lengthtype = 0, .inValue = 1.086960, .cssLength = 0x00008b20 },
{ .lengthtype = 0, .inValue = 10.937500, .cssLength = 0x00057800 },
{ .lengthtype = 0, .inValue = 1.095240, .cssLength = 0x00008c30 },
{ .lengthtype = 0, .inValue = 1.100000, .cssLength = 0x00008cc8 },
{ .lengthtype = 0, .inValue = 1.111110, .cssLength = 0x00008e38 },
{ .lengthtype = 0, .inValue = 1.125000, .cssLength = 0x00009000 },
{ .lengthtype = 0, .inValue = -1.125000, .cssLength = 0xffff7000 },
{ .lengthtype = 0, .inValue = 1.133330, .cssLength = 0x00009110 },
{ .lengthtype = 0, .inValue = 1.136360, .cssLength = 0x00009170 },
{ .lengthtype = 0, .inValue = 1.142860, .cssLength = 0x00009248 },
{ .lengthtype = 0, .inValue = 1.153850, .cssLength = 0x000093b0 },
{ .lengthtype = 0, .inValue = 1.166670, .cssLength = 0x00009550 },
{ .lengthtype = 0, .inValue = 11.875000, .cssLength = 0x0005f000 },
{ .lengthtype = 0, .inValue = 1.200000, .cssLength = 0x00009998 },
{ .lengthtype = 0, .inValue = 1.230770, .cssLength = 0x00009d88 },
{ .lengthtype = 0, .inValue = 1.250000, .cssLength = 0x0000a000 },
{ .lengthtype = 0, .inValue = -1.250000, .cssLength = 0xffff6000 },
{ .lengthtype = 0, .inValue = -12.812500, .cssLength = 0xfff99800 },
{ .lengthtype = 0, .inValue = 1.285710, .cssLength = 0x0000a490 },
{ .lengthtype = 0, .inValue = 1.294120, .cssLength = 0x0000a5a0 },
{ .lengthtype = 0, .inValue = 1.300000, .cssLength = 0x0000a660 },
{ .lengthtype = 0, .inValue = 1.304350, .cssLength = 0x0000a6f0 },
{ .lengthtype = 0, .inValue = 1.312500, .cssLength = 0x0000a800 },
{ .lengthtype = 0, .inValue = 1.333330, .cssLength = 0x0000aaa8 },
{ .lengthtype = 0, .inValue = 1.363640, .cssLength = 0x0000ae88 },
{ .lengthtype = 0, .inValue = 13.750000, .cssLength = 0x0006e000 },
{ .lengthtype = 0, .inValue = 1.375000, .cssLength = 0x0000b000 },
{ .lengthtype = 0, .inValue = 1.400000, .cssLength = 0x0000b330 },
{ .lengthtype = 0, .inValue = 1.428570, .cssLength = 0x0000b6d8 },
{ .lengthtype = 0, .inValue = 1.437500, .cssLength = 0x0000b800 },
{ .lengthtype = 0, .inValue = -1.437500, .cssLength = 0xffff4800 },
{ .lengthtype = 0, .inValue = 1.444440, .cssLength = 0x0000b8e0 },
{ .lengthtype = 0, .inValue = 1.461540, .cssLength = 0x0000bb10 },
{ .lengthtype = 0, .inValue = 1.466670, .cssLength = 0x0000bbb8 },
{ .lengthtype = 0, .inValue = 14.687500, .cssLength = 0x00075800 },
{ .lengthtype = 0, .inValue = 15.000000, .cssLength = 0x00078000 },
{ .lengthtype = 0, .inValue = 1.500000, .cssLength = 0x0000c000 },
{ .lengthtype = 0, .inValue = -1.500000, .cssLength = 0xffff4000 },
{ .lengthtype = 0, .inValue = 1.533330, .cssLength = 0x0000c440 },
{ .lengthtype = 0, .inValue = 1.555560, .cssLength = 0x0000c718 },
{ .lengthtype = 0, .inValue = 15.625000, .cssLength = 0x0007d000 },
{ .lengthtype = 0, .inValue = 1.562500, .cssLength = 0x0000c800 },
{ .lengthtype = 0, .inValue = 1.571430, .cssLength = 0x0000c920 },
{ .lengthtype = 0, .inValue = 16.000000, .cssLength = 0x00080000 },
{ .lengthtype = 0, .inValue = 1.600000, .cssLength = 0x0000ccc8 },
{ .lengthtype = 0, .inValue = 1.625000, .cssLength = 0x0000d000 },
{ .lengthtype = 0, .inValue = 1.636360, .cssLength = 0x0000d170 },
{ .lengthtype = 0, .inValue = 1.642860, .cssLength = 0x0000d248 },
{ .lengthtype = 0, .inValue = 1.666670, .cssLength = 0x0000d550 },
{ .lengthtype = 0, .inValue = 1.687500, .cssLength = 0x0000d800 },
{ .lengthtype = 0, .inValue = 1.700000, .cssLength = 0x0000d998 },
{ .lengthtype = 0, .inValue = 1.733330, .cssLength = 0x0000ddd8 },
{ .lengthtype = 0, .inValue = 1.750000, .cssLength = 0x0000e000 },
{ .lengthtype = 0, .inValue = 1.764710, .cssLength = 0x0000e1e0 },
{ .lengthtype = 0, .inValue = 18.000000, .cssLength = 0x00090000 },
{ .lengthtype = 0, .inValue = 1.800000, .cssLength = 0x0000e660 },
{ .lengthtype = 0, .inValue = 1.812500, .cssLength = 0x0000e800 },
{ .lengthtype = 0, .inValue = 18.437500, .cssLength = 0x00093800 },
{ .lengthtype = 0, .inValue = 1.866670, .cssLength = 0x0000eee8 },
{ .lengthtype = 0, .inValue = 18.750000, .cssLength = 0x00096000 },
{ .lengthtype = 0, .inValue = 1.875000, .cssLength = 0x0000f000 },
{ .lengthtype = 0, .inValue = -1.875000, .cssLength = 0xffff1000 },
{ .lengthtype = 0, .inValue = 1.937500, .cssLength = 0x0000f800 },
{ .lengthtype = 0, .inValue = 19.583330, .cssLength = 0x0009caa8 },
{ .lengthtype = 0, .inValue = 20.000000, .cssLength = 0x000a0000 },
{ .lengthtype = 0, .inValue = 2.000000, .cssLength = 0x00010000 },
{ .lengthtype = 0, .inValue = 2.062500, .cssLength = 0x00010800 },
{ .lengthtype = 0, .inValue = 2.125000, .cssLength = 0x00011000 },
{ .lengthtype = 0, .inValue = 2.133330, .cssLength = 0x00011110 },
{ .lengthtype = 0, .inValue = 2.142860, .cssLength = 0x00011248 },
{ .lengthtype = 0, .inValue = 2.187500, .cssLength = 0x00011800 },
{ .lengthtype = 0, .inValue = 2.200000, .cssLength = 0x00011998 },
{ .lengthtype = 0, .inValue = 2.250000, .cssLength = 0x00012000 },
{ .lengthtype = 0, .inValue = -2.250000, .cssLength = 0xfffee000 },
{ .lengthtype = 0, .inValue = 2.266670, .cssLength = 0x00012220 },
{ .lengthtype = 0, .inValue = 2.300000, .cssLength = 0x00012660 },
{ .lengthtype = 0, .inValue = 2.307690, .cssLength = 0x00012760 },
{ .lengthtype = 0, .inValue = 2.333330, .cssLength = 0x00012aa8 },
{ .lengthtype = 0, .inValue = 2.352940, .cssLength = 0x00012d28 },
{ .lengthtype = 0, .inValue = 2.375000, .cssLength = 0x00013000 },
{ .lengthtype = 0, .inValue = 2.400000, .cssLength = 0x00013330 },
{ .lengthtype = 0, .inValue = 2.437500, .cssLength = 0x00013800 },
{ .lengthtype = 0, .inValue = 25.000000, .cssLength = 0x000c8000 },
{ .lengthtype = 0, .inValue = 2.500000, .cssLength = 0x00014000 },
{ .lengthtype = 0, .inValue = 2.562500, .cssLength = 0x00014800 },
{ .lengthtype = 0, .inValue = 26.062500, .cssLength = 0x000d0800 },
{ .lengthtype = 0, .inValue = 2.625000, .cssLength = 0x00015000 },
{ .lengthtype = 0, .inValue = 2.666670, .cssLength = 0x00015550 },
{ .lengthtype = 0, .inValue = 27.250000, .cssLength = 0x000da000 },
{ .lengthtype = 0, .inValue = 2.727270, .cssLength = 0x00015d10 },
{ .lengthtype = 0, .inValue = 2.750000, .cssLength = 0x00016000 },
{ .lengthtype = 0, .inValue = 280.000000, .cssLength = 0x008c0000 },
{ .lengthtype = 0, .inValue = 2.800000, .cssLength = 0x00016660 },
{ .lengthtype = 0, .inValue = 2.875000, .cssLength = 0x00017000 },
{ .lengthtype = 0, .inValue = 29.375000, .cssLength = 0x000eb000 },
{ .lengthtype = 0, .inValue = 3.000000, .cssLength = 0x00018000 },
{ .lengthtype = 0, .inValue = 30.062500, .cssLength = 0x000f0800 },
{ .lengthtype = 0, .inValue = 3.066670, .cssLength = 0x00018888 },
{ .lengthtype = 0, .inValue = 31.250000, .cssLength = 0x000fa000 },
{ .lengthtype = 0, .inValue = 3.125000, .cssLength = 0x00019000 },
{ .lengthtype = 0, .inValue = 3.187500, .cssLength = 0x00019800 },
{ .lengthtype = 0, .inValue = 3.200000, .cssLength = 0x00019998 },
{ .lengthtype = 0, .inValue = 3.250000, .cssLength = 0x0001a000 },
{ .lengthtype = 0, .inValue = 3.375000, .cssLength = 0x0001b000 },
{ .lengthtype = 0, .inValue = 3.454550, .cssLength = 0x0001ba28 },
{ .lengthtype = 0, .inValue = 3.466670, .cssLength = 0x0001bbb8 },
{ .lengthtype = 0, .inValue = 3.625000, .cssLength = 0x0001d000 },
{ .lengthtype = 0, .inValue = 3.750000, .cssLength = 0x0001e000 },
{ .lengthtype = 0, .inValue = 3.800000, .cssLength = 0x0001e660 },
{ .lengthtype = 0, .inValue = 3.866670, .cssLength = 0x0001eee8 },
{ .lengthtype = 0, .inValue = 400.000000, .cssLength = 0x00c80000 },
{ .lengthtype = 0, .inValue = 40.000000, .cssLength = 0x00140000 },
{ .lengthtype = 0, .inValue = 4.000000, .cssLength = 0x00020000 },
{ .lengthtype = 0, .inValue = 4.133330, .cssLength = 0x00021110 },
{ .lengthtype = 0, .inValue = 4.375000, .cssLength = 0x00023000 },
{ .lengthtype = 0, .inValue = 4.500000, .cssLength = 0x00024000 },
{ .lengthtype = 0, .inValue = 4.687500, .cssLength = 0x00025800 },
{ .lengthtype = 0, .inValue = 4.800000, .cssLength = 0x00026660 },
{ .lengthtype = 0, .inValue = 5.000000, .cssLength = 0x00028000 },
{ .lengthtype = 0, .inValue = 5.312500, .cssLength = 0x0002a800 },
{ .lengthtype = 0, .inValue = -5.312500, .cssLength = 0xfffd5800 },
{ .lengthtype = 0, .inValue = 5.375000, .cssLength = 0x0002b000 },
{ .lengthtype = 0, .inValue = 56.250000, .cssLength = 0x001c2000 },
{ .lengthtype = 0, .inValue = 5.625000, .cssLength = 0x0002d000 },
{ .lengthtype = 0, .inValue = 56.299999, .cssLength = 0x001c2660 },
{ .lengthtype = 0, .inValue = 5.750000, .cssLength = 0x0002e000 },
{ .lengthtype = 0, .inValue = 58.750000, .cssLength = 0x001d6000 },
{ .lengthtype = 0, .inValue = 5.937500, .cssLength = 0x0002f800 },
{ .lengthtype = 0, .inValue = 60.000000, .cssLength = 0x001e0000 },
{ .lengthtype = 0, .inValue = 6.000000, .cssLength = 0x00030000 },
{ .lengthtype = 0, .inValue = 6.250000, .cssLength = 0x00032000 },
{ .lengthtype = 0, .inValue = 66.000000, .cssLength = 0x00210000 },
{ .lengthtype = 0, .inValue = 69.250000, .cssLength = 0x0022a000 },
{ .lengthtype = 0, .inValue = 7.125000, .cssLength = 0x00039000 },
{ .lengthtype = 0, .inValue = -7.187500, .cssLength = 0xfffc6800 },
{ .lengthtype = 0, .inValue = 7.437500, .cssLength = 0x0003b800 },
{ .lengthtype = 0, .inValue = 75.000000, .cssLength = 0x00258000 },
{ .lengthtype = 0, .inValue = 7.500000, .cssLength = 0x0003c000 },
{ .lengthtype = 0, .inValue = -7.500000, .cssLength = 0xfffc4000 },
{ .lengthtype = 0, .inValue = 7.625000, .cssLength = 0x0003d000 },
{ .lengthtype = 0, .inValue = 8.125000, .cssLength = 0x00041000 },
{ .lengthtype = 0, .inValue = 8.187500, .cssLength = 0x00041800 },
{ .lengthtype = 0, .inValue = 8.250000, .cssLength = 0x00042000 },
{ .lengthtype = 0, .inValue = 8.437500, .cssLength = 0x00043800 },
{ .lengthtype = 0, .inValue = 8.750000, .cssLength = 0x00046000 },
{ .lengthtype = 0, .inValue = 9.625000, .cssLength = 0x0004d000 },
{ .lengthtype = 1, .inValue = 0.000000, .cssLength = 0x00000001 },
//{ .lengthtype = 1, .inValue = -0.340000, .cssLength = 0x00000001 }, // 0.000000
//{ .lengthtype = 1, .inValue = -0.510000, .cssLength = 0xfffffff9 }, // -1.000000
{ .lengthtype = 1, .inValue = 100.000000, .cssLength = 0x00000321 },
{ .lengthtype = 1, .inValue = -100.000000, .cssLength = 0xfffffce1 },
{ .lengthtype = 1, .inValue = 10.000000, .cssLength = 0x00000051 },
{ .lengthtype = 1, .inValue = -10.000000, .cssLength = 0xffffffb1 },
{ .lengthtype = 1, .inValue = 1.000000, .cssLength = 0x00000009 },
{ .lengthtype = 1, .inValue = -1.000000, .cssLength = 0xfffffff9 },
{ .lengthtype = 1, .inValue = 100.285713, .cssLength = 0x00000321 },
{ .lengthtype = 1, .inValue = 100.571426, .cssLength = 0x00000329 },
{ .lengthtype = 1, .inValue = 1010.000000, .cssLength = 0x00001f91 },
{ .lengthtype = 1, .inValue = 102.057137, .cssLength = 0x00000331 },
{ .lengthtype = 1, .inValue = 103.000000, .cssLength = 0x00000339 },
{ .lengthtype = 1, .inValue = 105.750000, .cssLength = 0x00000351 },
{ .lengthtype = 1, .inValue = 106.000000, .cssLength = 0x00000351 },
{ .lengthtype = 1, .inValue = 106.400002, .cssLength = 0x00000351 },
{ .lengthtype = 1, .inValue = 107.428574, .cssLength = 0x00000359 },
{ .lengthtype = 1, .inValue = 108.842857, .cssLength = 0x00000369 },
{ .lengthtype = 1, .inValue = 110.000000, .cssLength = 0x00000371 },
{ .lengthtype = 1, .inValue = 11.000000, .cssLength = 0x00000059 },
{ .lengthtype = 1, .inValue = 112.000000, .cssLength = 0x00000381 },
{ .lengthtype = 1, .inValue = 114.571426, .cssLength = 0x00000399 },
{ .lengthtype = 1, .inValue = 115.628571, .cssLength = 0x000003a1 },
{ .lengthtype = 1, .inValue = 120.000000, .cssLength = 0x000003c1 },
{ .lengthtype = 1, .inValue = 12.000000, .cssLength = 0x00000061 },
{ .lengthtype = 1, .inValue = -12.000000, .cssLength = 0xffffffa1 },
{ .lengthtype = 1, .inValue = 12.006160, .cssLength = 0x00000061 },
{ .lengthtype = 1, .inValue = 121.714287, .cssLength = 0x000003d1 },
{ .lengthtype = 1, .inValue = 122.000000, .cssLength = 0x000003d1 },
{ .lengthtype = 1, .inValue = 122.414291, .cssLength = 0x000003d1 },
{ .lengthtype = 1, .inValue = 123.291428, .cssLength = 0x000003d9 },
// { .lengthtype = 1, .inValue = 12.800000, .cssLength = 0x00000069 }, // 13.000000
{ .lengthtype = 1, .inValue = 128.857147, .cssLength = 0x00000409 },
{ .lengthtype = 1, .inValue = 129.000000, .cssLength = 0x00000409 },
{ .lengthtype = 1, .inValue = 129.199997, .cssLength = 0x00000409 },
{ .lengthtype = 1, .inValue = 129.505707, .cssLength = 0x00000411 },
{ .lengthtype = 1, .inValue = 130.000000, .cssLength = 0x00000411 },
{ .lengthtype = 1, .inValue = 13.000000, .cssLength = 0x00000069 },
{ .lengthtype = 1, .inValue = 135.720001, .cssLength = 0x00000441 },
{ .lengthtype = 1, .inValue = 136.000000, .cssLength = 0x00000441 },
{ .lengthtype = 1, .inValue = 140.000000, .cssLength = 0x00000461 },
{ .lengthtype = 1, .inValue = 14.000000, .cssLength = 0x00000071 },
{ .lengthtype = 1, .inValue = 141.714294, .cssLength = 0x00000471 },
{ .lengthtype = 1, .inValue = 142.500000, .cssLength = 0x00000479 },
//{ .lengthtype = 1, .inValue = 14.400000, .cssLength = 0x00000071 }, // 14.000000
{ .lengthtype = 1, .inValue = 148.857147, .cssLength = 0x000004a9 },
{ .lengthtype = 1, .inValue = 150.000000, .cssLength = 0x000004b1 },
{ .lengthtype = 1, .inValue = 15.000000, .cssLength = 0x00000079 },
{ .lengthtype = 1, .inValue = -15.000000, .cssLength = 0xffffff89 },
// { .lengthtype = 1, .inValue = 1.500000, .cssLength = 0x00000011 }, // 2.000000
//{ .lengthtype = 1, .inValue = -1.500000, .cssLength = 0xfffffff1 }, // -2.000000
{ .lengthtype = 1, .inValue = 152.000000, .cssLength = 0x000004c1 },
{ .lengthtype = 1, .inValue = 156.000000, .cssLength = 0x000004e1 },
{ .lengthtype = 1, .inValue = 160.000000, .cssLength = 0x00000501 },
{ .lengthtype = 1, .inValue = 16.000000, .cssLength = 0x00000081 },
{ .lengthtype = 1, .inValue = -16.000000, .cssLength = 0xffffff81 },
{ .lengthtype = 1, .inValue = 166.000000, .cssLength = 0x00000531 },
{ .lengthtype = 1, .inValue = 168.750000, .cssLength = 0x00000549 },
{ .lengthtype = 1, .inValue = 170.000000, .cssLength = 0x00000551 },
{ .lengthtype = 1, .inValue = 17.000000, .cssLength = 0x00000089 },
{ .lengthtype = 1, .inValue = 172.000000, .cssLength = 0x00000561 },
{ .lengthtype = 1, .inValue = 180.000000, .cssLength = 0x000005a1 },
{ .lengthtype = 1, .inValue = 18.000000, .cssLength = 0x00000091 },
{ .lengthtype = 1, .inValue = 185.000000, .cssLength = 0x000005c9 },
{ .lengthtype = 1, .inValue = 188.000000, .cssLength = 0x000005e1 },
{ .lengthtype = 1, .inValue = 19.000000, .cssLength = 0x00000099 },
{ .lengthtype = 1, .inValue = 195.000000, .cssLength = 0x00000619 },
{ .lengthtype = 1, .inValue = 200.000000, .cssLength = 0x00000641 },
{ .lengthtype = 1, .inValue = -200.000000, .cssLength = 0xfffff9c1 },
{ .lengthtype = 1, .inValue = 20.000000, .cssLength = 0x000000a1 },
{ .lengthtype = 1, .inValue = -20.000000, .cssLength = 0xffffff61 },
{ .lengthtype = 1, .inValue = 2.000000, .cssLength = 0x00000011 },
{ .lengthtype = 1, .inValue = -2.000000, .cssLength = 0xfffffff1 },
{ .lengthtype = 1, .inValue = 20.080000, .cssLength = 0x000000a1 },
{ .lengthtype = 1, .inValue = 204.000000, .cssLength = 0x00000661 },
{ .lengthtype = 1, .inValue = 210.000000, .cssLength = 0x00000691 },
{ .lengthtype = 1, .inValue = 21.000000, .cssLength = 0x000000a9 },
{ .lengthtype = 1, .inValue = 220.000000, .cssLength = 0x000006e1 },
{ .lengthtype = 1, .inValue = 22.000000, .cssLength = 0x000000b1 },
{ .lengthtype = 1, .inValue = 230.000000, .cssLength = 0x00000731 },
{ .lengthtype = 1, .inValue = 23.000000, .cssLength = 0x000000b9 },
{ .lengthtype = 1, .inValue = -23.000000, .cssLength = 0xffffff49 },
{ .lengthtype = 1, .inValue = 240.000000, .cssLength = 0x00000781 },
{ .lengthtype = 1, .inValue = 24.000000, .cssLength = 0x000000c1 },
{ .lengthtype = 1, .inValue = 243.000000, .cssLength = 0x00000799 },
{ .lengthtype = 1, .inValue = 250.000000, .cssLength = 0x000007d1 },
{ .lengthtype = 1, .inValue = 25.000000, .cssLength = 0x000000c9 },
{ .lengthtype = 1, .inValue = -25.000000, .cssLength = 0xffffff39 },
// { .lengthtype = 1, .inValue = 2.500000, .cssLength = 0x00000019 }, // 3.000000
{ .lengthtype = 1, .inValue = 260.000000, .cssLength = 0x00000821 },
{ .lengthtype = 1, .inValue = 26.000000, .cssLength = 0x000000d1 },
{ .lengthtype = 1, .inValue = 27.000000, .cssLength = 0x000000d9 },
{ .lengthtype = 1, .inValue = 272.000000, .cssLength = 0x00000881 },
{ .lengthtype = 1, .inValue = 280.000000, .cssLength = 0x000008c1 },
{ .lengthtype = 1, .inValue = 28.000000, .cssLength = 0x000000e1 },
{ .lengthtype = 1, .inValue = 29.000000, .cssLength = 0x000000e9 },
{ .lengthtype = 1, .inValue = 295.000000, .cssLength = 0x00000939 },
{ .lengthtype = 1, .inValue = 300.000000, .cssLength = 0x00000961 },
{ .lengthtype = 1, .inValue = 30.000000, .cssLength = 0x000000f1 },
{ .lengthtype = 1, .inValue = -30.000000, .cssLength = 0xffffff11 },
{ .lengthtype = 1, .inValue = 3.000000, .cssLength = 0x00000019 },
{ .lengthtype = 1, .inValue = -3.000000, .cssLength = 0xffffffe9 },
//{ .lengthtype = 1, .inValue = 31.320000, .cssLength = 0x000000f9 }, // 31.000000
{ .lengthtype = 1, .inValue = 320.000000, .cssLength = 0x00000a01 },
{ .lengthtype = 1, .inValue = 32.000000, .cssLength = 0x00000101 },
{ .lengthtype = 1, .inValue = -32.000000, .cssLength = 0xffffff01 },
{ .lengthtype = 1, .inValue = 326.000000, .cssLength = 0x00000a31 },
{ .lengthtype = 1, .inValue = 330.000000, .cssLength = 0x00000a51 },
{ .lengthtype = 1, .inValue = 33.000000, .cssLength = 0x00000109 },
{ .lengthtype = 1, .inValue = 334.000000, .cssLength = 0x00000a71 },
{ .lengthtype = 1, .inValue = 34.000000, .cssLength = 0x00000111 },
{ .lengthtype = 1, .inValue = 34.200001, .cssLength = 0x00000111 },
{ .lengthtype = 1, .inValue = 346.000000, .cssLength = 0x00000ad1 },
{ .lengthtype = 1, .inValue = 350.000000, .cssLength = 0x00000af1 },
{ .lengthtype = 1, .inValue = 35.000000, .cssLength = 0x00000119 },
{ .lengthtype = 1, .inValue = 360.000000, .cssLength = 0x00000b41 },
{ .lengthtype = 1, .inValue = 36.000000, .cssLength = 0x00000121 },
{ .lengthtype = 1, .inValue = 37.000000, .cssLength = 0x00000129 },
// { .lengthtype = 1, .inValue = 37.500000, .cssLength = 0x00000131 }, // 38.000000
// { .lengthtype = 1, .inValue = 37.534290, .cssLength = 0x00000131 }, // 38.000000
{ .lengthtype = 1, .inValue = 380.000000, .cssLength = 0x00000be1 },
{ .lengthtype = 1, .inValue = 38.000000, .cssLength = 0x00000131 },
{ .lengthtype = 1, .inValue = 40.000000, .cssLength = 0x00000141 },
{ .lengthtype = 1, .inValue = -40.000000, .cssLength = 0xfffffec1 },
{ .lengthtype = 1, .inValue = 4.000000, .cssLength = 0x00000021 },
{ .lengthtype = 1, .inValue = -4.000000, .cssLength = 0xffffffe1 },
{ .lengthtype = 1, .inValue = 406.000000, .cssLength = 0x00000cb1 },
{ .lengthtype = 1, .inValue = 40.985710, .cssLength = 0x00000149 },
{ .lengthtype = 1, .inValue = 41.000000, .cssLength = 0x00000149 },
{ .lengthtype = 1, .inValue = 42.000000, .cssLength = 0x00000151 },
{ .lengthtype = 1, .inValue = 43.142860, .cssLength = 0x00000159 },
{ .lengthtype = 1, .inValue = 43.748569, .cssLength = 0x00000161 },
{ .lengthtype = 1, .inValue = 43.997139, .cssLength = 0x00000161 },
{ .lengthtype = 1, .inValue = 44.000000, .cssLength = 0x00000161 },
{ .lengthtype = 1, .inValue = 45.000000, .cssLength = 0x00000169 },
{ .lengthtype = 1, .inValue = 458.000000, .cssLength = 0x00000e51 },
{ .lengthtype = 1, .inValue = 46.000000, .cssLength = 0x00000171 },
{ .lengthtype = 1, .inValue = 46.299999, .cssLength = 0x00000171 },
{ .lengthtype = 1, .inValue = 47.771431, .cssLength = 0x00000181 },
{ .lengthtype = 1, .inValue = 48.000000, .cssLength = 0x00000181 },
{ .lengthtype = 1, .inValue = 490.000000, .cssLength = 0x00000f51 },
// { .lengthtype = 1, .inValue = 4.949120, .cssLength = 0x00000029 }, // 5.000000
{ .lengthtype = 1, .inValue = 496.000000, .cssLength = 0x00000f81 },
{ .lengthtype = 1, .inValue = 49.962860, .cssLength = 0x00000191 },
{ .lengthtype = 1, .inValue = 50.000000, .cssLength = 0x00000191 },
{ .lengthtype = 1, .inValue = -50.000000, .cssLength = 0xfffffe71 },
{ .lengthtype = 1, .inValue = 5.000000, .cssLength = 0x00000029 },
{ .lengthtype = 1, .inValue = -5.000000, .cssLength = 0xffffffd9 },
{ .lengthtype = 1, .inValue = 50.211430, .cssLength = 0x00000191 },
{ .lengthtype = 1, .inValue = 50.285709, .cssLength = 0x00000191 },
{ .lengthtype = 1, .inValue = 50.571430, .cssLength = 0x00000199 },
{ .lengthtype = 1, .inValue = 51.000000, .cssLength = 0x00000199 },
{ .lengthtype = 1, .inValue = 52.000000, .cssLength = 0x000001a1 },
{ .lengthtype = 1, .inValue = 540.000000, .cssLength = 0x000010e1 },
{ .lengthtype = 1, .inValue = 54.557140, .cssLength = 0x000001b9 },
{ .lengthtype = 1, .inValue = 55.000000, .cssLength = 0x000001b9 },
{ .lengthtype = 1, .inValue = 56.000000, .cssLength = 0x000001c1 },
{ .lengthtype = 1, .inValue = 56.425709, .cssLength = 0x000001c1 },
{ .lengthtype = 1, .inValue = 57.428570, .cssLength = 0x000001c9 },
{ .lengthtype = 1, .inValue = 57.714291, .cssLength = 0x000001d1 },
{ .lengthtype = 1, .inValue = 578.000000, .cssLength = 0x00001211 },
{ .lengthtype = 1, .inValue = 58.000000, .cssLength = 0x000001d1 },
{ .lengthtype = 1, .inValue = 590.000000, .cssLength = 0x00001271 },
{ .lengthtype = 1, .inValue = 600.000000, .cssLength = 0x000012c1 },
{ .lengthtype = 1, .inValue = 60.000000, .cssLength = 0x000001e1 },
{ .lengthtype = 1, .inValue = -60.000000, .cssLength = 0xfffffe21 },
{ .lengthtype = 1, .inValue = 6.000000, .cssLength = 0x00000031 },
{ .lengthtype = 1, .inValue = -6.000000, .cssLength = 0xffffffd1 },
{ .lengthtype = 1, .inValue = 61.342861, .cssLength = 0x000001e9 },
{ .lengthtype = 1, .inValue = 62.000000, .cssLength = 0x000001f1 },
{ .lengthtype = 1, .inValue = 62.639999, .cssLength = 0x000001f9 },
{ .lengthtype = 1, .inValue = 630.000000, .cssLength = 0x000013b1 },
{ .lengthtype = 1, .inValue = 64.000000, .cssLength = 0x00000201 },
{ .lengthtype = 1, .inValue = 64.571426, .cssLength = 0x00000209 },
{ .lengthtype = 1, .inValue = 64.857140, .cssLength = 0x00000209 },
{ .lengthtype = 1, .inValue = 65.000000, .cssLength = 0x00000209 },
{ .lengthtype = 1, .inValue = 65.599998, .cssLength = 0x00000211 },
{ .lengthtype = 1, .inValue = 68.000000, .cssLength = 0x00000221 },
{ .lengthtype = 1, .inValue = 68.128571, .cssLength = 0x00000221 },
{ .lengthtype = 1, .inValue = 68.854286, .cssLength = 0x00000229 },
{ .lengthtype = 1, .inValue = 70.000000, .cssLength = 0x00000231 },
{ .lengthtype = 1, .inValue = -70.000000, .cssLength = 0xfffffdd1 },
{ .lengthtype = 1, .inValue = 7.000000, .cssLength = 0x00000039 },
{ .lengthtype = 1, .inValue = -7.000000, .cssLength = 0xffffffc9 },
{ .lengthtype = 1, .inValue = 71.714287, .cssLength = 0x00000241 },
{ .lengthtype = 1, .inValue = 72.000000, .cssLength = 0x00000241 },
{ .lengthtype = 1, .inValue = 74.000000, .cssLength = 0x00000251 },
{ .lengthtype = 1, .inValue = 74.914291, .cssLength = 0x00000259 },
{ .lengthtype = 1, .inValue = 75.000000, .cssLength = 0x00000259 },
// { .lengthtype = 1, .inValue = -7.500000, .cssLength = 0xffffffc1 }, // -8.000000
{ .lengthtype = 1, .inValue = 75.068573, .cssLength = 0x00000259 },
// { .lengthtype = 1, .inValue = 7.521700, .cssLength = 0x00000041 }, // 8.000000
{ .lengthtype = 1, .inValue = 78.000000, .cssLength = 0x00000271 },
{ .lengthtype = 1, .inValue = 78.857140, .cssLength = 0x00000279 },
{ .lengthtype = 1, .inValue = 79.000000, .cssLength = 0x00000279 },
{ .lengthtype = 1, .inValue = 79.142860, .cssLength = 0x00000279 },
{ .lengthtype = 1, .inValue = 800.000000, .cssLength = 0x00001901 },
{ .lengthtype = 1, .inValue = 80.000000, .cssLength = 0x00000281 },
{ .lengthtype = 1, .inValue = 8.000000, .cssLength = 0x00000041 },
{ .lengthtype = 1, .inValue = -8.000000, .cssLength = 0xffffffc1 },
{ .lengthtype = 1, .inValue = 81.282860, .cssLength = 0x00000289 },
{ .lengthtype = 1, .inValue = 81.699997, .cssLength = 0x00000291 },
{ .lengthtype = 1, .inValue = 85.000000, .cssLength = 0x000002a9 },
{ .lengthtype = 1, .inValue = 86.000000, .cssLength = 0x000002b1 },
{ .lengthtype = 1, .inValue = 86.285713, .cssLength = 0x000002b1 },
{ .lengthtype = 1, .inValue = 87.497139, .cssLength = 0x000002b9 },
{ .lengthtype = 1, .inValue = 88.485710, .cssLength = 0x000002c1 },
{ .lengthtype = 1, .inValue = 90.000000, .cssLength = 0x000002d1 },
{ .lengthtype = 1, .inValue = -90.000000, .cssLength = 0xfffffd31 },
{ .lengthtype = 1, .inValue = 9.000000, .cssLength = 0x00000049 },
{ .lengthtype = 1, .inValue = 92.000000, .cssLength = 0x000002e1 },
{ .lengthtype = 1, .inValue = 93.142860, .cssLength = 0x000002e9 },
{ .lengthtype = 1, .inValue = 93.428574, .cssLength = 0x000002e9 },
{ .lengthtype = 1, .inValue = 940.000000, .cssLength = 0x00001d61 },
{ .lengthtype = 1, .inValue = 95.271431, .cssLength = 0x000002f9 },
{ .lengthtype = 1, .inValue = 960.000000, .cssLength = 0x00001e01 },
{ .lengthtype = 1, .inValue = 964.000000, .cssLength = 0x00001e21 },
{ .lengthtype = 1, .inValue = 970.000000, .cssLength = 0x00001e51 },
{ .lengthtype = 1, .inValue = 97.000000, .cssLength = 0x00000309 },
{ .lengthtype = 1, .inValue = 972.000000, .cssLength = 0x00001e61 },
{ .lengthtype = 2, .inValue = 0.000000, .cssLength = 0x00000002 },
{ .lengthtype = 2, .inValue = 10.000000, .cssLength = 0x00050002 },
{ .lengthtype = 2, .inValue = 180.000000, .cssLength = 0x005a0002 },
{ .lengthtype = 2, .inValue = 2.822222, .cssLength = 0x0001693a },
{ .lengthtype = 2, .inValue = 3.527778, .cssLength = 0x0001c38a },
{ .lengthtype = 2, .inValue = 5.000000, .cssLength = 0x00028002 },
// { .lengthtype = 3, .inValue = 0.010000, .cssLength = 0x00000143 }, // 0.009766
{ .lengthtype = 3, .inValue = 0.100000, .cssLength = 0x00000ccb },
{ .lengthtype = 3, .inValue = -0.100000, .cssLength = 0xfffff333 },
{ .lengthtype = 3, .inValue = 0.200000, .cssLength = 0x0000199b },
{ .lengthtype = 3, .inValue = -0.200000, .cssLength = 0xffffe663 },
{ .lengthtype = 3, .inValue = 0.250000, .cssLength = 0x00002003 },
{ .lengthtype = 3, .inValue = 0.300000, .cssLength = 0x00002663 },
{ .lengthtype = 3, .inValue = -0.300000, .cssLength = 0xffffd99b },
{ .lengthtype = 3, .inValue = 0.400000, .cssLength = 0x00003333 },
{ .lengthtype = 3, .inValue = 0.500000, .cssLength = 0x00004003 },
{ .lengthtype = 3, .inValue = -0.500000, .cssLength = 0xffffc003 },
{ .lengthtype = 3, .inValue = 0.750000, .cssLength = 0x00006003 },
{ .lengthtype = 3, .inValue = 0.830000, .cssLength = 0x00006a3b },
{ .lengthtype = 3, .inValue = 10.000000, .cssLength = 0x00050003 },
{ .lengthtype = 3, .inValue = 1.000000, .cssLength = 0x00008003 },
{ .lengthtype = 3, .inValue = 10.500000, .cssLength = 0x00054003 },
{ .lengthtype = 3, .inValue = 11.000000, .cssLength = 0x00058003 },
{ .lengthtype = 3, .inValue = 1.100000, .cssLength = 0x00008ccb },
{ .lengthtype = 3, .inValue = 1.120000, .cssLength = 0x00008f5b },
{ .lengthtype = 3, .inValue = 1.142857, .cssLength = 0x0000924b },
{ .lengthtype = 3, .inValue = 1.143000, .cssLength = 0x0000924b },
{ .lengthtype = 3, .inValue = -1.150000, .cssLength = 0xffff6ccb },
{ .lengthtype = 3, .inValue = 1.170000, .cssLength = 0x000095c3 },
{ .lengthtype = 3, .inValue = 1.200000, .cssLength = 0x0000999b },
{ .lengthtype = 3, .inValue = 1.285714, .cssLength = 0x0000a493 },
{ .lengthtype = 3, .inValue = 13.000000, .cssLength = 0x00068003 },
{ .lengthtype = 3, .inValue = 1.300000, .cssLength = 0x0000a663 },
{ .lengthtype = 3, .inValue = 1.330000, .cssLength = 0x0000aa3b },
{ .lengthtype = 3, .inValue = 1.350000, .cssLength = 0x0000accb },
{ .lengthtype = 3, .inValue = -1.350000, .cssLength = 0xffff5333 },
{ .lengthtype = 3, .inValue = 15.000000, .cssLength = 0x00078003 },
{ .lengthtype = 3, .inValue = 1.500000, .cssLength = 0x0000c003 },
{ .lengthtype = 3, .inValue = 1.670000, .cssLength = 0x0000d5c3 },
{ .lengthtype = 3, .inValue = 1.700000, .cssLength = 0x0000d99b },
{ .lengthtype = 3, .inValue = 1.800000, .cssLength = 0x0000e663 },
{ .lengthtype = 3, .inValue = 2.000000, .cssLength = 0x00010003 },
{ .lengthtype = 3, .inValue = 2.200000, .cssLength = 0x0001199b },
{ .lengthtype = 3, .inValue = 2.250000, .cssLength = 0x00012003 },
{ .lengthtype = 3, .inValue = 2.330000, .cssLength = 0x00012a3b },
{ .lengthtype = 3, .inValue = 2.800000, .cssLength = 0x00016663 },
{ .lengthtype = 3, .inValue = 3.000000, .cssLength = 0x00018003 },
{ .lengthtype = 3, .inValue = 5.000000, .cssLength = 0x00028003 },
{ .lengthtype = 3, .inValue = 6.000000, .cssLength = 0x00030003 },
{ .lengthtype = 5, .inValue = 0.000000, .cssLength = 0x00000005 },
{ .lengthtype = 5, .inValue = 0.041667, .cssLength = 0x00000555 },
{ .lengthtype = 5, .inValue = 0.060000, .cssLength = 0x000007ad },
{ .lengthtype = 5, .inValue = 0.066667, .cssLength = 0x0000088d },
{ .lengthtype = 5, .inValue = 0.076923, .cssLength = 0x000009dd },
{ .lengthtype = 5, .inValue = 0.083333, .cssLength = 0x00000aad },
{ .lengthtype = 5, .inValue = 0.120000, .cssLength = 0x00000f5d },
{ .lengthtype = 5, .inValue = 0.125000, .cssLength = 0x00001005 },
{ .lengthtype = 5, .inValue = 0.130000, .cssLength = 0x000010a5 },
{ .lengthtype = 5, .inValue = 0.160000, .cssLength = 0x0000147d },
{ .lengthtype = 5, .inValue = 0.166667, .cssLength = 0x00001555 },
{ .lengthtype = 5, .inValue = 0.170000, .cssLength = 0x000015c5 },
{ .lengthtype = 5, .inValue = 0.200000, .cssLength = 0x0000199d },
{ .lengthtype = 5, .inValue = 0.208333, .cssLength = 0x00001aad },
{ .lengthtype = 5, .inValue = 0.245000, .cssLength = 0x00001f5d },
//{ .lengthtype = 5, .inValue = 0.250000, .cssLength = 0x00001ffd }, // 0x00002005
{ .lengthtype = 5, .inValue = 0.250000, .cssLength = 0x00002005 },
{ .lengthtype = 5, .inValue = 0.291667, .cssLength = 0x00002555 },
{ .lengthtype = 5, .inValue = 0.300000, .cssLength = 0x00002665 },
{ .lengthtype = 5, .inValue = 0.330000, .cssLength = 0x00002a3d },
{ .lengthtype = 5, .inValue = 0.333300, .cssLength = 0x00002aad },
{ .lengthtype = 5, .inValue = 0.333330, .cssLength = 0x00002aad },
{ .lengthtype = 5, .inValue = 0.333333, .cssLength = 0x00002aad },
{ .lengthtype = 5, .inValue = 0.350000, .cssLength = 0x00002ccd },
{ .lengthtype = 5, .inValue = 0.375000, .cssLength = 0x00003005 },
{ .lengthtype = 5, .inValue = 0.400000, .cssLength = 0x00003335 },
{ .lengthtype = 5, .inValue = 0.416667, .cssLength = 0x00003555 },
{ .lengthtype = 5, .inValue = 0.450000, .cssLength = 0x0000399d },
{ .lengthtype = 5, .inValue = 0.458333, .cssLength = 0x00003aad },
{ .lengthtype = 5, .inValue = 0.490000, .cssLength = 0x00003ebd },
// { .lengthtype = 5, .inValue = 0.500000, .cssLength = 0x00003ffd }, // 0x00004005
{ .lengthtype = 5, .inValue = 0.500000, .cssLength = 0x00004005 },
{ .lengthtype = 5, .inValue = 0.540000, .cssLength = 0x0000451d },
{ .lengthtype = 5, .inValue = 0.541667, .cssLength = 0x00004555 },
{ .lengthtype = 5, .inValue = 0.583333, .cssLength = 0x00004aad },
{ .lengthtype = 5, .inValue = 0.590000, .cssLength = 0x00004b85 },
{ .lengthtype = 5, .inValue = 0.600000, .cssLength = 0x00004ccd },
{ .lengthtype = 5, .inValue = 0.625000, .cssLength = 0x00005005 },
{ .lengthtype = 5, .inValue = 0.660000, .cssLength = 0x0000547d },
{ .lengthtype = 5, .inValue = 0.666667, .cssLength = 0x00005555 },
{ .lengthtype = 5, .inValue = 0.700000, .cssLength = 0x0000599d },
{ .lengthtype = 5, .inValue = 0.708333, .cssLength = 0x00005aad },
//{ .lengthtype = 5, .inValue = 0.750000, .cssLength = 0x00005ffd }, // 0x00006005
{ .lengthtype = 5, .inValue = 0.750000, .cssLength = 0x00006005 },
{ .lengthtype = 5, .inValue = 0.791667, .cssLength = 0x00006555 },
{ .lengthtype = 5, .inValue = 0.800000, .cssLength = 0x00006665 },
{ .lengthtype = 5, .inValue = 0.820000, .cssLength = 0x000068f5 },
{ .lengthtype = 5, .inValue = 0.830000, .cssLength = 0x00006a3d },
{ .lengthtype = 5, .inValue = 0.833333, .cssLength = 0x00006aad },
{ .lengthtype = 5, .inValue = 0.850000, .cssLength = 0x00006ccd },
{ .lengthtype = 5, .inValue = 0.875000, .cssLength = 0x00007005 },
{ .lengthtype = 5, .inValue = 0.880000, .cssLength = 0x000070a5 },
{ .lengthtype = 5, .inValue = 0.900000, .cssLength = 0x00007335 },
{ .lengthtype = 5, .inValue = 0.916667, .cssLength = 0x00007555 },
{ .lengthtype = 5, .inValue = 0.930000, .cssLength = 0x0000770d },
{ .lengthtype = 5, .inValue = 0.950000, .cssLength = 0x0000799d },
{ .lengthtype = 5, .inValue = 0.958333, .cssLength = 0x00007aad },
{ .lengthtype = 5, .inValue = 0.975000, .cssLength = 0x00007ccd },
{ .lengthtype = 5, .inValue = 0.980000, .cssLength = 0x00007d75 },
//{ .lengthtype = 5, .inValue = 1.000000, .cssLength = 0x00007ffd }, // 0x00008005
{ .lengthtype = 5, .inValue = 1.000000, .cssLength = 0x00008005 },
{ .lengthtype = 5, .inValue = 1.050000, .cssLength = 0x00008665 },
{ .lengthtype = 5, .inValue = 1.100000, .cssLength = 0x00008ccd },
{ .lengthtype = 5, .inValue = 1.150000, .cssLength = 0x00009335 },
{ .lengthtype = 5, .inValue = 1.200000, .cssLength = 0x0000999d },
{ .lengthtype = 5, .inValue = 1.350000, .cssLength = 0x0000accd },
{ .lengthtype = 5, .inValue = 2.000000, .cssLength = 0x00010005 },
{ .lengthtype = 7, .inValue = 0.000000, .cssLength = 0x00000007 },
{ .lengthtype = 99, .inValue = 0,       .cssLength = 0x00 }
};



void css_length_test(void)
{
   int z = 0;
   while (css_length_test_data[z].lengthtype != 99) {

	   CssLength lenVal = cpp_cssCreateLength(css_length_test_data[z].inValue, (CssLengthType) css_length_test_data[z].lengthtype);

      const CssLengthType t = cpp_cssLengthType(lenVal);
      if (t != (CssLengthType) css_length_test_data[z].lengthtype) {
         fprintf(stderr, "CSS_LENGTH_TYPE: Failure in test %d, expected result = %d, but got %d\n", z, css_length_test_data[z].lengthtype, t);
         exit(-1);
      }

      {
         const float f = cpp_cssLengthValue(lenVal);
         const float epsilon = 0.01 * css_length_test_data[z].inValue;
         if (fabs(f - css_length_test_data[z].inValue) > fabs(epsilon)) {
            fprintf(stderr, "CSS_LENGTH_VALUE (1): Failure in test %d, type %d, expected result = %f, but got %f (len = %d / 0x%08x)\n",
                    z, t, css_length_test_data[z].inValue, f, (int) lenVal.length_bits, lenVal.length_bits);
            exit(-1);
         }
      }

      {
         CssLength cssLength;
         cssLength.length_bits = css_length_test_data[z].cssLength;
         const float f = cpp_cssLengthValue(cssLength);
         const float epsilon = 0.01 * css_length_test_data[z].inValue;
         if (fabs(f - css_length_test_data[z].inValue) > fabs(epsilon)) {
            fprintf(stderr, "CSS_LENGTH_VALUE (2): Failure in test %d, type %d, expected result = %f, but got %f (len = %d / 0x%08x)\n",
                    z, t, css_length_test_data[z].inValue, f, (int) css_length_test_data[z].cssLength, css_length_test_data[z].cssLength);
            exit(-1);
         }
      }

      z++;
   }

}
