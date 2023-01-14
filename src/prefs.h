/*
 * Preferences
 *
 * Copyright (C) 2006-2009 Jorge Arellano Cid <jcid@dillo.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 */

#ifndef __PREFS_H__
#define __PREFS_H__

#include "url.h"
#include "Hello/hello.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define PREFS_GEOMETRY_DEFAULT_WIDTH   780
#define PREFS_GEOMETRY_DEFAULT_HEIGHT  580
#define PREFS_GEOMETRY_DEFAULT_XPOS  -9999
#define PREFS_GEOMETRY_DEFAULT_YPOS  -9999

/* FLTK has free color indices from 16 to 31 */
#define PREFS_UI_BUTTON_HIGHLIGHT_COLOR 16
#define PREFS_UI_TAB_ACTIVE_BG_COLOR 17
#define PREFS_UI_TAB_ACTIVE_FG_COLOR 18
#define PREFS_UI_TAB_BG_COLOR 19
#define PREFS_UI_TAB_FG_COLOR 20

/* Panel sizes */
enum { P_tiny = 0, P_small, P_medium };

typedef struct {
   int width;
   int height;
   int xpos;
   int ypos;
   char *http_language;
   int32_t http_max_conns;
   DilloUrl *http_proxy;
   char *http_proxyuser;
   char *http_referer;
   char *http_user_agent;
   char *no_proxy;
   DilloUrl *start_page;
   DilloUrl *home;
   bool allow_white_bg;
   int32_t white_bg_replacement;
   int32_t bg_color;
   int32_t ui_button_highlight_color;
   int32_t ui_fg_color;
   int32_t ui_main_bg_color;
   int32_t ui_selection_color;
   int32_t ui_tab_active_bg_color;
   int32_t ui_tab_active_fg_color;
   int32_t ui_tab_bg_color;
   int32_t ui_tab_fg_color;
   int32_t ui_text_bg_color;
   bool contrast_visited_color;
   bool show_tooltip;
   bool show_ui_tooltip;
   char *theme;
   int panel_size;
   bool small_icons;
   bool limit_text_width;
   bool w3c_plus_heuristics;
   bool focus_new_tab;

   bool show_back;
   bool show_forw;
   bool show_home;
   bool show_reload;
   bool show_save;
   bool show_stop;
   bool show_bookmarks;
   bool show_tools;
   bool show_filemenu;
   bool show_clear_url;
   bool show_url;
   bool show_search;
   bool show_help;
   bool show_progress_box;
   bool show_quit_dialog;
   bool fullwindow_start;
   bool load_images;
   bool load_background_images;
   bool load_stylesheets;
   bool parse_embedded_css;
   int32_t buffered_drawing;

   c_prefs_t preferences;

   bool enterpress_forces_submit;
   bool middle_click_opens_new_tab;
   bool right_click_closes_tab;
   bool search_url_idx;
   Dlist *search_urls;
   char *save_dir;
   bool show_msg;
   bool show_extra_warnings;
   bool middle_click_drags_page;
   int penalty_hyphen, penalty_hyphen_2;
   int penalty_em_dash_left, penalty_em_dash_right, penalty_em_dash_right_2;
   int stretchability_factor;
} DilloPrefs;

/* Global Data */
extern DilloPrefs prefs;

void a_Prefs_init(void);
void a_Prefs_freeall(void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __PREFS_H__ */
