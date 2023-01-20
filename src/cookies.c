/*
 * File: cookies.c
 *
 * Copyright 2001 Lars Clausen   <lrclause@cs.uiuc.edu>
 *                Jörgen Viksell <jorgen.viksell@telia.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 */

/* Handling of cookies takes place here. */

#include "msg.h"
#include "Hello/hello.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#include "IO/Url.h"
#include "list.h"
#include "cookies.h"
#include "capi.h"
#include "../dpip/dpip.h"

typedef enum {
   COOKIE_ACCEPT = 0,
   COOKIE_ACCEPT_SESSION = 1,
   COOKIE_DENY = 2,
} CookieControlAction;

static CookieControlAction Cookies_control_check(const DilloUrl *url);

/*
 * Initialize the cookies module
 * (The 'disabled' variable is writable only within a_Cookies_init)
 */
void a_Cookies_init(void)
{
}

/*
 * Flush cookies to disk and free all the memory allocated.
 */
void a_Cookies_freeall()
{
}

/*
 * Set the value corresponding to the cookie string
 */
void a_Cookies_set(Dlist *cookie_strings, const DilloUrl *set_url,
                   const char *date)
{
   CookieControlAction action;
   char *cmd, *cookie_string, *dpip_tag;
   const char *path;
   int i;

   /* TODO: make Haskell cookies function that returns 'is disabled' flag. We
      can use already existing CookiesConfig::cookiesEnabled flag for
      that. */
#if 0
   if (g_disabled)
      return;
#endif

   action = Cookies_control_check(set_url);
   if (action == COOKIE_DENY) {
      MSG("hello: cookies: denied SET for %s\n", URL_HOST_(set_url));
      return;
   }

   for (i = 0; (cookie_string = dList_nth_data(cookie_strings, i)); ++i) {
      path = URL_PATH_(set_url);
      if (date)
         cmd = a_Dpip_build_cmd("cmd=%s cookie=%s host=%s path=%s date=%s",
                                "set_cookie", cookie_string,
                                URL_HOST_(set_url), path ? path : "/", date);
      else
         cmd = a_Dpip_build_cmd("cmd=%s cookie=%s host=%s path=%s",
                                "set_cookie", cookie_string,
                                URL_HOST_(set_url), path ? path : "/");

      MSG("Cookies.c: a_Cookies_set \n\t \"%s\" \n",cmd );
      /* This call is commented because it doesn't guarantee the order
       * in which cookies are set and got. (It may deadlock too) */
      //a_Capi_dpi_send_cmd(NULL, NULL, cmd, "cookies", 1);

      dpip_tag = a_Dpi_send_blocking_cmd("cookies", cmd);
      MSG("a_Cookies_set: dpip_tag = {%s}\n", dpip_tag);
      dFree(dpip_tag);
      dFree(cmd);
   }
}

/*
 * Return a string containing cookie data for an HTTP query.
 */
char *a_Cookies_get_query(const DilloUrl *query_url, const DilloUrl *requester)
{
   char *cmd, *dpip_tag, *query;
   const char *path;
   CookieControlAction action;

   /* TODO: make Haskell cookies function that returns 'is disabled' flag. We
      can use already existing CookiesConfig::cookiesEnabled flag for
      that. */
#if 0
   if (g_disabled)
      return dStrdup("");
#endif

   action = Cookies_control_check(query_url);
   if (action == COOKIE_DENY) {
      _MSG("hello: cookies: denied GET for '%s'\n", URL_HOST_(query_url));
      return dStrdup("");
   }

   if (requester == NULL) {
      /* request made by user */
   } else if (!a_Url_same_organization(query_url, requester)) {
      MSG("Cookies: not sent for request by '%s' for '%s'\n",
          URL_HOST(requester), URL_HOST(query_url));
      return dStrdup("");
   }

   path = URL_PATH_(query_url);

   cmd = a_Dpip_build_cmd("cmd=%s scheme=%s host=%s path=%s",
                          "get_cookie", URL_SCHEME(query_url),
                         URL_HOST(query_url), path ? path : "/");

   /* Get the answer from cookies.dpi */
   _MSG("cookies.c: a_Dpi_send_blocking_cmd cmd = {%s}\n", cmd);
   dpip_tag = a_Dpi_send_blocking_cmd("cookies", cmd);
   _MSG("cookies.c: after a_Dpi_send_blocking_cmd resp={%s}\n", dpip_tag);
   dFree(cmd);

   if (dpip_tag != NULL) {
      query = a_Dpip_get_attr(dpip_tag, "cookie");
      dFree(dpip_tag);
   } else {
      query = dStrdup("");
   }
   return query;
}

/* -------------------------------------------------------------
 *                    Access control routines
 * ------------------------------------------------------------- */


/*
 * Same as the above except it takes an URL
 */
static CookieControlAction Cookies_control_check(const DilloUrl *url)
{
   return (CookieControlAction) ffiLookupActionForDomain(URL_HOST(url));
}

