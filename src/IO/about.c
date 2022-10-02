/*
 * File: about.c
 *
 * Copyright (C) 1999-2007 Jorge Arellano Cid <jcid@dillo.org>
 * Copyright (C) 2021-2022 Kamil Ignacak acerion@wp.pl
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 */

#include <config.h>

/*
 * HTML text for startup screen
 */
const char *const AboutSplash=
"<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>\n"
"<html>\n"
"    <head>\n"
"        <title>Splash screen for hello-" VERSION "</title>\n"
"    </head>\n"
"    <body bgcolor='#778899' text='#000000' link='#000000' vlink='#000000'>\n"
"\n"
"\n"
"\n"
"\n"
"        <!--   the head of the page   -->\n"
"\n"
"        <table width='100%' border='1' cellspacing='0' cellpadding='0' bgcolor='#CCCCCC'>\n"
"            <tr>\n"
"                <td valign='top' align='left'>\n"
"                <h1>&nbsp;Welcome to Hello " VERSION "&nbsp;</h1>\n"
"        </table>\n"
"\n"
"        <br>\n"
"\n"
"        <!-- the main layout table with a single row -->\n"
"        <table width='100%' border='0' cellspacing='0' cellpadding='0'>\n"
"            <tr>\n"
"                <td valign='top' width='150' align='center'>\n"
"\n"
"                    <!--   The left-hand side bar   -->\n"
"\n"
"                    <table border='0' cellspacing='1' cellpadding='3' width='140' bgcolor='#000000'>\n"
"                        <tr>\n"
"                            <td colspan='1' bgcolor='#CCCCCC'>Hello\n"
"                        <tr>\n"
"                            <td bgcolor='#FFFFFF'>\n"
"                            <p>&nbsp;&nbsp;<a href='https://github.com/acerion/hello'>Home</a></p>\n"
"                    </table>\n"
"                    <br>\n"
"\n"
"\n"
"                    <table border='0' cellspacing='1' cellpadding='3' width='140' bgcolor='#000000'>\n"
"                        <tr>\n"
"                            <td colspan='1' bgcolor='#CCCCCC'>Test links</td>\n"
"                        </tr>\n"
"                        <tr>\n"
"                            <td bgcolor='#FFFFFF'>\n"
"                                <p>&nbsp;&nbsp;<a href='http://lwn.net/'>LWN</a></p>\n"
"                                <p>&nbsp;&nbsp;<a href='https://soylentnews.org/'>SoylentNews</a></p>\n"
"                           </td>\n"
"                        </tr>\n"
"                    </table>\n"
"                </td>\n"
"\n"
"\n"
"                <!-- Small vertical spacer between side bar and main area -->\n"
"                <td width='20'></td>\n"
"\n"
"\n"
"                <!-- The big, main right-hand side area of layout table -->\n"
"                <td valign='top'>\n"
"\n"
"                    <table border='0' cellpadding='5' cellspacing='1' align='center' bgcolor='#000000' width='100%'>\n"
"                        <tr>\n"
"                            <td bgcolor='#CCCCCC'>\n"
"                                <h4>Free Software</h4>\n"
"                        <tr>\n"
"                            <td bgcolor='#FFFFFF'>\n"
"                                <p>\n"
"                                    The Hello web browser is Free Software under the terms of version 3 of\n"
"                                    the <a href='http://www.gnu.org/licenses/gpl.html'>GPL</a>.\n"
"                                    This means you have four basic freedoms:\n"
"                                    <ul>\n"
"                                        <li>Freedom to use the program any way you see fit.\n"
"                                        <li>Freedom to study and modify the source code.\n"
"                                        <li>Freedom to make backup copies.\n"
"                                        <li>Freedom to redistribute it.\n"
"                                    </ul>\n"
"                                    <br/>\n"
"                                    The GPL is the legal mechanism that gives you these freedoms.\n"
"                                    It also protects you from having them taken away: any derivative work\n"
"                                    based on the program must be under GPLv3 as well.\n"
"                            </td>\n"
"                        </tr>\n"
"                    </table>\n"
"\n"
"                    <br>\n"
"\n"
"                    <table border='0' cellpadding='5' cellspacing='1' align='center' bgcolor='#000000' width='100%'>\n"
"                        <tr>\n"
"                            <td bgcolor='#CCCCCC'>\n"
"                                <h4>Notes</h4>\n"
"                        <tr>\n"
"                            <td bgcolor='#FFFFFF'>\n"
"                                <ul>\n"
"                                    <li>Hello is based on <a href='http://www.dillo.org/'>dillo</a> web browser v3.0.5\n</li>"
"                                    <li>Hello is in early stages of development. Expect a lot of functionality to be missing\n</li>"
"                                    <li>Hello's web page is <a href='https://github.com/acerion/hello'>https://github.com/acerion/hello</a>\n</li>"
"                                </ul>\n"
"                    </table>\n"
"                </td>\n"
"            </tr>\n"
"        <!-- end of the main layout table -->\n"
"        </table>\n"
"\n"
"    <!--   footnotes   -->\n"
"\n"
"    </body>\n"
"</html>\n";

