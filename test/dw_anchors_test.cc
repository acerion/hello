/*
 * Dillo Widget
 *
 * Copyright 2005-2007 Sebastian Geerken <sgeerken@dillo.org>
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



#include <ctype.h>
#include <FL/Fl_Window.H>
#include <FL/Fl.H>

#include "../dw/core.hh"
#include "../dw/fltkcore.hh"
#include "../dw/fltkviewport.hh"
#include "../dw/textblock.hh"

using namespace lout::container::typed;
using namespace dw;
using namespace dw::core;
using namespace dw::core::style;
using namespace dw::fltk;

static FltkPlatform *platform;
static Layout *layout;
static Fl_Window *window;
static FltkViewport *viewport;
static Style *topWidgetStyle, *widgetStyle, *wordStyle, *headingStyle;
static Textblock *topTextblock = NULL;
static int textblockNo = 0;

static const char *numbers[10] = {
   "one", "two", "three", "four", "five",
   "six", "seven", "eight", "nine", "ten"
};

static void anchorCallback (Fl_Widget *widget, void *data)
{
   layout->setAnchor (numbers[(long)data]);
}

static void textTimeout (void *data)
{
   Textblock *oldTop = topTextblock;
   topTextblock = new Textblock (false);

   if (oldTop) {
      oldTop->addLinebreak (wordStyle);
      oldTop->addWidget (topTextblock, widgetStyle);
   } else {
      topTextblock->setStyle (topWidgetStyle);
      layout->setWidget (topTextblock);
   }

   topTextblock->addAnchor (numbers[textblockNo], headingStyle);

   char buf[16];
   strcpy (buf, numbers[textblockNo]);
   buf[0] = lout::misc::AsciiToupper (buf[0]);
   topTextblock->addText (buf, headingStyle);
   topTextblock->addParbreak (5, headingStyle);

   for (int i = 0; i < 30; i++) {
      strcpy (buf, numbers[textblockNo]);
      if (i == 0)
         buf[0] = lout::misc::AsciiToupper (buf[0]);
      strcat (buf, i == 29 ? "." : ",");

      topTextblock->addText (buf, wordStyle);
      topTextblock->addSpace (wordStyle);
   }

   topTextblock->flush ();

   textblockNo++;
   if (textblockNo < 10)
      Fl::repeat_timeout (1, textTimeout, NULL);

}

int main(int argc, char **argv)
{
   char *buttonLabel[10];

   platform = new FltkPlatform ();
   layout = new Layout (platform);

   window = new Fl_Window(250, 200, "Dw Anchors Test");
   window->box(FL_NO_BOX);
   window->begin();

   viewport = new FltkViewport (50, 0, 200, 200);
   viewport->end();
   layout->attachView (viewport);

   for (int i = 0; i < 10; i++) {
      char buf[16];
      strcpy (buf, numbers[i]);
      buf[0] = lout::misc::AsciiToupper (buf[0]);
      buttonLabel[i] = strdup(buf);
      Fl_Button *button = new Fl_Button(0, 20 * i, 50, 20, buttonLabel[i]);
      button->callback (anchorCallback, (void*)(long)i);
      button->when (FL_WHEN_RELEASE);
   }

   FontAttrs fontAttrs;
   fontAttrs.font_attrs.name = "Bitstream Charter";
   fontAttrs.font_attrs.size = 14;
   fontAttrs.font_attrs.weight = 400;
   fontAttrs.font_attrs.style = FONT_STYLE_NORMAL;
   fontAttrs.font_attrs.letterSpacing = 0;
   fontAttrs.font_attrs.fontVariant = FONT_VARIANT_NORMAL;

   StyleAttrs styleAttrs;
   styleAttrs.initValues ();
   styleAttrs.font = dw::core::style::Font::create (layout, &fontAttrs);
   ffiStyleAttrsSetMargin(styleAttrs.c_attrs.c_style_attrs_ref, 5);
   styleAttrs.color = Color::create (layout, 0x000000);
   styleAttrs.backgroundColor = Color::create (layout, 0xffffff);
   topWidgetStyle = Style::create (&styleAttrs);

   c_style_margin_t margin = { .right = 0, .left = 20 };
   ffiStyleAttrsSetMargin2(styleAttrs.c_attrs.c_style_attrs_ref, &margin);
   styleAttrs.backgroundColor = NULL;
   widgetStyle = Style::create (&styleAttrs);

   margin.left = 0;
   ffiStyleAttrsSetMargin2(styleAttrs.c_attrs.c_style_attrs_ref, &margin);
   wordStyle = Style::create (&styleAttrs);

   fontAttrs.font_attrs.size = 28;
   fontAttrs.font_attrs.weight = 700;
   styleAttrs.font = dw::core::style::Font::create (layout, &fontAttrs);
   headingStyle = Style::create (&styleAttrs);

   Fl::add_timeout (0, textTimeout, NULL);

   window->resizable(viewport);
   window->show();

   int errorCode = Fl::run();

   topWidgetStyle->unref ();
   widgetStyle->unref ();
   wordStyle->unref ();
   headingStyle->unref ();
   for (int i = 0; i < 10; i++)
      free(buttonLabel[i]);
   delete layout;

   return errorCode;
}
