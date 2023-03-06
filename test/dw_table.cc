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



#include <FL/Fl.H>
#include <FL/Fl_Window.H>

#include "../dw/core.hh"
#include "../dw/fltkcore.hh"
#include "../dw/fltkviewport.hh"
#include "../dw/table.hh"
#include "../dw/tablecell.hh"

using namespace dw;
using namespace dw::core;
using namespace dw::core::style;
using namespace dw::fltk;

int main(int argc, char **argv)
{
   FltkPlatform *platform = new FltkPlatform ();
   Layout *layout = new Layout (platform);

   Fl_Window *window = new Fl_Window(300, 300, "Dw Table");
   window->box(FL_NO_BOX);
   window->begin();

   FltkViewport *viewport = new FltkViewport (0, 0, 300, 300);
   layout->attachView (viewport);

   StyleAttrs styleAttrs;
   styleAttrs.initValues ();
   styleMarginSetVal(&styleAttrs.margin, 5);
   stylePaddingSetVal(&styleAttrs.padding, 0);
   borderWidthSetVal(&styleAttrs.borderWidth, 1);
   ffiStyleAttrsSetBorderStyle(styleAttrs.c_attrs.c_style_attrs_ref, BORDER_OUTSET);
   styleAttrs.setBorderColor (Color::create (layout, 0xffffff));
   styleAttrs.color = Color::create (layout, 0x000000);
   styleAttrs.backgroundColor = Color::create (layout, 0xffffff);
   ffiStyleAttrsSetHorizBorderSpacing(styleAttrs.c_attrs.c_style_attrs_ref, 5);
   ffiStyleAttrsSetVertBorderSpacing(styleAttrs.c_attrs.c_style_attrs_ref, 5);

   FontAttrs fontAttrs;
   fontAttrs.font_attrs.name = "Bitstream Charter";
   fontAttrs.font_attrs.size = 14;
   fontAttrs.font_attrs.weight = 400;
   fontAttrs.font_attrs.style = FONT_STYLE_NORMAL;
   fontAttrs.font_attrs.letterSpacing = 0;
   fontAttrs.font_attrs.fontVariant = FONT_VARIANT_NORMAL;
   styleAttrs.font = dw::core::style::Font::create (layout, &fontAttrs);

   Style *tableStyle = Style::create (&styleAttrs);

   Table *table = new Table (false);
   table->setStyle (tableStyle);
   layout->setWidget (table);

   tableStyle->unref();

   ffiStyleAttrsSetBorderStyle(styleAttrs.c_attrs.c_style_attrs_ref, BORDER_INSET);
   styleAttrs.backgroundColor = NULL;
   styleMarginSetVal(&styleAttrs.margin, 0);
   stylePaddingSetVal(&styleAttrs.padding, 5);

   Style *cellStyle = Style::create (&styleAttrs);

   borderWidthSetVal(&styleAttrs.borderWidth, 0);
   styleMarginSetVal(&styleAttrs.margin, 0);
   ffiStyleAttrsSetCursor(styleAttrs.c_attrs.c_style_attrs_ref, CURSOR_TEXT);

   Style *wordStyle = Style::create (&styleAttrs);

   for (int i = 0; i < 4; i++) {
      table->addRow (wordStyle);

      for (int j = 0; j < 4; j++) {
         Textblock *cell = new Textblock (false);
         cell->setStyle (cellStyle);
         table->addCell (cell, 1, 1);

         char buf[10];
         sprintf (buf, "cell %c", 'A' + 4 * i + j);

         cell->addText (buf, wordStyle);
         cell->flush ();
      }
   }

   wordStyle->unref();
   cellStyle->unref();

   window->resizable(viewport);
   window->show();
   int errorCode = Fl::run();

   delete layout;

   return errorCode;
}
