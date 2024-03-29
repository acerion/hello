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
#include "../dw/textblock.hh"
#include "../dw/ui.hh"

using namespace dw;
using namespace dw::core;
using namespace dw::core::style;
using namespace dw::core::ui;
using namespace dw::fltk;

int main(int argc, char **argv)
{
   FltkPlatform *platform = new FltkPlatform ();
   Layout *layout = new Layout (platform);

   Fl_Window *window = new Fl_Window(410, 210, "Dw Resource test");
   window->box(FL_NO_BOX);
   window->begin();

   FltkViewport *viewport = new FltkViewport (0, 0, 410, 210);
   layout->attachView (viewport);

   StyleAttrs styleAttrs;
   styleAttrs.initValues ();
   ffiStyleAttrsSetMargin(styleAttrs.c_style_attrs_ref, 5);

   FontAttrs fontAttrs;
   fontAttrs.font_attrs.name = "Bitstream Charter";
   fontAttrs.font_attrs.size = 14;
   fontAttrs.font_attrs.weight = 400;
   fontAttrs.font_attrs.style = FONT_STYLE_NORMAL;
   fontAttrs.font_attrs.letterSpacing = 0;
   fontAttrs.font_attrs.fontVariant = FONT_VARIANT_NORMAL;
   styleAttrs.font = dw::core::style::Font::create (layout, &fontAttrs);

   styleAttrs.color = Color::create (layout, 0x000000);
   styleAttrs.backgroundColor = Color::create (layout, 0xffffff);

   Style *widgetStyle = Style::create (&styleAttrs);

   Textblock *textblock = new Textblock (false);
   textblock->setStyle (widgetStyle);
   layout->setWidget (textblock);

   widgetStyle->unref();

   ffiStyleAttrsSetMargin(styleAttrs.c_style_attrs_ref, 0);
   styleAttrs.backgroundColor = NULL;

   widgetStyle = Style::create (&styleAttrs);

   SelectionResource *res = layout->getResourceFactory()->createListResource
      (ListResource::SELECTION_AT_MOST_ONE, 4);
   //SelectionResource *res =
   //   layout->getResourceFactory()->createOptionMenuResource ();

   Embed *embed = new Embed (res);
   textblock->addWidget (embed, widgetStyle);
   textblock->addSpace (widgetStyle);

   widgetStyle->unref();

   for(int i = 0; i < 50; i++)
      res->addItem ("Hello, world!", true, i == 0 ? true : false);

   textblock->flush ();

   window->resizable(viewport);
   window->show();

   int errorCode = Fl::run();

   delete layout;

   return errorCode;
}
