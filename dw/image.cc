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



#include "image.hh"
#include "../lout/msg.h"
#include "../lout/misc.hh"
#include "../lout/debug.hh"
#include "../src/Hello/hello.h"

namespace dw {

using namespace lout;

ImageMapsList::ImageMap::ImageMap ()
{
   shapesAndLinks = new container::typed::List <ShapeAndLink> (true);
   defaultLink = -1;
}

ImageMapsList::ImageMap::~ImageMap ()
{
   delete shapesAndLinks;
}

void ImageMapsList::ImageMap::draw (core::View *view,core::style::Style *style,
                                    int x, int y)
{
   container::typed::Iterator <ShapeAndLink> it;

   for (it = shapesAndLinks->iterator (); it.hasNext (); ) {
      ShapeAndLink *shapeAndLink = it.getNext ();

      shapeAndLink->shape->draw(view, style, x, y);
   }
}

void ImageMapsList::ImageMap::add (core::Shape *shape, int link) {
   ShapeAndLink *shapeAndLink = new ShapeAndLink ();
   shapeAndLink->shape = shape;
   shapeAndLink->link = link;
   shapesAndLinks->append (shapeAndLink);
}

int ImageMapsList::ImageMap::link (int x, int y) {
   container::typed::Iterator <ShapeAndLink> it;
   int link = defaultLink;

   for (it = shapesAndLinks->iterator (); it.hasNext (); ) {
      ShapeAndLink *shapeAndLink = it.getNext ();

      if (shapeAndLink->shape->isPointWithin (x, y)) {
         link = shapeAndLink->link;
         break;
      }
   }

   return link;
}

ImageMapsList::ImageMapsList ()
{
   imageMaps = new container::typed::HashTable <object::Object, ImageMap>
      (true, true);
   currentMap = NULL;
}

ImageMapsList::~ImageMapsList ()
{
   delete imageMaps;
}

/**
 * \brief Start a new map and make it the current one.
 *
 * This has to be called before dw::ImageMapsList::addShapeToCurrentMap.
 * "key" is owned by the image map list, so a copy should be passed, when
 * necessary.
 */
void ImageMapsList::startNewMap (object::Object *key)
{
   currentMap = new ImageMap ();
   imageMaps->put (key, currentMap);
}

/**
 * \brief Add a shape to the current map-
 *
 * "shape" is owned by the image map list, so a copy should be passed, when
 * necessary.
 */
void ImageMapsList::addShapeToCurrentMap (core::Shape *shape, int link)
{
   currentMap->add (shape, link);
}

/**
 * \brief Set default link for current map-
 */
void ImageMapsList::setCurrentMapDefaultLink (int link)
{
   currentMap->setDefaultLink (link);
}

void ImageMapsList::drawMap (lout::object::Object *key, core::View *view,
                             core::style::Style *style, int x, int y)
{
   ImageMap *map = imageMaps->get (key);

   if (map)
      map->draw(view, style, x, y);
}

int ImageMapsList::link (object::Object *key, int x, int y)
{
   int link = -1;
   ImageMap *map = imageMaps->get (key);

   if (map)
      link = map->link (x, y);

   return link;
}

// ----------------------------------------------------------------------

int Image::CLASS_ID = -1;

Image::Image(const char *altText)
{
   DBG_OBJ_CREATE ("dw::Image");
   registerName ("dw::Image", &CLASS_ID);
   this->altText = altText ? strdup (altText) : NULL;
   altTextWidth = -1; // not yet calculated
   buffer = NULL;
   clicking = false;
   currLink = -1;
   mapList = NULL;
   mapKey = NULL;
   isMap = false;
}

Image::~Image()
{
   if (altText)
      free(altText);
   if (buffer)
      buffer->unref ();
   if (mapKey)
      delete mapKey;

   DBG_OBJ_DELETE ();
}

void Image::sizeRequestImpl (core::Requisition *requisition)
{
   if (buffer) {
      DwLength aHeight = {};
      ffiStyleAttrsGetHeight(getStyle()->c_style_attrs_ref, &aHeight);
      DwLength aWidth = {};
      ffiStyleAttrsGetWidth(getStyle()->c_style_attrs_ref, &aWidth);

      if (ffiIsAutoDwLength(&aHeight) &&
          ffiIsAbsoluteDwLength (&aWidth) &&
          buffer->getRootWidth () > 0) {
         // preserve aspect ratio when only width is given
         requisition->width = ffiGetAbsoluteDwLengthValue(&aWidth);
         requisition->ascent = buffer->getRootHeight () *
                               requisition->width / buffer->getRootWidth ();
      } else if (ffiIsAutoDwLength(&aWidth) &&
                 ffiIsAbsoluteDwLength (&aHeight) &&
                 buffer->getRootHeight () > 0) {
         // preserve aspect ratio when only height is given
         requisition->ascent = ffiGetAbsoluteDwLengthValue(&aHeight);
         requisition->width = buffer->getRootWidth () *
                               requisition->ascent / buffer->getRootHeight ();
      } else {
         requisition->width = buffer->getRootWidth ();
         requisition->ascent = buffer->getRootHeight ();
      }
      requisition->descent = 0;
   } else {
      if (altText && altText[0]) {
         if (altTextWidth == -1)
            altTextWidth =
               layout->textWidth (getStyle()->font, altText, strlen (altText));

         requisition->width = altTextWidth;
         requisition->ascent = getStyle()->font->ascent;
         requisition->descent = getStyle()->font->descent;
      } else {
         requisition->width = 0;
         requisition->ascent = 0;
         requisition->descent = 0;
      }
   }

   requisition->width += ffiStyleAttrsBoxDiffWidth (getStyle()->c_style_attrs_ref);
   requisition->ascent += ffiStyleAttrsBoxOffsetY (getStyle()->c_style_attrs_ref);
   requisition->descent += ffiStyleAttrsBoxRestHeight (getStyle()->c_style_attrs_ref);
}

void Image::sizeAllocateImpl (core::Allocation *allocation)
{
   core::Imgbuf *oldBuffer;
   int dx, dy;

   /* if image is moved only */
   if (allocation->width == this->allocation.width &&
       allocation->ascent + allocation->descent == getHeight ())
      return;

   dx = ffiStyleAttrsBoxDiffWidth (getStyle()->c_style_attrs_ref);
   dy = ffiStyleAttrsBoxDiffHeight (getStyle()->c_style_attrs_ref);
#if 0
   MSG("ffiStyleAttrsBoxDiffHeight = %d + %d, buffer=%p\n",
       ffiStyleAttrsBoxOffsetY(getStyle()->c_style_attrs_ref), ffiStyleAttrsBoxRestHeight(getStyle()->c_style_attrs_ref), buffer);
   MSG("getContentWidth() = allocation.width - ffiStyleAttrsBoxDiffWidth (style->c_style_attrs_ref)"
       " = %d - %d = %d\n",
       this->allocation.width, ffiStyleAttrsBoxDiffWidth(getStyle()->c_style_attrs_ref),
       this->allocation.width - ffiStyleAttrsBoxDiffWidth(getStyle()->c_style_attrs_ref));
   MSG("getContentHeight() = getHeight() - ffiStyleAttrsBoxDiffHeight ()"
       " = %d - %d = %d\n", this->getHeight(), ffiStyleAttrsBoxDiffHeight(getStyle()->c_style_attrs_ref),
       this->getHeight() - ffiStyleAttrsBoxDiffHeight(getStyle()->c_style_attrs_ref));
#endif
   if (buffer &&
       (allocation->width - dx > 0 ||
        allocation->ascent + allocation->descent - dy > 0)) {
      // Zero content size : simply wait...
      // Only one dimension: naturally scale
      oldBuffer = buffer;
      buffer = oldBuffer->getScaledBuf (allocation->width - dx,
                                        allocation->ascent
                                        + allocation->descent - dy);
      oldBuffer->unref ();
   }
}

void Image::enterNotifyImpl (core::EventCrossing *event)
{
   // BUG: this is wrong for image maps, but the cursor position is unknown.
   currLink = ffiStyleAttrsXLink(getStyle()->c_style_attrs_ref);

   if (currLink != -1) {
      (void) layout->emitLinkEnter (this, currLink, -1, -1, -1);
   }
   Widget::enterNotifyImpl(event);
}

void Image::leaveNotifyImpl (core::EventCrossing *event)
{
   clicking = false;

   if (currLink != -1) {
      currLink = -1;
      (void) layout->emitLinkEnter (this, -1, -1, -1, -1);
   }
   Widget::leaveNotifyImpl(event);
}

/*
 * Return the coordinate relative to the contents.
 * If the event occurred in the surrounding box, return the value at the
 * edge of the contents instead.
 */
int Image::contentX (core::MousePositionEvent *event)
{
   int ret = event->xWidget - ffiStyleAttrsBoxOffsetX(getStyle()->c_style_attrs_ref);

   ret = misc::min(getContentWidth(), misc::max(ret, 0));
   return ret;
}

int Image::contentY (core::MousePositionEvent *event)
{
   int ret = event->yWidget - ffiStyleAttrsBoxOffsetY(getStyle()->c_style_attrs_ref);

   ret = misc::min(getContentHeight(), misc::max(ret, 0));
   return ret;
}

bool Image::motionNotifyImpl (core::EventMotion *event)
{
   if (mapList || isMap) {
      int x = contentX(event);
      int y = contentY(event);

      if (mapList) {
         /* client-side image map */
         int newLink = mapList->link (mapKey, x, y);
         if (newLink != currLink) {
            currLink = newLink;
            clicking = false;
            /* \todo Using MAP/AREA styles would probably be best */
            setCursor(newLink == -1 ? ((dw::core::style::Cursor) ffiStyleAttrsCursor(getStyle()->c_style_attrs_ref)) : core::style::CURSOR_POINTER);
            (void) layout->emitLinkEnter (this, newLink, -1, -1, -1);
         }
      } else if (isMap && currLink != -1) {
         /* server-side image map */
         (void) layout->emitLinkEnter (this, currLink, -1, x, y);
      }
   }
   return true;
}

bool Image::buttonPressImpl (core::EventButton *event)
{
   bool ret = false;

   currLink = mapList ? mapList->link(mapKey,contentX(event),contentY(event)) :
      ffiStyleAttrsXLink(getStyle()->c_style_attrs_ref);
   if (event->button == 3){
      (void)layout->emitLinkPress(this, currLink, ffiStyleAttrsXImg(getStyle()->c_style_attrs_ref), -1, -1,
                                  event);
      ret = true;
   } else if (event->button == 1 || currLink != -1){
      clicking = true;
      ret = true;
   }
   return ret;
}

bool Image::buttonReleaseImpl (core::EventButton *event)
{
   fprintf(stderr, "button release event for x_img = %d\n", ffiStyleAttrsXImg(getStyle()->c_style_attrs_ref));
   currLink = mapList ? mapList->link(mapKey,contentX(event),contentY(event)) :
      ffiStyleAttrsXLink(getStyle()->c_style_attrs_ref);
   if (clicking) {
      int x = isMap ? contentX(event) : -1;
      int y = isMap ? contentY(event) : -1;
      clicking = false;
      layout->emitLinkClick (this, currLink, ffiStyleAttrsXImg(getStyle()->c_style_attrs_ref), x, y, event);
      return true;
   }
   return false;
}

void Image::draw (core::View *view, core::Rectangle *area)
{
   int dx, dy;
   core::Rectangle content, intersection;

   drawWidgetBox (view, area, false);

   if (buffer) {
      dx = ffiStyleAttrsBoxOffsetX (getStyle()->c_style_attrs_ref);
      dy = ffiStyleAttrsBoxOffsetY (getStyle()->c_style_attrs_ref);
      content.x = dx;
      content.y = dy;
      content.width = getContentWidth ();
      content.height = getContentHeight ();

      if (area->intersectsWith (&content, &intersection))
         view->drawImage (buffer,
                          allocation.x + dx, allocation.y + dy,
                          intersection.x - dx, intersection.y - dy,
                          intersection.width, intersection.height);
   } else {
      core::View *clippingView;

      if (altText && altText[0]) {
         core::View *usedView = view;

         clippingView = NULL;

         if (altTextWidth == -1)
            altTextWidth =
               layout->textWidth (getStyle()->font, altText, strlen (altText));

         if ((getContentWidth() < altTextWidth) ||
             (getContentHeight() <
              getStyle()->font->ascent + getStyle()->font->descent)) {
            clippingView = usedView =
               view->getClippingView (allocation.x + ffiStyleAttrsBoxOffsetX (getStyle()->c_style_attrs_ref),
                                      allocation.y + ffiStyleAttrsBoxOffsetY (getStyle()->c_style_attrs_ref),
                                      getContentWidth(),
                                      getContentHeight());
         }

         usedView->drawSimpleWrappedText (getStyle()->font, getStyle()->color,
                             core::style::Color::SHADING_NORMAL,
                             allocation.x + ffiStyleAttrsBoxOffsetX (getStyle()->c_style_attrs_ref),
                             allocation.y + ffiStyleAttrsBoxOffsetY (getStyle()->c_style_attrs_ref),
                             getContentWidth(), getContentHeight(), altText);

         if (clippingView)
            view->mergeClippingView (clippingView);
      }
      if (mapKey) {
         clippingView = view->getClippingView (allocation.x +
                                               ffiStyleAttrsBoxOffsetX (getStyle()->c_style_attrs_ref),
                                               allocation.y +
                                               ffiStyleAttrsBoxOffsetY (getStyle()->c_style_attrs_ref),
                                               getContentWidth(),
                                               getContentHeight());
         mapList->drawMap(mapKey, clippingView, getStyle(),
                          allocation.x + ffiStyleAttrsBoxOffsetX (getStyle()->c_style_attrs_ref),
                          allocation.y + ffiStyleAttrsBoxOffsetY (getStyle()->c_style_attrs_ref));
         view->mergeClippingView (clippingView);
      }
   }

   /** TODO: draw selection */
}

core::Iterator *Image::iterator (core::Content::Type mask, bool atEnd)
{
   //return new core::TextIterator (this, mask, atEnd, altText);
   /** \bug Not implemented. */
   return new core::EmptyIterator (this, mask, atEnd);
}

void Image::setBuffer (core::Imgbuf *buffer, bool resize)
{
   core::Imgbuf *oldBuf = this->buffer;

   if (wasAllocated () && needsResize () &&
      getContentWidth () > 0 && getContentHeight () > 0) {
      // Don't create a new buffer for the transition from alt text to img,
      // and only scale when both dimensions are known.
      this->buffer =
         buffer->getScaledBuf (getContentWidth (), getContentHeight ());
   } else {
      this->buffer = buffer;
      buffer->ref ();
   }
   queueResize (0, true);

   DBG_OBJ_ASSOC_CHILD (this->buffer);

   if (oldBuf)
      oldBuf->unref ();
}

void Image::drawRow (int row)
{
   core::Rectangle area;

   assert (buffer != NULL);

   buffer->getRowArea (row, &area);
   if (area.width && area.height)
      queueDrawArea (area.x + ffiStyleAttrsBoxOffsetX (getStyle()->c_style_attrs_ref),
                     area.y + ffiStyleAttrsBoxOffsetY (getStyle()->c_style_attrs_ref),
                     area.width, area.height);
}

void Image::finish ()
{
   // Nothing to do; images are always drawn line by line.
}

void Image::fatal ()
{
   // Could display an error.
}


/**
 * \brief Sets image as server side image map.
 */
void Image::setIsMap ()
{
   isMap = true;
}


/**
 * \brief Sets image as client side image map.
 *
 * "list" is not owned by the image, the caller has to free it. "key"
 * is owned by the image, if it is used by the caller afterwards, a copy
 * should be passed.
 */
void Image::setUseMap (ImageMapsList *list, object::Object *key)
{
   mapList = list;
   if (mapKey && mapKey != key)
      delete mapKey;
   mapKey = key;
}

} // namespace dw
