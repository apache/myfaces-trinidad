/*
 * Copyright  2000-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.myfaces.adfinternal.image.laf.browser;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageProducer;
import java.util.Map;

import org.apache.myfaces.adfinternal.util.nls.LocaleUtils;
import org.apache.myfaces.adfinternal.util.IntegerUtils;

import org.apache.myfaces.adfinternal.image.ImageConstants;
import org.apache.myfaces.adfinternal.image.ImageContext;
import org.apache.myfaces.adfinternal.image.ImageRenderer;
import org.apache.myfaces.adfinternal.image.painter.ImageLoader;
import org.apache.myfaces.adfinternal.image.painter.ImageUtils;

import org.apache.myfaces.adfinternal.style.util.GraphicsUtils;

import java.awt.image.BufferedImage;



/**
 * ImageRenderer for flipped icons
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/laf/browser/FlippedIconImageRenderer.java#0 $) $Date: 10-nov-2005.19:05:09 $
 * @author The Oracle ADF Faces Team
 */
public class FlippedIconImageRenderer implements ImageRenderer
{
  /**
   * Implementation of ImageRenderer.renderImage().
   */
  public Image renderImage(
    ImageContext context,
    Map requestedProperties,
    Map responseProperties
    )
  {
    // Make sure we've got a graphical environment before we try rendering.
    if (!GraphicsUtils.isGraphicalEnvironment())
      return null;


    Image icon = SourceUtils.getSourceIcon(context, requestedProperties);

    // if we can't find the icon, return null
    if (icon == null)
    {
      return null;
    }

    Toolkit toolkit = Toolkit.getDefaultToolkit();

    ImageProducer producer = icon.getSource();

    // If direction is RTL, flip the source image
    if (_isRightToLeft(context))
      producer = new FilteredImageSource(producer, new MirrorImageFilter());

    Image flippedIcon =
      toolkit.createImage(producer);

    // Be sure that the image is fully loaded
    ImageUtils.loadImage(flippedIcon);

    // Get the width/height.  We use an ImageLoader object the ImageObserver
    // just for the heck of it.
    ImageLoader loader = new ImageLoader(icon);
    int width = icon.getWidth(loader);
    int height = icon.getHeight(loader);

    // Store width/height for client
    if (width != -1)
    {
      responseProperties.put(ImageConstants.WIDTH_RESPONSE_KEY,
                             IntegerUtils.getInteger(width));
    }

    if (height != -1)
    {
      responseProperties.put(ImageConstants.HEIGHT_RESPONSE_KEY,
                             IntegerUtils.getInteger(height));
    }

    // This very strange code is here to work around problems
    // with colorized icon generation in which garbage pixels are
    // randomly generated for no apparent reason.  (See bug 1398379.)
    // The idea here is that by copying the colorized data into a
    // BufferedImage, we remove the colorization filter from the
    // image encoding pipeline.  That is, when we go to encode the
    // image using an ImageEncoder, the colorized image data will be
    // available in the BufferedImage's data buffer.  Otherwise, it
    // seems as if color filtering gets re-run during image encoding.
    // Admittedly, we aren't really fixing the underlying problem here -
    // the image encoding pipeline needs to be examined.  However, this
    // small change seems to reduce the likelihood of garbage pixels
    // showing up in our colorized images, so what the heck.
    BufferedImage bufferedIcon = new BufferedImage(width,
                                                   height,
                                                BufferedImage.TYPE_4BYTE_ABGR);

    Graphics g = bufferedIcon.getGraphics();

    g.drawImage(flippedIcon, 0, 0, loader);

    // Free up resources used by any images we created
    flippedIcon.flush();

    return bufferedIcon;
  }

  // Tests whether the requested direction is RTL
  private boolean _isRightToLeft(
    ImageContext context
    )
  {
    int direction = context.getLocaleContext().getReadingDirection();

    return (direction == LocaleUtils.DIRECTION_RIGHTTOLEFT);
  }

}
