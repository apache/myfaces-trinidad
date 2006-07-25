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

import java.util.Map;

import java.awt.Image;


import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adfinternal.image.ImageContext;
import org.apache.myfaces.adfinternal.image.ImageRenderer;

/**
 * UnsupportedImageRenderer is a simple image renderer
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/laf/browser/UnsupportedImageRenderer.java#0 $) $Date: 10-nov-2005.19:05:14 $
 * @author The Oracle ADF Faces Team
 */
class UnsupportedImageRenderer implements ImageRenderer
{
  /**
   * Returns the image renderer.
   */
  static public ImageRenderer sharedInstance()
  {
    return _sInstance;
  }

  /**
   * Render and return an image given the Map of
   * properties that describe what to render.  In addition
   * to returning the rendered image, the image renderer may
   * store additional properties describing the generated image
   * (such as its width and height) in the provided responseProperties
   * Map.
   *
   * @param context The rendering context
   * @param requestedProperties Map of requested properties.
   *   The keys for this Map are the KEY constants defined
   *   in ImageConstants.
   * @param responseProperties Map for response properties.
   *   The keys for this Map are the RESPONSE_KEY constants
   *   defined in ImageConstants.
   * @return an Image containing the rendered results
   * @see ImageContext
   * @see ImageConstants
   */
  public Image renderImage(
    ImageContext context,
    Map requestedProperties,
    Map responseProperties
    )
  {
    if ((_LOG.isWarning()))
      _LOG.warning(_CANT_RENDER_MESSAGE);

    return null;
  }

  static private final UnsupportedImageRenderer _sInstance =
    new UnsupportedImageRenderer();

  static private final String _CANT_RENDER_MESSAGE =
    "Dynamic image rendering is not supported on JDK 1.1.  You must\n" +
    "pregenerate the image cache.";
  private static final ADFLogger _LOG = ADFLogger.createADFLogger(UnsupportedImageRenderer.class);
}
