/*
 * Copyright  2001-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.ui.laf.base.xhtml;

import java.awt.Color;

import java.util.Map;

import org.apache.myfaces.adfinternal.image.ImageConstants;
import org.apache.myfaces.adfinternal.image.ImageContext;

import org.apache.myfaces.adfinternal.image.cache.AccentColorizedIconKey;

import org.apache.myfaces.adfinternal.share.io.NameResolver;


/**
 * The ImageProviderRequest class that we use for requesting accent
 * colorized icons.  It extends the AccentColorizedIconKey
 * by adding support for obtaining an InputStreamProvider for the source
 * icon.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/AccentIconRequest.java#0 $) $Date: 10-nov-2005.18:53:11 $
 * @author The Oracle ADF Faces Team
 */
public final class AccentIconRequest extends AccentColorizedIconKey
{
  public AccentIconRequest(
    ImageContext context, 
    String       source, 
    Class        lookAndFeel,
    int          direction,
    Color        color,
    Color        surroundingColor,
    NameResolver resolver
    )
  {
    super(context, source, lookAndFeel, direction, color, surroundingColor);

    _resolver = resolver;
  }

  // Override of getRenderProperties() which adds in the 
  // InputStreamProvider for the source icon
  public Map getRenderProperties(ImageContext context)
  {
    Map properties = super.getRenderProperties(context);

    properties.put(ImageConstants.SOURCE_INPUT_STREAM_PROVIDER_KEY,
                   CoreIconRequest.resolveSourceIcon(getSource(),
                                                     _resolver));
                                                     
    return properties;
  }

  private NameResolver _resolver;
}
