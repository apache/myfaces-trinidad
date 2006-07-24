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
package org.apache.myfaces.adfinternal.ui.laf.base;


import org.apache.myfaces.adfinternal.image.ImageProviderResponse;
import org.apache.myfaces.adfinternal.ui.RenderingContext;

/**
 * Abstracts out the retrieval of ImageProviderResponses for
 * icon indices.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/ColorizedLafIconProvider.java#0 $) $Date: 10-nov-2005.18:52:59 $
 * @author The Oracle ADF Faces Team
 */
public abstract class ColorizedLafIconProvider extends LafIconProvider
                           implements ColorizedIconProvider
{
  
  /**
   * Retrieves the ImageProviderReponse for the image indentified
   * by the iconKey
   */
  public abstract ImageProviderResponse getColorizedIcon(
    RenderingContext context,
    IconKey          iconKey);
  



  /**
   * Returns the URI to the icon indentified by the icon key
   */
  public String getColorizedIconURI(
    RenderingContext context,
    IconKey          iconKey
    )
  {
    ImageProviderResponse response = getColorizedIcon(context, iconKey);
    
    if (response != null)
    {
      return getCacheImageURI(context) + response.getImageURI();
    }
    else
    {
      return null;
    }
  }

}
