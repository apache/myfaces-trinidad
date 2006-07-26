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

package org.apache.myfaces.adfinternal.image;

import java.util.Map;



/**
 * Implementation of ImageProviderRequest.
 *
 * @see ImageProviderRequest
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/ImageProviderRequestImpl.java#0 $) $Date: 10-nov-2005.19:03:54 $
 * @author The Oracle ADF Faces Team
 */
public class ImageProviderRequestImpl implements ImageProviderRequest
{
  private ImageProviderRequestImpl() {}

  /**
   * Creates an ImageProviderRequestImpl with the specified
   * namespace, name and render properties.
   */
  public ImageProviderRequestImpl(
    String     namespaceURI,
    String     name,
    Map renderProperties)
  {
    if (namespaceURI == null) 
    {
      throw new NullPointerException("Null namespaceURI");
    }
    
    if (name == null) 
    {
      throw new NullPointerException("Null name");
    }

    _namespace = namespaceURI;
    _name = name;
    _properties = renderProperties;
  }

  /**
   * Implementation of ImageProviderRequest.getNamespaceURI().
   */
  public String getNamespaceURI()
  {
    return _namespace;
  }

  /**
   * Implementation of ImageProviderRequest.getLocalName().
   */
  public String getLocalName()
  {
    return _name;
  }

  /**
   * Implementation of ImageProviderRequest.getRenderProperties().
   */
  public Map getRenderProperties(ImageContext context)
  {
    return _properties;
  }

  private String     _namespace;
  private String     _name;
  private Map _properties;
}
