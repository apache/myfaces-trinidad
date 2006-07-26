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

package org.apache.myfaces.adfinternal.image.cache;

import java.util.Map;



import org.apache.myfaces.adfinternal.image.ImageContext;

/**
 * The PropertiesFilter interface is designed to allow type-specific
 * filtering to be performed on the client-provided "requested" properties
 * prior to rendering an image.  Filtering might be performed to enhance
 * or modify the client-provided properties.  For example, the filter
 * might add type-specific default values for properties that are not
 * specified by the client.  Type-specific filters are registered and
 * retrieved from ImageType instances using the PROPERTIES_FILTER_PROPERTY
 * key.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/PropertiesFilter.java#0 $) $Date: 10-nov-2005.19:06:11 $
 * @author The Oracle ADF Faces Team
 */
public interface PropertiesFilter
{
  /**
   * Key used to retrieve the PropertiesFilter property from an ImageType.
   */
  public static final Object PROPERTIES_FILTER_PROPERTY = "propertiesFilter";

  /**
   * Filters a dictionary of properties
   *
   * @param context The image context
   * @param properties The dictionary of properties to filter
   * @return The filtered dictionary of properties
   */
  public Map filterProperties(
    ImageContext context,
    Map properties
    );
}
