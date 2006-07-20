/*
 * Copyright 2006 The Apache Software Foundation.
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
package org.apache.myfaces.adf.util;

import java.util.Map;

import javax.faces.context.FacesContext;

/**
 * Utility methods for Renderers which support partial page rendering.
 * <p>
 * @author The Apache Trinidad Podling
 */
public final class PartitialPageRenderingUtils
{
  
  private PartitialPageRenderingUtils()
  { 
    
  }
  
  /**
   * Utility method to indicate if this current HTTP request is a
   * partial page rendering request.
   * 
   * @param context the <code>FacesContext</code> object for
   * the request we are processing
   * @return is this request a PPR request?
   */
  public static boolean isPartialRequest(FacesContext context)
  {
    Map requestMap = context.getExternalContext().getRequestMap();
    if (Boolean.TRUE.equals(requestMap.get(_PARTIAL_KEY)))
      return true;

    Map parameters = context.getExternalContext().getRequestParameterMap();
    if ("true".equals(parameters.get("partial")))
      return true;

    return false;
  }
  
  /*
   * key, used for the PPR feature, inside the internal API.
   */
  private static final String _PARTIAL_KEY =
    "org.apache.myfaces.adfinternal.ForcedPartialRequest";
}