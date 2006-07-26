/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidad.context;

import java.util.HashMap;

/**
 * Factory for creating RequestContext objects.
 * 
 * @author The Oracle ADF Faces Team
 */
abstract public class RequestContextFactory
{
  /**
   * Retrieve the current RequestContextFactory.
   */
  static public RequestContextFactory getFactory()
  {
    synchronized (_FACTORIES)
    {
      return (RequestContextFactory) _FACTORIES.get(_getClassLoader());
    }
  }

  /**
   * Store the current RequestContextFactory.
   */
  static public void setFactory(RequestContextFactory factory)
  {
    synchronized (_FACTORIES)
    {
      ClassLoader cl = _getClassLoader();
      if (_FACTORIES.get(cl) != null)
      {
        throw new IllegalStateException(
          "Factory already available for this class loader.");
      }

      _FACTORIES.put(cl, factory);
    }
  }

  /**
   * Create a RequestContext.
   * @todo do we need to pass the servlet objects, or can we rely
   * on their being available via the FacesContext?
   */
  abstract public RequestContext createContext(Object context,
                                                Object request);

  static private ClassLoader _getClassLoader()
  {
    return Thread.currentThread().getContextClassLoader();
  }

  static private final HashMap _FACTORIES = new HashMap();
}
