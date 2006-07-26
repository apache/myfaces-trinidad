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
package org.apache.myfaces.adfinternal.el;

import java.util.AbstractMap;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import org.apache.myfaces.adfinternal.share.util.FastMessageFormat;

/**
 * Map implementation that provides a functor for message formatting.
 * <p>
 * @author The Oracle ADF Faces Team
 */
public class FormatterMap extends AbstractMap
{
  static public Map sharedInstance()
  {
    return _INSTANCE;
  }

  private FormatterMap()
  {
  }

  public Object get(Object key)
  {
    if (key == null)
      return Collections.EMPTY_MAP;

    return new FunctorMap(key);
  }

  public Set entrySet()
  {
    return Collections.EMPTY_SET;
  }

  static private final class FunctorMap extends AbstractMap
  {
    public FunctorMap(Object key)
    {
      // Assumes check against null above
      _format = new FastMessageFormat(key.toString());
    }

    public Object get(Object key)
    {
      return _format.format(new Object[]{key});
    }

    public Set entrySet()
    {
      return Collections.EMPTY_SET;
    }
    
    private final FastMessageFormat _format;
  }

  
  static private final Map _INSTANCE = new FormatterMap();
}
