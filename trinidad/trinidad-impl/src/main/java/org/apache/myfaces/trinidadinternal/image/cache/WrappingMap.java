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

package org.apache.myfaces.trinidadinternal.image.cache;

import java.util.Collection;
import java.util.Map;
import java.util.Iterator;
import java.util.Set;

/**
 * Map which wraps Map.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/cache/WrappingMap.java#0 $) $Date: 10-nov-2005.19:06:16 $
 * @author The Oracle ADF Faces Team
 */
class WrappingMap implements Map
{
  public WrappingMap(Map map)
  {
    _wrappedMap = map;
  }

  public int size()
  {
    return _wrappedMap.size();
  }

  public boolean isEmpty()
  {
    return _wrappedMap.isEmpty();
  }

  public Iterator keys()
  {
    return _wrappedMap.keySet().iterator();
  }

  public Set entrySet()
  {
    return _wrappedMap.entrySet();
  }

  public Set keySet()
  {
    return _wrappedMap.keySet();
  }

  public void putAll(Map map)
  {
    throw new UnsupportedOperationException("putAll operation not supported for WrappingMap");
  }

  public Collection values()
  {
    return _wrappedMap.values();
  }

  public boolean containsValue(Object value)
  {
    return _wrappedMap.containsValue(value);
  }

  public boolean containsKey(Object key)
  {
    return _wrappedMap.containsKey(key);
  }

  public void clear()
  {
    throw new UnsupportedOperationException("clear operation not supported for WrappingMap");
  }

  public Iterator elements()
  {
    return _wrappedMap.values().iterator();
  }

  public Object get(Object key)
  {
    return _wrappedMap.get(key);
  }

  public Object put(Object key, Object value)
  {
    return _wrappedMap.put(key, value);
  }

  public Object remove(Object key)
  {
    return _wrappedMap.remove(key);
  }

  protected Map getWrappedMap()
  {
    return _wrappedMap;
  }

  private Map _wrappedMap;
}
