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
package org.apache.myfaces.trinidadinternal.ui.collection;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;

import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidadinternal.ui.RenderingContext;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/MapContextMap.java#0 $) $Date: 10-nov-2005.18:57:34 $
 * @author The Oracle ADF Faces Team
 */
public final class MapContextMap implements ContextMap
{
  public MapContextMap()
  {
    this(null);
  }

  public MapContextMap(
    Map map
    )
  {
    if (map == null)
      map = createDefaultMap();

    _map = map;
  }

  public Object get(
    RenderingContext context,
    Object           key
    )
  {
    return _map.get(key);
  }

  public void set(
    Object  key,
    Object  value
    )
  {
    if (value == null)
    {
      _map.remove(key);
    }
    else
    {
      _map.put(key, value);
    }
  }

  public Iterator keys(
    RenderingContext context
    )
  {  
    if(_map instanceof ArrayMap)
    {
      return ((ArrayMap) _map).keys();
    }
    return _map.keySet().iterator();
  }

  public int size()
  {
    return _map.size();
  }

  protected Map createDefaultMap()
  {
    return new HashMap(13);
  }

  private Map _map;
}
