/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adf.component;

import javax.faces.component.UIComponent;

import java.util.Iterator;
import java.util.Map;

import org.apache.myfaces.adf.util.ArrayMap;

/**
 * HashMap used for storing facets.
 *
 * @author The Oracle ADF Faces Team
 */
class FacetHashMap extends ArrayMap
{
  public FacetHashMap(UIComponent parent)
  {
    super(0, 5);
    _parent = parent;
  }

  public void clear()
  {
    Iterator values = values().iterator();
    while (values.hasNext())
    {
      UIComponent value = (UIComponent) values.next();
      value.setParent(null);
    }

    super.clear();
  }
  
  public Object put(Object key, Object value)
  {
    if ((key == null) || (value == null))
    {
      throw new NullPointerException();
    }

    else if (!(key instanceof String) ||
             !(value instanceof UIComponent))
    {
      throw new ClassCastException();
    }

    UIComponent previous = (UIComponent) super.get(key);
    if (previous != null)
    {
      previous.setParent(null);
    }
    
    UIComponent current = (UIComponent) value;
    if (current.getParent() != null)
    {
      ChildArrayList.__removeFromParent(current, -1);
    }

    current.setParent(_parent);
    return (super.put(key, value));
  }

  public void putAll(Map map)
  {
    if (map == null)
    {
      throw new NullPointerException();
    }

    Iterator keys = map.keySet().iterator();
    while (keys.hasNext())
    {
      Object key = keys.next();
      put(key, map.get(key));
    }
  }
  
  public Object remove(Object key)
  {
    UIComponent previous = (UIComponent) get(key);
    if (previous != null)
    {
      previous.setParent(null);
    }
    
    super.remove(key);
    return (previous);
  }

  private UIComponent _parent;
}
