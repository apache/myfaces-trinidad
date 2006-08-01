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
package org.apache.myfaces.trinidad.bean.util;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;

/**
 * Map implementation that exposes the properties of a FacesBean
 * as a Map.  This Map supports Iterator.remove(), treats
 * putting a null value as identical to removing the value,
 * but does not support null keys.  The keys may be either
 * Strings or {@link PropertyKey}s, but all Map.Entry objects
 * will return String keys.
 *
 * @author The Oracle ADF Faces Team
 */
public class ValueMap extends AbstractMap<Object, Object>
{
  public ValueMap(FacesBean bean)
  {
    _bean = bean;
  }

  @Override
  public Object get(Object key)
  {
    if (key == null)
      throw new NullPointerException();

    PropertyKey propertyKey = _getPropertyKey(key);
    // Support attribute transparency for list-based
    // properties
    if (propertyKey.isList())
    {
      Class<?> type = propertyKey.getType();
      if (type.isArray())
        type = type.getComponentType();

      return _bean.getEntries(propertyKey, type);
    }
    else
    {
      Object val = _bean.getProperty(propertyKey);
      return (val != null) ? val : propertyKey.getDefault();
    }
  }

  @Override
  public Object put(Object key, Object value)
  {
    if (key == null)
      throw new NullPointerException();

    PropertyKey propertyKey = _getPropertyKey(key);
    Object oldValue = _bean.getProperty(propertyKey);
    _bean.setProperty(propertyKey, value);
    if (_entries != null)
    {
      if (value == null)
        _entries._keys.remove(propertyKey);
      else
        _entries._keys.add(propertyKey);
    }

    return oldValue;
  }
  
  /**
   * @todo Should remove just remove values, or also remove bindings?
   */
  @Override
  public Object remove(Object key)
  {
    PropertyKey propertyKey = _getPropertyKey(key);
    Object oldValue = _bean.getProperty(propertyKey);
    _bean.setProperty(propertyKey, null);
    if (_entries != null)
      _entries._keys.remove(propertyKey);

    return oldValue;
  }

  @Override
  public Set<Map.Entry<Object, Object>> entrySet()
  {
    if (_entries == null)
    {
      HashSet<PropertyKey> keySet = new HashSet<PropertyKey>();
      keySet.addAll(_bean.keySet());
      keySet.addAll(_bean.bindingKeySet());
      _entries = new MakeEntries(keySet);
    }

    return _entries;
  }

  private class MakeEntries extends AbstractSet<Map.Entry<Object, Object>>
  {
    public MakeEntries(Set<PropertyKey> keys)
    {
      _keys = keys;
    }

    @Override
    public int size()
    {
      return _keys.size();
    }

    @Override
    public boolean remove(Object o)
    {
      if (!(o instanceof EntryImpl))
        return false;

      Object key = ((EntryImpl) o).getKey();
      _keys.remove(key);
      return (ValueMap.this.remove(key) != null);
    }

    @Override
    public Iterator<Map.Entry<Object, Object>> iterator()
    {
      final Iterator<PropertyKey> base = _keys.iterator();
      return new Iterator<Map.Entry<Object, Object>>()
      {
        public boolean hasNext()
        {
          return base.hasNext();
        }

        public Map.Entry<Object, Object> next()
        {
          _lastEntry = new EntryImpl(base.next());
          return _lastEntry;
        }

        public void remove()
        {
          if (_lastEntry == null)
            throw new IllegalStateException();
          base.remove();
          ValueMap.this.remove(_lastEntry.getKey());
          _lastEntry = null;
          //          throw new UnsupportedOperationException();
          
        }
        
        private EntryImpl _lastEntry;
      };
    }

    private Set<PropertyKey> _keys;
  }

  private class EntryImpl implements Entry<Object, Object>
  {
    public EntryImpl(PropertyKey key)
    {
      _key = key;
    }

    public Object getKey()
    {
      return _key.getName();
    }

    public Object getValue()
    {
      return get(_key);
    }

    public Object setValue(Object value)
    {
      return put(_key, value);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean equals(Object o)
    {
      if (o == this)
        return true;

      if (!(o instanceof Map.Entry))
        return false;

      Map.Entry<Object, Object> e = (Map.Entry<Object, Object>)o;
      Object k1 = getKey();
      Object k2 = e.getKey();
      if (k1 == k2 || (k1 != null && k1.equals(k2)))
      {
        Object v1 = getValue();
        Object v2 = e.getValue();
        if (v1 == v2 || (v1 != null && v1.equals(v2))) 
          return true;
      }

      return false;
    }
    
    @Override
    public int hashCode()
    {
      Object value = getValue();
      return _key.hashCode() ^ (value==null ? 0 : value.hashCode());
    }

    private final PropertyKey _key;
  }

  private PropertyKey _getPropertyKey(Object key)
  {
    if (key instanceof String)
    {
      String keyString = (String) key;
      PropertyKey propertyKey = _bean.getType().findKey(keyString);
      if (propertyKey == null)
        propertyKey = PropertyKey.createPropertyKey(keyString);
      
      return propertyKey;
    }
    
    if (key instanceof PropertyKey)
    {
      return (PropertyKey) key;
    }
    
    throw new ClassCastException();
  }

  private FacesBean    _bean;
  private MakeEntries  _entries;
}
