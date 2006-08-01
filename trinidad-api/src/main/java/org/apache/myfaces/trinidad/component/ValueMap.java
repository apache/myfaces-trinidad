/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.trinidad.component;

import java.io.Externalizable;

import java.util.AbstractMap;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.io.ObjectOutput;
import java.io.IOException;
import java.io.ObjectInput;

/**
 * This class implements a Map whose values can themselves be used to obtain
 * the corresponding keys.
 * 
 * it is illegal for two keys to be paired with the same value. 
 * 
 * @author The Oracle ADF Faces Team
 */
final class ValueMap<T> extends AbstractMap<T, T> implements Externalizable 
{
  public ValueMap()
  {
    // noarg constructor needed for serialization
  }

  /**
   * Gets the value associated with the given key
   */
  @Override
  public T get(Object key)
  {
    return _cache.get(key);
  }
  
  /**
   * Gets the key associated with the given value
   */
  public Object getKey(Object value)
  {
    return _valueMap.get(value);
  }
  
  @Override
  public T put(T key, T value)
  {
    T oldKey = _valueMap.put(value, key);
    assert oldKey == null : "value:"+value+" is referenced by both key:"+key+
      " and key:"+oldKey;                          

    T old = _cache.put(key, value);
    assert old == null : "can't put the same key twice";
    return old;
  }
    
  @Override
  public void clear()
  {
    _cache.clear();
    _valueMap.clear();
  }
  
  @Override
  public int size()
  {
    return _cache.size();
  }
  
  @Override
  public Set<Map.Entry<T, T>> entrySet()
  {
    return Collections.unmodifiableSet(_cache.entrySet());
  }
  
  private static <T> Map<T, T> _setupValueMap(Map<T, T> cache)
  {
    Map<T, T> valueMap = new HashMap<T, T>(cache.size());
    for(Map.Entry<T, T> entry : cache.entrySet())
    {
      T old = valueMap.put(entry.getValue(), entry.getKey());
      assert old == null : "the value:"+entry.getValue()+
                           " was bound to both key:"+old+
                           " and key:"+entry.getKey();
    }
    
    return valueMap;
  }
  
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(_cache);
  }

  @SuppressWarnings("unchecked")
  public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException
  {
    _cache = (Map<T, T>) in.readObject();
    _valueMap = _setupValueMap(_cache);
  }

  private Map<T, T> _cache = new HashMap<T, T>(13);
  private transient Map<T, T> _valueMap = new HashMap<T, T>(13);

  //private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(InvertibleMap.class);
}
