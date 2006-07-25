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
package org.apache.myfaces.adf.component;
import java.io.Externalizable;

import java.util.AbstractMap;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
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
final class ValueMap extends AbstractMap implements Externalizable 
{
  public ValueMap()
  {
    // noarg constructor needed for serialization
  }

  /**
   * Gets the value associated with the given key
   */
  public Object get(Object key)
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
  
  public Object put(Object key, Object value)
  {
    Object oldKey = _valueMap.put(value, key);
    assert oldKey == null : "value:"+value+" is referenced by both key:"+key+
      " and key:"+oldKey;                          

    Object old = _cache.put(key, value);
    assert old == null : "can't put the same key twice";
    return old;
  }
    
  public void clear()
  {
    _cache.clear();
    _valueMap.clear();
  }
  
  public int size()
  {
    return _cache.size();
  }
  
  public Set entrySet()
  {
    return Collections.unmodifiableSet(_cache.entrySet());
  }
  
  private static Map _setupValueMap(Map cache)
  {
    Map valueMap = new HashMap(cache.size());
    Iterator entries = cache.entrySet().iterator();
    while(entries.hasNext())
    {
      Map.Entry entry = (Map.Entry) entries.next();
      Object old = valueMap.put(entry.getValue(), entry.getKey());
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

  public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException
  {
    _cache = (Map) in.readObject();
    _valueMap = _setupValueMap(_cache);
  }

  private Map _cache = new HashMap(13);
  private transient Map _valueMap = new HashMap(13);

  //private static final ADFLogger _LOG = ADFLogger.createADFLogger(InvertibleMap.class);
}
