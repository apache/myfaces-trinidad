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
package org.apache.myfaces.adfinternal.agent;

import java.util.Set;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.AbstractMap;
import java.util.HashSet;
import java.util.Collections;

/**
 * Pretty much the same Impl of UIX CapabilityMap, with following changes
 * -- Actually a map implementation now
 * -- added support to get Entries (To support a map interface)
 * -- Also this impl assumes that the number of keys **will** change (increase) after init.
 *
 */
public class CapabilityMap extends AbstractMap implements Cloneable
{

  private CapabilityMap()
  {
  }

  CapabilityMap(Object[][] keyValuesArr)
  {

    int arrayCount = keyValuesArr.length;
    CapabilityKey[][] keyArrays = new CapabilityKey[arrayCount][];

    for (int arrayIndex = 0; arrayIndex < arrayCount; arrayIndex++)
    {
      keyArrays[arrayIndex] = new CapabilityKey[
              keyValuesArr[arrayIndex] == null ? 0 : (keyValuesArr[arrayIndex].length >> 1)];
    }

    //Get All the Keys
    //determine how many keys we need. The keys list/count could change hence
    //need to find max key index.
    int maxKeyIndex = 0;
    for (int arrayIndex = 0; arrayIndex < arrayCount; arrayIndex++)
    {
      Object[] keyValues = keyValuesArr[arrayIndex];
      CapabilityKey[] keys = keyArrays[arrayIndex];
      int keyCount = keys.length;

      for (int i = 0; i < keyCount; i++)
      {
        keys[i] = (CapabilityKey) keyValues[i << 1];
        maxKeyIndex = _max(keys[i].getIndex(), maxKeyIndex);
      }
    }

    _indexedValues = new Object[maxKeyIndex + 1];
    for (int arrayIndex = 0; arrayIndex < arrayCount; arrayIndex++)
    {
      Object[] keyValues = keyValuesArr[arrayIndex];
      CapabilityKey[] keys = keyArrays[arrayIndex];
      int keyCount = keys.length;

      for (int i = 0; i < keyCount; i++)
      {
        _indexedValues[keys[i].getIndex()] = keyValues[(i << 1) + 1];
      }
    }
  }


  /**
   * @param capKey
   * @return value object for the capability
   */
  public Object getCapability(CapabilityKey capKey)
  {
    int keyIndex = capKey.getIndex();

    if (keyIndex < _indexedValues.length)
      return _indexedValues[keyIndex];

    return null;
  }

  /**
   * @param capabilities
   * @return  returns a new capability map that merges key/values of the provided map
   */
  public CapabilityMap merge(Map capabilities)
  {
    if ((capabilities == null) || (capabilities.isEmpty()))
      return this;

    return merge(_getMapAsArray(capabilities));
  }


  /**
   * @param capabilities
   * @return
   */
  //Doing this as current Impl. uses this method
  public CapabilityMap merge(Object[] capabilities)
  {
    // could simply do this but .....
    // new CapabilitiesImpl (new Object[][] {_getCapabilitiesAsArray(),
    //                                       capabilities});

    if ((capabilities == null) || (capabilities.length <= 0))
      return this;

    int maxKeyIndex = _indexedValues.length;
    Object[] capKeys = new Object[capabilities.length >> 1];
    Object[] capValues = new Object[capabilities.length >> 1];
    for (int i = 0, j = 0; i < capabilities.length - 1; i++)
    {
      CapabilityKey capKey = (CapabilityKey) capabilities[i++];
      capKeys[j] = capKey;
      capValues[j++] = capabilities[i];
      maxKeyIndex = _max(capKey.getIndex(), maxKeyIndex);
    }

    //new structures
    Object[] indexedValues = new Object[maxKeyIndex + 1];

    //Copy the values
    System.arraycopy(_indexedValues, 0, indexedValues, 0, _indexedValues.length);
    for (int i = 0; i < capKeys.length; i++)
    {
      CapabilityKey capKey = (CapabilityKey) capKeys[i];
      int keyIndex = capKey.getIndex();
      indexedValues[keyIndex] = capValues[i];
    }

    CapabilityMap newImpl = new CapabilityMap();
    newImpl._indexedValues = indexedValues;
    return newImpl;
  }

  //Implementation of Map Methods
  /**
   * @param key
   * @return
   */
  public Object get(Object key)
  {
    if (key == null)
      return null;

    if (key instanceof CapabilityKey)
      return getCapability((CapabilityKey) key);

    return _get(key.toString());
  }

  /**
   * @param key
   * @param value
   * @return
   */
  public Object put(Object key, Object value)
  {
    return new UnsupportedOperationException();
  }

  /**
   * @return
   */
  public Set entrySet()
  {
    if (_entrySet == null)
      _createEntrySet();

    return _entrySet;
  }

  public Object clone()
  {
    try
    {
      CapabilityMap that = (CapabilityMap) super.clone();
      that._indexedValues = (Object[]) _indexedValues.clone();
      that._entrySet = null;
      return that;
    }
    catch (CloneNotSupportedException cnse)
    {
      assert false;
      return null;
    }
  }

  private Object _get(String key)
  {
    //key == null should be done before
    CapabilityKey key0 = null;
    Object value = null;
    key0 = CapabilityKey.getCapabilityKey(key);
    if (key0 != null)
      value = getCapability(key0);

    return value;
  }


  synchronized private void _createEntrySet()
  {
    if (_entrySet == null)
    {
      HashSet hs = new HashSet();
      Iterator iter = new KeyIterator();
      while (iter.hasNext())
      {
        CapabilityKey capKey = (CapabilityKey) iter.next();
        Object value = getCapability(capKey);
        CEntry ce = new CEntry(capKey.getCapabilityName(), value);
        hs.add(ce);
      }
      //Don't allow clear(), remove(), etc etc on the map
      _entrySet = Collections.unmodifiableSet(hs);
    }
  }


  private int _max(int value1, int value2)
  {
    return (value1 > value2 ? value1 : value2);
  }


  private Object[] _getMapAsArray(Map capabilities)
  {
    Iterator iter = capabilities.keySet().iterator();
    Object[] caps = new Object[capabilities.size() * 2];
    int i = 0;
    while (iter.hasNext())
    {
      Object key = iter.next();
      CapabilityKey capKey = key instanceof CapabilityKey
                             ? (CapabilityKey) key
                             : CapabilityKey.getCapabilityKey((String) key);
      caps[i++] = capKey;
      caps[i++] = capabilities.get(key);
    }
    return caps;
  }


  //KeyIterator
  private class KeyIterator implements Iterator
  {
    public KeyIterator()
    {
      _setNext();
    }

    public void remove()
    {
      throw new UnsupportedOperationException();
    }

    public boolean hasNext()
    {
      if (_next != null)
        return true;
      return false;
    }

    public Object next()
    {
      if (_next == null)
        _setNext();

      if (_next != null)
      {
        CapabilityKey next = _next;
        _setNext();
        return next;
      }

      throw new NoSuchElementException();
    }

    private void _setNext()
    {
      _next = null;
      for (int i = _current; i < _indexedValues.length; i++)
      {
        if (_indexedValues[i] != null)
        {
          _next = CapabilityKey.getKeyAt(i);
          _current = ++i;
          break;
        }
      }
    }

    private int _current = 0;
    private CapabilityKey _next = null;

  }


  //Map Entry
  private class CEntry implements Entry
  {

    private Object key;
    private Object value;

    CEntry(Object key, Object value)
    {
      this.key = key;
      this.value = value;
    }

    public boolean equals(Object o)
    {
      if (this == o)
        return true;

      if (!(o instanceof CEntry))
        return false;

      CEntry ce = (CEntry) o;
      return ((ce.key == key || ce.key.equals(key)) &&
              (value == null ? ce.value == null : ce.value.equals(value)));
    }

    public Object getKey()
    {
      return key;
    }

    public Object getValue()
    {
      return value;
    }

    public Object setValue(Object newValue)
    {
      throw new UnsupportedOperationException();
    }

    public int hashCode()
    {
      return key.hashCode() ^ (value == null ? 0 : value.hashCode());
    }
  }


  private Object[] _indexedValues;
  private Set _entrySet;
}
