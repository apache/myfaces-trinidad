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
package org.apache.myfaces.trinidadinternal.util;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Map that wraps another to provide an isolated namespace using
 * a prefix.  This is especially handy for storing properties on
 * the session in a structured manner without putting them into
 * a true "Map" - because storing in a Map breaks session failover.
 * (Session failover won't trigger on mutations of contained objects.)
 * <p>
 * Note that there is a potential design flaw;  if you create a SubKeyMap
 * for "mypackage.foo" and for "mypackage.foo.bar", all the keys in the
 * latter will actually show up in the former (prefixed by ".bar").  This
 * "flaw" is actually relied on by PageFlowScopeMap (since it provides
 * a handy way to clear out all descendents), so don't "fix" it!
 */
final public class SubKeyMap extends AbstractMap
{
  public SubKeyMap(Map base, String prefix)
  {
    if (base == null)
      throw new NullPointerException();
    if (prefix == null)
      throw new NullPointerException();

    // Optimize the scenario where we're wrapping another SubKeyMap
    if (base instanceof SubKeyMap)
    {
      _base = ((SubKeyMap) base)._base;
      _prefix = ((SubKeyMap) base)._prefix + prefix;
    }
    else
    {
      _base = base;
      _prefix = prefix;
    }
  }

  public boolean isEmpty()
  {
    return entrySet().isEmpty();
  }

  public Object get(Object key)
  {
    key = _getBaseKey(key);
    return _base.get(key);
  }

  public Object put(Object key, Object value)
  {
    key = _getBaseKey(key);
    return _base.put(key, value);
  }


  public Object remove(Object key)
  {
    key = _getBaseKey(key);
    _LOG.finest("Removing {0}", key);
    return _base.remove(key);
  }

  public boolean containsKey(Object key)
  {
    if (!(key instanceof String))
      return false;

    return _base.containsKey(_getBaseKey(key));
  }

  public Set entrySet()
  {
    if (_entrySet == null)
      _entrySet = new Entries(_base.entrySet());
    return _entrySet;
  }

  private String _getBaseKey(Object key)
  {
    if (key == null)
      throw new NullPointerException();
    // Yes, I want a ClassCastException if it's not a String
    return _prefix + ((String) key);
  }

  private String _getSubKey(Object key)
  {
    if (key == null)
      return null;

    if (!(key instanceof String))
      return null;

    String keyStr = (String) key;
    if (!keyStr.startsWith(_prefix))
      return null;

    return keyStr.substring(_prefix.length());
  }


  private List _gatherKeys()
  {
    List list = new ArrayList();
    Iterator keys = _base.keySet().iterator();
    while (keys.hasNext())
    {
      Object key = keys.next();
      if ((key instanceof String) && ((String) key).startsWith(_prefix))
        list.add(key);
    }

    return list;
  }

  //
  // Set implementation for SubkeyMap.entrySet()
  //
  private class Entries extends AbstractSet
  {
    public Entries(Set baseSet)
    {
      _baseSet = baseSet;
    }

    public Iterator iterator()
    {
      // Sadly, if you just try to use a filtering approach
      // on the iterator, you'll get concurrent modification
      // exceptions.  Consequently, gather the keys in a list
      // and iterator over that.
      List keyList = _gatherKeys();
      return new EntryIterator(keyList.iterator());
    }

    public int size()
    {
      int size = 0;
      Iterator keys = _base.keySet().iterator();
      while (keys.hasNext())
      {

        Object key = keys.next();
        if ((key instanceof String) && ((String) key).startsWith(_prefix))
          size++;
      }

      return size;
    }

    public boolean isEmpty()
    {
      Iterator keys = _base.keySet().iterator();
      while (keys.hasNext())
      {
        Object key = keys.next();
        // Short-circuit:  the default implementation would always
        // need to iterate to find the total size.
        if ((key instanceof String) && ((String) key).startsWith(_prefix))
          return false;
      }

      return true;
    }

    public void clear()
    {
      Iterator keys = _base.keySet().iterator();
      while (keys.hasNext())
      {
        Object key = keys.next();
        if ((key instanceof String) && ((String) key).startsWith(_prefix))
        {
          _LOG.finest("Clearing out {0}", key);
          keys.remove();
        }
      }
    }

    private Set _baseSet;
  }

  private class EntryIterator implements Iterator
  {
    public EntryIterator(Iterator iterator)
    {
      _iterator = iterator;
    }

    public boolean hasNext()
    {
      return _iterator.hasNext();
    }

    public Object next()
    {
      String baseKey = (String) _iterator.next();
      _currentKey = baseKey;
      return new Entry(baseKey);
    }

    public void remove()
    {
      if (_currentKey == null)
        throw new IllegalStateException();

      _base.remove(_currentKey);

      _currentKey = null;
    }

    private Iterator  _iterator;
    private String    _currentKey;
  }

  private class Entry implements Map.Entry
  {
    public Entry(String baseKey)
    {
      _baseKey = baseKey;
    }

    public Object getKey()
    {
      if (_key == null)
        _key = _baseKey.substring(_prefix.length());
      return _key;
    }

    public Object getValue()
    {
      return _base.get(_baseKey);
    }

    public Object setValue(Object value)
    {
      return _base.put(_baseKey, value);
    }

    public boolean equals(Object o)
    {
      if (!(o instanceof Map.Entry))
        return false;
      Map.Entry e = (Map.Entry)o;
      return _equals(getKey(), e.getKey()) &&
      _equals(getValue(), e.getValue());
    }

    public int hashCode()
    {
      Object key = getKey();
      Object value = getValue();
      return ((key   == null)   ? 0 :   key.hashCode()) ^
      ((value == null)   ? 0 : value.hashCode());
    }

    private String    _baseKey;
    private Object    _key;
  }

  static private boolean _equals(Object a, Object b)
  {
    if (a == null)
      return b == null;
    return a.equals(b);
  }

  private final Map    _base;
  private final String _prefix;
  private Set          _entrySet;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SubKeyMap.class);
}
