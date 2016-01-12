/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadbuild.test;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

/**
 * Base class for Map test cases.
 */
abstract public class MapTestCase extends TestCase
{
  public MapTestCase(String testName)
  {
    super(testName);
  }

  public void testInitialState()
  {
    Map<String, Object> map = createMap();
    assertEquals(0, map.size());
    assertTrue(map.isEmpty());
    assertTrue(map.entrySet().isEmpty());
    assertTrue(map.keySet().isEmpty());
    assertTrue(map.values().isEmpty());
    assertTrue(!map.entrySet().iterator().hasNext());
  }

  public void testPut()
  {
    Map<String, Object> map = createMap();
    assertNull(map.put("foo", "bar"));
    assertEquals(1, map.size());
    assertEquals("bar", map.get("foo"));
    assertTrue(map.containsKey("foo"));
    assertTrue(map.containsValue("bar"));

    assertTrue(map.keySet().contains("foo"));
    assertTrue(map.values().contains("bar"));

    assertEquals("bar", map.put("foo", "baz"));
    assertEquals(1, map.size());
    assertEquals("baz", map.get("foo"));

    assertTrue(map.containsKey("foo"));
    assertTrue(map.containsValue("baz"));
    assertTrue(!map.containsValue("bar"));
  }

  public void testPutAll()
  {
    Map<String, Object> map = createMap();
    HashMap<String, Object> hashMap = new HashMap<String, Object>();
    _putTwo(hashMap);
    
    map.putAll(hashMap);
    assertEquals(2, map.size());
    assertTrue(map.containsKey("first"));
    assertEquals(ONE, map.get("first"));
    assertTrue(map.containsKey("second"));
    assertEquals(TWO, map.get("second"));
  }


  public void testPutNull()
  {

    Map<String, Object> map = createMap();

    // Test putting a null value
    try
    {
      map.put("foo", null);
    }
    catch (NullPointerException e)
    {
      if (supportsNullValues())
        fail();
    }

    if (supportsNullValues())
    {
      if (isNullRemove())
      {
        assertEquals(0, map.size());
        assertTrue(!map.containsKey("foo"));
        assertTrue(!map.containsValue(null));
      }
      else
      {
        assertEquals(1, map.size());
        assertTrue(map.containsKey("foo"));
        assertTrue(map.containsValue(null));
      }
    }

    // Test putting a null key
    map = createMap();
    try
    {
      map.put(null, "foo");
    }
    catch (NullPointerException e)
    {
      if (supportsNullKeys())
        fail();
    }
    
    if (supportsNullKeys())
    {
      assertEquals(1, map.size());
      assertTrue(map.containsKey(null));
      assertTrue(map.containsValue("foo"));
    }
  }

  public void testEntrySet()
  {
    Map<String, Object> map = createMap();
    _putTwo(map);

    Set<Map.Entry<String, Object>> entries = map.entrySet();
    assertEquals(2, entries.size());
    _assertIteratorSize(entries.iterator(), 2);

    Iterator<Map.Entry<String, Object>> iterator = entries.iterator();
    while (iterator.hasNext())
    {
      Map.Entry<String, Object> entry = iterator.next();
      if (entry.getKey().equals("second"))
      {
        entry.setValue(THREE);
      }
      else if (entry.getKey().equals("first"))
      {
        try
        {
          iterator.remove();
        }
        catch (UnsupportedOperationException e)
        {
          if (supportsIteratorRemove())
            fail();
        }
      }
      else
      {
        fail();
      }
    }

    if (supportsIteratorRemove())
    {
      assertTrue(!map.containsKey("first"));
    }

    assertEquals(THREE, map.get("second"));

    map.clear();
    assertTrue(map.isEmpty());
    assertTrue(entries.isEmpty());

    _putTwo(map);

    _assertIteratorSize(entries.iterator(), 2);
    
    assertTrue(!entries.isEmpty());
    entries.clear();

    _assertIteratorSize(entries.iterator(), 0);
    assertTrue(map.isEmpty());
    assertTrue(entries.isEmpty());
  }

  public void testEquals()
  {
    Map<String, Object> map = createMap();
    _putTwo(map);
    assertEquals(map, map);

    Map<String, Object> secondMap = createMap();
    assertTrue(!secondMap.equals(map));
    assertTrue(!map.equals(secondMap));
    assertTrue(!map.equals(null));
    
    _putTwo(secondMap);
    assertEquals(map, secondMap);
    
    HashMap<String, Object> hashMap = new HashMap<String, Object>();
    _putTwo(hashMap);
    assertEquals(hashMap, map);
    assertEquals(map, hashMap);
  }

  public void testRemove()
  {
    Map<String, Object> map = createMap();
    _putTwo(map);
    assertNull(map.remove("NOTTHERE"));
    assertEquals(TWO, map.remove("second"));
    assertEquals(1, map.size());

    assertTrue(!map.containsKey("second"));
    assertNull(map.remove("second"));
    assertEquals(1, map.size());

    assertEquals(ONE, map.remove("first"));
    assertTrue(map.isEmpty());
    assertNull(map.remove("first"));
  }

  public void testKeySet()
  {
    Map<String, Object> map = createMap();
    _putTwo(map);
    Set<String> keys = map.keySet();
    assertEquals(2, keys.size());
    assertTrue(keys.contains("first"));
    assertTrue(keys.contains("second"));
    
    HashSet<String> hashSet = new HashSet<String>();
    hashSet.add("first");
    hashSet.add("second");
    
    assertEquals(keys, hashSet);
    assertEquals(hashSet, keys);

    hashSet.add("third");
    assertTrue(!keys.equals(hashSet));
    assertTrue(!hashSet.equals(keys));

    
    keys.clear();
    assertTrue(keys.isEmpty());
    assertTrue(map.isEmpty());
  }
  
  public void testValues()
  {
    Map<String, Object> map = createMap();
    _putTwo(map);
    Collection<Object> values = map.values();
    assertEquals(2, values.size());
    assertTrue(values.contains(ONE));
    assertTrue(values.contains(TWO));

    // Can't really assert that this values collection is equal to 
    // any other, because we can't rely on the order of the collection
    
    values.clear();
    assertTrue(values.isEmpty());
    assertTrue(map.isEmpty());
  }


  public void testPutAndGetWithClashingHashCode()
  {
    Map<LameKey, Object> cache = createMapWithLameKey();

    if (cache == null)
      return;

    cache.put(AAA, ONE);
    cache.put(ABB, TWO);
    cache.put(ACC, THREE);
    assertEquals(ONE, cache.get(AAA));
    assertEquals(TWO, cache.get(ABB));
    assertEquals(THREE, cache.get(ACC));

    cache = createMapWithLameKey();
    cache.put(AAA, ONE);
    cache.put(BAA, TWO);
    cache.put(BBB, THREE);
    assertEquals(ONE, cache.get(AAA));
    assertEquals(TWO, cache.get(BAA));
    assertEquals(THREE, cache.get(BBB));

    cache = createMapWithLameKey();
    cache.put(BAA, ONE);
    cache.put(AAA, TWO);
    cache.put(BBB, THREE);
    assertEquals(ONE, cache.get(BAA));
    assertEquals(TWO, cache.get(AAA));
    assertEquals(THREE, cache.get(BBB));
  }

  public void testRemoveWithClashingHashCode()
  {
    Map<LameKey, Object> cache = createMapWithLameKey();

    if (cache == null)
      return;

    cache.put(AAA, ONE);
    cache.put(ABB, TWO);
    cache.put(ACC, THREE);
    cache.remove(AAA);
    assertEquals(2, cache.size());
    assertContains(cache, ABB, ACC);

    cache = createMapWithLameKey();

    cache.put(AAA, ONE);
    cache.put(BAA, TWO);
    cache.put(BBB, THREE);
    cache.remove(BAA);
    assertEquals(2, cache.size());
    assertContains(cache, AAA, BBB);

    cache.put(BAA, ONE);
    cache.put(AAA, TWO);
    cache.put(BBB, THREE);
    cache.remove(BBB);
    assertEquals(2, cache.size());
    assertContains(cache, AAA, BAA);
  }

  protected void assertContains(Map<String, Object> cache, String... keys)
  {
    for (String key : keys)
      assertTrue("Object with key '" + key + "' expected to be available in the cache.",
                 cache.containsKey(key));
  }

  protected void assertContains(Map<LameKey, Object> cache, LameKey... keys)
  {
    for (LameKey key : keys)
      assertTrue("Object with key '" + key + "' expected to be available in the cache.",
                 cache.containsKey(key));
  }

  protected boolean isNullRemove()
  {
    return false;
  }

  protected boolean supportsNullValues()
  {
    return true;
  }

  protected boolean supportsNullKeys()
  {
    return true;
  }

  protected boolean supportsIteratorRemove()
  {
    return true;
  }

  /**
   * Key with lame, but stable hashing.  The key uses the first code point of the String
   * as its hashCode
   */
  protected static class LameKey
  {
    public LameKey(String key)
    {
      _key = key;
      _hashCode = key.codePointAt(0);
    }

    @Override
    public int hashCode()
    {
      return _hashCode;
    }

    @Override
    public boolean equals(Object o)
    {
      if (!(o instanceof LameKey))
        return false;

      return _key.equals(((LameKey)o)._key);
    }

    @Override
    public String toString()
    {
      StringBuilder sb = new StringBuilder();

      sb.append('(');
      sb.append(_key.charAt(0));
      sb.append(')');
      sb.append(_key.substring(1));

      return sb.toString();
    }

    private final String _key;
    private final int _hashCode;
  }

  private void _assertIteratorSize(Iterator<?> iterator, int count)
  {
    for (int i = 0; i < count; i++)
    {
      assertTrue(iterator.hasNext());
      iterator.next();
    }

    assertTrue(!iterator.hasNext());
  }

  private void _putTwo(Map<String, Object> map)
  {
    map.put("first", ONE);
    map.put("second", TWO);
  }

  /**
   * @return map with minimum size 3
   */
  abstract protected Map<String, Object> createMap();

  /**
   * @return map with minimum size 3, or
   *         null when not applicable
   */
  abstract protected Map<LameKey, Object> createMapWithLameKey();

  protected static final Object ONE   = new Integer(1);
  protected static final Object TWO   = new Integer(2);
  protected static final Object THREE = new Integer(3);
  protected static final Object FOUR  = new Integer(4);

  protected static final LameKey AAA = new LameKey("aaa");
  protected static final LameKey ABB = new LameKey("abb");
  protected static final LameKey ACC = new LameKey("acc");
  protected static final LameKey ADD = new LameKey("add");
  protected static final LameKey BAA = new LameKey("baa");
  protected static final LameKey BBB = new LameKey("bbb");
  protected static final LameKey BCC = new LameKey("bcc");
  protected static final LameKey BDD = new LameKey("bdd");
  protected static final LameKey CAA = new LameKey("caa");
  protected static final LameKey CBB = new LameKey("cbb");
  protected static final LameKey CCC = new LameKey("ccc");
  protected static final LameKey CDD = new LameKey("cdd");
  protected static final LameKey DAA = new LameKey("daa");
  protected static final LameKey DBB = new LameKey("dbb");
  protected static final LameKey DCC = new LameKey("dcc");
  protected static final LameKey DDD = new LameKey("ddd");
}
