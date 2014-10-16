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

import java.util.Map;
import java.util.concurrent.ConcurrentMap;

public abstract class ConcurrentMapTestCase extends MapTestCase
{
  public ConcurrentMapTestCase(String testName)
  {
    super(testName);
  }

  public void testRemoveKeyValue()
  {
    ConcurrentMap<String, Object> cache = createMap();
    _putTwo(cache);
    boolean bool = cache.remove(A_STR, "aStr");
    assertFalse(bool);
    assertEquals("Remove operation did not work as expected.", 2, cache.size());
    assertNotNull("Entry with key 'aaa' expected to be available.",
                  cache.get(A_STR));
    bool = cache.remove(A_STR, ONE);
    assertTrue(bool);
    assertEquals("Remove operation did not work as expected.", 1, cache.size());
    assertNull("Entry with key 'aaa' expected to be removed.",
               cache.get(A_STR));
  }

  public void testReplace()
  {
    ConcurrentMap<String, Object> cache = createMap();
    _putTwo(cache);
    Object val = cache.replace(C_STR, ONE);
    assertNull(val);
    assertEquals("Replace operation did not work as expected.", 2, cache.size());
    assertEquals(ONE, cache.get(A_STR));
    assertEquals(TWO, cache.get(B_STR));

    val = cache.replace(A_STR, "aaaString");
    assertEquals(ONE, val);
    assertEquals("Replace operation did not work as expected.", 2, cache.size());
    assertEquals("aaaString", cache.get(A_STR));

    boolean bool = cache.replace(B_STR, "bb", "newValue");
    assertFalse(bool);
    assertEquals("Replace operation did not work as expected.", 2, cache.size());
    assertEquals(TWO, cache.get(B_STR));

    bool = cache.replace(B_STR, TWO, "newValue");
    assertTrue(bool);
    assertEquals("Replace operation did not work as expected.", 2, cache.size());
    assertEquals("newValue", cache.get(B_STR));
  }

  public void testPutIfAbsent()
  {
    ConcurrentMap<String, Object> cache = createMap();
    Object val = cache.putIfAbsent(A_STR, ONE);
    assertEquals("putIfAbsent operation did not work as expected.", 1, cache.size());
    assertNull(val);
    val = cache.putIfAbsent(A_STR, "newVal");
    assertEquals(ONE, val);
    val = cache.putIfAbsent(B_STR, TWO);
    assertNull(val);
    assertEquals("putIfAbsent operation did not work as expected.", 2, cache.size());
  }

  public void testPutIfAbsentWithClashingHashCode()
  {
    ConcurrentMap<LameKey, Object> cache = createMapWithLameKey();

    if (cache == null)
      return;

    cache.putIfAbsent(AAA, ONE);
    cache.putIfAbsent(ABB, TWO);
    cache.putIfAbsent(AAA, FOUR);
    cache.putIfAbsent(ACC, THREE);
    cache.putIfAbsent(ABB, THREE);
    assertEquals(3, cache.size());
    assertEquals(ONE, cache.get(AAA));
    assertEquals(TWO, cache.get(ABB));
    assertEquals(THREE, cache.get(ACC));

    cache = createMapWithLameKey();
    cache.putIfAbsent(AAA, ONE);
    cache.putIfAbsent(BAA, TWO);
    cache.putIfAbsent(BAA, FOUR);
    cache.putIfAbsent(BBB, THREE);
    cache.putIfAbsent(BBB, ONE);
    assertEquals(3, cache.size());
    assertEquals(ONE, cache.get(AAA));
    assertEquals(TWO, cache.get(BAA));
    assertEquals(THREE, cache.get(BBB));

    cache = createMapWithLameKey();
    cache.putIfAbsent(BAA, ONE);
    cache.putIfAbsent(BAA, TWO);
    cache.putIfAbsent(AAA, TWO);
    cache.putIfAbsent(AAA, FOUR);
    cache.putIfAbsent(BBB, THREE);
    assertEquals(3, cache.size());
    assertEquals(ONE, cache.get(BAA));
    assertEquals(TWO, cache.get(AAA));
    assertEquals(THREE, cache.get(BBB));
  }

  abstract protected ConcurrentMap<String, Object> createMap();
  abstract protected ConcurrentMap<LameKey, Object> createMapWithLameKey();

  private void _putTwo(ConcurrentMap<String, Object> map)
  {
    map.put(A_STR, ONE);
    map.put(B_STR, TWO);
  }

  protected static final String A_STR = "aaa";
  protected static final String B_STR = "bbb";
  protected static final String C_STR = "ccc";
  protected static final String D_STR = "ddd";
}
