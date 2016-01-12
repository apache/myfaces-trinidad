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
package org.apache.myfaces.trinidadinternal.util;

import java.util.concurrent.ConcurrentMap;

public class LRUCopyOnWriteArrayMapTest extends CopyOnWriteArrayMapTest

{
  public LRUCopyOnWriteArrayMapTest(String testName)
  {
    super(testName);
  }

  public void testEvictionAtTailInsertAtTail()
  {
    ConcurrentMap<String, Object> cache = createMap();
    _putThree(cache, A_STR, B_STR, C_STR);

    cache.get(A_STR);
    cache.get(B_STR);

    cache.put(D_STR, FOUR);

    assertOldestEvicted(cache, C_STR);
    assertContains(cache, A_STR, B_STR, D_STR);
  }

  public void testEvictionAtMiddleInsertAtTail()
  {
    ConcurrentMap<String, Object> cache = createMap();
    _putThree(cache, A_STR, B_STR, C_STR);

    cache.get(A_STR);
    cache.get(C_STR);

    cache.put(D_STR, FOUR);
    assertOldestEvicted(cache, B_STR);
    assertContains(cache, A_STR, C_STR, D_STR);
  }

  public void testEvictionAtHeadInsertAtTail()
  {
    ConcurrentMap<String, Object> cache = createMap();
    _putThree(cache, A_STR, B_STR, C_STR);

    cache.put(D_STR, FOUR);

    assertOldestEvicted(cache, A_STR);
    assertContains(cache, B_STR, C_STR, D_STR);
  }

  public void testEvictionAtTailInsertAtHead()
  {
    ConcurrentMap<String, Object> cache = createMap();
    _putThree(cache, B_STR, C_STR, D_STR);

    cache.get(B_STR);
    cache.get(C_STR);

    cache.put(A_STR, FOUR);

    assertOldestEvicted(cache, D_STR);
    assertContains(cache, A_STR, B_STR, C_STR);
  }

  public void testEvictionAtMiddleInsertAtHead()
  {
    ConcurrentMap<String, Object> cache = createMap();
    _putThree(cache, B_STR, C_STR, D_STR);

    cache.get(B_STR);
    cache.get(D_STR);

    cache.put(A_STR, ONE);

    assertOldestEvicted(cache, C_STR);
    assertContains(cache, A_STR, B_STR, D_STR);
  }

  public void testEvictionAtHeadInsertAtHead()
  {
    ConcurrentMap<String, Object> cache = createMap();
    _putThree(cache, B_STR, C_STR, D_STR);

    cache.put(A_STR, ONE);

    assertOldestEvicted(cache, B_STR);
    assertContains(cache, A_STR, C_STR, D_STR);
  }

  public void testEvictionAtTailInsertAtMiddle()
  {
    ConcurrentMap<String, Object> cache = createMap();
    _putThree(cache, A_STR, C_STR, D_STR);

    cache.get(A_STR);
    cache.get(C_STR);

    cache.put(B_STR, TWO);

    assertOldestEvicted(cache, D_STR);
    assertContains(cache, A_STR, C_STR, B_STR);
  }

  public void testEvictionAtMiddleInsertAtMiddle()
  {
    ConcurrentMap<String, Object> cache = createMap();
    _putThree(cache, A_STR, C_STR, D_STR);

    cache.get(A_STR);
    cache.get(D_STR);

    cache.put(B_STR, TWO);


    assertOldestEvicted(cache, C_STR);
    assertContains(cache, A_STR, D_STR, B_STR);
  }

  public void testEvictionAtHeadInsertAtMiddle()
  {
    ConcurrentMap<String, Object> cache = createMap();
    _putThree(cache, A_STR, B_STR, D_STR);

    cache.put(C_STR, THREE);

    assertOldestEvicted(cache, A_STR);
    assertContains(cache, C_STR, D_STR, B_STR);
  }

  public void testEvictionAtTailInsertAtTailWithClashingHashCodeKeys()
  {
    ConcurrentMap<LameKey, Object> cache = createMapWithLameKey();
    _putThree(cache, AAA, ABB, ACC);
    cache.get(AAA);
    cache.get(ABB);
    cache.put(CCC, FOUR);
    assertOldestEvicted(cache, ACC);
    assertContains(cache, AAA, ABB, CCC);

    cache = createMapWithLameKey();
    _putThree(cache, AAA, BAA, BBB);
    cache.get(AAA);
    cache.get(BAA);
    cache.put(CCC, FOUR);
    assertOldestEvicted(cache, BBB);
    assertContains(cache, AAA, BAA, CCC);

    cache = createMapWithLameKey();
    _putThree(cache, AAA, ABB, BBB);
    cache.get(AAA);
    cache.get(ABB);
    cache.put(CCC, FOUR);
    assertOldestEvicted(cache, BBB);
    assertContains(cache, AAA, ABB, CCC);
  }

  public void testEvictionAtMiddleInsertAtTailWithClashingHashCodeKeys()
  {
    ConcurrentMap<LameKey, Object> cache = createMapWithLameKey();
    _putThree(cache, AAA, ABB, ACC);
    cache.get(AAA);
    cache.get(ACC);
    cache.put(CCC, FOUR);
    assertOldestEvicted(cache, ABB);
    assertContains(cache, AAA, ACC, CCC);

    cache = createMapWithLameKey();
    _putThree(cache, AAA, BAA, BBB);
    cache.get(AAA);
    cache.get(BBB);
    cache.put(CCC, FOUR);
    assertOldestEvicted(cache, BAA);
    assertContains(cache, AAA, BBB, CCC);

    cache = createMapWithLameKey();
    _putThree(cache, AAA, ABB, BBB);
    cache.get(AAA);
    cache.get(BBB);
    cache.put(CCC, FOUR);
    assertOldestEvicted(cache, ABB);
    assertContains(cache, AAA, BBB, CCC);
  }

  public void testEvictionAtHeadInsertAtTailWithClashingHashCodeKeys()
  {
    ConcurrentMap<LameKey, Object> cache = createMapWithLameKey();
    _putThree(cache, AAA, ABB, ACC);
    cache.put(ADD, FOUR);
    assertOldestEvicted(cache, AAA);
    assertContains(cache, ABB, ACC, ADD);

    cache = createMapWithLameKey();
    _putThree(cache, AAA, BAA, BBB);
    cache.put(CCC, FOUR);
    assertOldestEvicted(cache, AAA);
    assertContains(cache, BAA, BBB, CCC);

    cache = createMapWithLameKey();
    _putThree(cache, AAA, ABB, BBB);
    cache.put(CCC, FOUR);
    assertOldestEvicted(cache, AAA);
    assertContains(cache, ABB, BBB, CCC);
  }

  public void testEvictionAtTailInsertAtHeadWithClashingHashCodeKeys()
  {
    ConcurrentMap<LameKey, Object> cache = createMapWithLameKey();
    _putThree(cache, CAA, CBB, CCC);
    cache.get(CAA);
    cache.get(CBB);
    cache.put(AAA, FOUR);
    assertOldestEvicted(cache, CCC);
    assertContains(cache, CAA, CBB, AAA);

    cache = createMapWithLameKey();
    _putThree(cache, CAA, DAA, DBB);
    cache.get(CAA);
    cache.get(DAA);
    cache.put(AAA, FOUR);
    assertOldestEvicted(cache, DBB);
    assertContains(cache, CAA, DAA, AAA);

    cache = createMapWithLameKey();
    _putThree(cache, CAA, CBB, DAA);
    cache.get(CAA);
    cache.get(CBB);
    cache.put(AAA, FOUR);
    assertOldestEvicted(cache, DAA);
    assertContains(cache, CAA, CBB, AAA);
  }

  public void testEvictionAtMiddleInsertAtHeadWithClashingHashCodeKeys()
  {
    ConcurrentMap<LameKey, Object> cache = createMapWithLameKey();
    _putThree(cache, CAA, CBB, CCC);
    cache.get(CAA);
    cache.get(CCC);
    cache.put(AAA, FOUR);
    assertOldestEvicted(cache, CBB);
    assertContains(cache, CAA, CCC, AAA);

    cache = createMapWithLameKey();
    _putThree(cache, CAA, DAA, DBB);
    cache.get(CAA);
    cache.get(DBB);
    cache.put(AAA, FOUR);
    assertOldestEvicted(cache, DAA);
    assertContains(cache, CAA, DBB, AAA);

    cache = createMapWithLameKey();
    _putThree(cache, CAA, CBB, DAA);
    cache.get(CAA);
    cache.get(DAA);
    cache.put(AAA, FOUR);
    assertOldestEvicted(cache, CBB);
    assertContains(cache, CAA, DAA, AAA);
  }

  public void testEvictionAtHeadInsertAtHeadWithClashingHashCodeKeys()
  {
    ConcurrentMap<LameKey, Object> cache = createMapWithLameKey();
    _putThree(cache, CAA, CBB, CCC);
    cache.put(AAA, FOUR);
    assertOldestEvicted(cache, CAA);
    assertContains(cache, CBB, CCC, AAA);

    cache = createMapWithLameKey();
    _putThree(cache, CAA, DAA, DBB);
    cache.put(AAA, FOUR);
    assertOldestEvicted(cache, CAA);
    assertContains(cache, DAA, DBB, AAA);

    cache = createMapWithLameKey();
    _putThree(cache, CAA, CBB, DAA);
    cache.put(AAA, FOUR);
    assertOldestEvicted(cache, CAA);
    assertContains(cache, CBB, DAA, AAA);
  }

  public void testEvictionAtTailInsertAtMiddleWithClashingHashCodeKeys()
  {
    ConcurrentMap<LameKey, Object> cache = createMapWithLameKey();
    _putThree(cache, BAA, BCC, BDD);
    cache.get(BAA);
    cache.get(BCC);
    cache.put(BBB, FOUR);
    assertOldestEvicted(cache, BDD);
    assertContains(cache, BAA, BBB, BCC);

    cache = createMapWithLameKey();
    _putThree(cache, BAA, CAA, CBB);
    cache.get(BAA);
    cache.get(CAA);
    cache.put(BBB, FOUR);
    assertOldestEvicted(cache, CBB);
    assertContains(cache, BAA, BBB, CAA);

    cache = createMapWithLameKey();
    _putThree(cache, CAA, CDD, DDD);
    cache.get(CAA);
    cache.get(CDD);
    cache.put(CCC, FOUR);
    assertOldestEvicted(cache, DDD);
    assertContains(cache, CAA, CCC, CDD);
  }

  public void testEvictionAtMiddleInsertAtMiddleWithClashingHashCodeKeys()
  {
    ConcurrentMap<LameKey, Object> cache = createMapWithLameKey();
    _putThree(cache, BAA, BCC, BDD);
    cache.get(BAA);
    cache.get(BDD);
    cache.put(BBB, FOUR);
    assertOldestEvicted(cache, BCC);
    assertContains(cache, BAA, BBB, BDD);

    cache = createMapWithLameKey();
    _putThree(cache, BAA, CCC, CDD);
    cache.get(BAA);
    cache.get(CDD);
    cache.put(CAA, FOUR);
    assertOldestEvicted(cache, CCC);
    assertContains(cache, BAA, CAA, CDD);

    cache = createMapWithLameKey();
    _putThree(cache, CAA, CDD, DDD);
    cache.get(CAA);
    cache.get(DDD);
    cache.put(CCC, FOUR);
    assertOldestEvicted(cache, CDD);
    assertContains(cache, CAA, CCC, DDD);
  }

  public void testEvictionAtHeadInsertAtMiddleWithClashingHashCodeKeys()
  {
    ConcurrentMap<LameKey, Object> cache = createMapWithLameKey();
    _putThree(cache, DAA, DBB, DDD);
    cache.put(DCC, FOUR);
    assertOldestEvicted(cache, DAA);
    assertContains(cache, DBB, DCC, DDD);

    cache = createMapWithLameKey();
    _putThree(cache, CAA, DBB, DDD);
    cache.put(DCC, FOUR);
    assertOldestEvicted(cache, CAA);
    assertContains(cache, DBB, DCC, DDD);

    cache = createMapWithLameKey();
    _putThree(cache, CAA, CBB, DDD);
    cache.put(DCC, FOUR);
    assertOldestEvicted(cache, CAA);
    assertContains(cache, CBB, DCC, DDD);
  }

  private void assertOldestEvicted(ConcurrentMap<?, Object> cache, Object o)
  {
    assertNull(String.format("Oldest object with key '%s' expected to be evicted.", o), cache.get(o));
  }

  private void _putThree(ConcurrentMap cache, Object one, Object two, Object three)
  {
    cache.put(one, ONE);
    cache.put(two, TWO);
    cache.put(three, THREE);
  }

  protected ConcurrentMap<String, Object> createMap()
  {
    return CopyOnWriteArrayMap.newLRUConcurrentMap(3);
  }

  protected ConcurrentMap<LameKey, Object> createMapWithLameKey()
  {
    return CopyOnWriteArrayMap.newLRUConcurrentMap(3);
  }

  protected boolean supportsNullKeys()
  {
    return false;
  }
}
