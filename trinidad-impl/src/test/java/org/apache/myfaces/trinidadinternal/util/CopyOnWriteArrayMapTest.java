package org.apache.myfaces.trinidadinternal.util;

import org.apache.myfaces.trinidadbuild.test.ConcurrentMapTestCase;

import java.util.concurrent.ConcurrentMap;

public class CopyOnWriteArrayMapTest extends ConcurrentMapTestCase
{
  public CopyOnWriteArrayMapTest(String testName)
  {
    super(testName);
  }

  protected boolean supportsNullKeys()
  {
    return false;
  }

  protected ConcurrentMap<String, Object> createMap()
  {
    return CopyOnWriteArrayMap.newConcurrentMap();
  }

  protected ConcurrentMap<LameKey, Object> createMapWithLameKey()
  {
    return CopyOnWriteArrayMap.newConcurrentMap();
  }
}
