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
