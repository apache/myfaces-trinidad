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
package org.apache.myfaces.trinidad.bean.util;

import java.util.Map;

import org.apache.myfaces.trinidad.bean.TestBean;
import org.apache.myfaces.trinidad.bean.util.ValueMap;

/**
 * Test of ValueMap.
 */
public class ValueMapTest extends org.apache.myfaces.trinidadbuild.test.MapTestCase
{
  public ValueMapTest(String testName)
  {
    super(testName);
  }

  @Override
  protected boolean supportsNullKeys()
  {
    return false;
  }

  @Override
  protected boolean supportsIteratorRemove()
  {
    return true;
  }

  @Override
  protected boolean isNullRemove()
  {
    return true;
  }

  @Override
  protected Map<String, Object> createMap()
  {
    return new ValueMap(new TestBean());
  }

  @Override
  protected Map<LameKey, Object> createMapWithLameKey()
  {
    return null;
  }

}
