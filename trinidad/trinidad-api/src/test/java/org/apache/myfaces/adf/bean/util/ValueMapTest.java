/*
 * Copyright 2000, 2005-2006 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.adf.bean.util;

import java.util.Map;

import org.apache.myfaces.adf.bean.TestBean;

/**
 * Test of ValueMap.
 */
public class ValueMapTest extends org.apache.myfaces.adfbuild.test.MapTestCase
{
  public ValueMapTest(String testName)
  {
    super(testName);
  }

  protected boolean supportsNullKeys()
  {
    return false;
  }

  protected boolean supportsIteratorRemove()
  {
    return true;
  }

  protected boolean isNullRemove()
  {
    return true;
  }

  protected Map createMap()
  {
    return new ValueMap(new TestBean());
  }
}
