/*
 * Copyright 2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfbuild.test;
import java.util.HashMap;

import javax.faces.component.MockUIComponent;

/**
 * Utilitiy methods for junit tests of ADF Faces
 */
public class MockUtils
{
  public static MockUIComponent buildMockUIComponent()
  {
    return buildMockUIComponent(1);
  }

  public static MockUIComponent buildMockUIComponent(
    int iterations
    )
  {
    return buildMockUIComponent(iterations, new String[] {"label"});
  }

  /**
   * Builds a MockUIComponent with attributes setup for the requested number of
   * test iterations.
   */
  public static MockUIComponent buildMockUIComponent(
    int iterations,
    String attributeNames[]
     )
  {
    int i;
    MockUIComponent c = new MockUIComponent();
    HashMap attrs = new HashMap();
    for (i = 0; i < attributeNames.length; i++)
      attrs.put(attributeNames[i], attributeNames[i]);
    for (i = 0; i < iterations; i++)
      c.setupGetAttributes(attrs);
    return c;
  }

}
