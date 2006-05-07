/*
 * Copyright 2004,2006 The Apache Software Foundation.
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
package org.apache.myfaces.adf.component.core.nav;

import junit.textui.TestRunner;

import org.apache.myfaces.adf.component.UIComponentTestCase;
import org.apache.myfaces.adf.component.core.nav.CoreProcessTrain;

/**
 * Unit tests for CoreProcessTrain
 *
 * @author Jeanne Waldman
 */
public class CoreProcessTrainTest extends UIComponentTestCase
{
  /**
   * Creates a new CoreProcessTrainTest.
   *
   * @param testName  the unit test name
   */
  public CoreProcessTrainTest(
    String testName)
  {
    super(testName);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CoreProcessTrain component = new CoreProcessTrain();
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    CoreProcessTrain component = new CoreProcessTrain();

  }



  public static void main(String[] args)
  {
    TestRunner.run(CoreProcessTrainTest.class);

  }
}
