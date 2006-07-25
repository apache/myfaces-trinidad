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
package org.apache.myfaces.adf.component.core.layout;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.adf.component.UIXPanel;

/**
 * Unit tests for CorePanelSideBar.
 *
 * @author Gabrielle Crawford
 */
public class CorePanelSideBarTest extends UIXPanelTestCase
{
  /**
   * Creates a new CorePanelSideBarTest.
   *
   * @param testName  the unit test name
   */
  public CorePanelSideBarTest(
    String testName)
  {
    super(testName);
  }
  
  public void setUp()
  {
    super.setUp();
  }
  
  public void tearDown()
  {
    super.tearDown();
  }
  
  public static Test suite()
  {
    return new TestSuite(CorePanelSideBarTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CorePanelSideBar component = new CorePanelSideBar();
    assertEquals(true, component.isRendered());
    assertNull(component.getWidth());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    CorePanelSideBar component = new CorePanelSideBar();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "width",
                                "10%",
                                "20%");
  }

  protected UIXPanel createTestComponent()
  {
      return new CorePanelSideBar();
  }
}
