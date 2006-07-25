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
 * Unit tests for CorePanelGroup.
 *
 * @author John Fallows
 */
public class CorePanelGroupTest extends UIXPanelTestCase
{
  /**
   * Creates a new CorePanelGroupTest.
   *
   * @param testName  the unit test name
   */
  public CorePanelGroupTest(
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
    return new TestSuite(CorePanelGroupTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CorePanelGroup component = new CorePanelGroup();
    assertEquals(true, component.isRendered());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    // no attributes yet
  }

  /**
   * Tests the transparency of the component facets by comparing
   * bean accessor and mutator methods with facet map accessor
   * and mutator methods.
   */
  public void testFacetTransparency()
  {
    CorePanelGroup component = new CorePanelGroup();
    doTestFacetTransparency(component, CorePanelGroup.SEPARATOR_FACET);
  }

  protected UIXPanel createTestComponent()
  {
    return new CorePanelGroup();
  }
}
