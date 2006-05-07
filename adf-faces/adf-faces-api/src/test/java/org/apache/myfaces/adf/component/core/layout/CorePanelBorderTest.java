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

import junit.textui.TestRunner;

import org.apache.myfaces.adf.component.UIXPanel;

/**
 * Unit tests for CorePanelBorder.
 *
 * @author John Fallows
 */
public class CorePanelBorderTest extends UIXPanelTestCase
{
  /**
   * Creates a new CorePanelBorderTest.
   *
   * @param testName  the unit test name
   */
  public CorePanelBorderTest(
    String testName)
  {
    super(testName);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CorePanelBorder component = new CorePanelBorder();
    assertEquals(true, component.isRendered());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    CorePanelBorder component = new CorePanelBorder();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);
  }

  /**
   * Tests the transparency of the component facets by comparing
   * bean accessor and mutator methods with facet map accessor
   * and mutator methods.
   */
  public void testFacetTransparency()
  {
    CorePanelBorder component = new CorePanelBorder();

    doTestFacetTransparency(component, CorePanelBorder.TOP_FACET);
    doTestFacetTransparency(component, CorePanelBorder.BOTTOM_FACET);
    doTestFacetTransparency(component, CorePanelBorder.LEFT_FACET);
    doTestFacetTransparency(component, CorePanelBorder.RIGHT_FACET);
    doTestFacetTransparency(component, CorePanelBorder.START_FACET);
    doTestFacetTransparency(component, CorePanelBorder.END_FACET);
  }
  
  protected UIXPanel createTestComponent()
  {
    return new CorePanelBorder();
  }
  
  public static void main(String[] args)
  {
    TestRunner.run(CorePanelBorderTest.class);
  }
}
