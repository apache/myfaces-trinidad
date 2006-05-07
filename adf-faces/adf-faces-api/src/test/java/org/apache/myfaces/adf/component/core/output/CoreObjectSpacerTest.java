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
package org.apache.myfaces.adf.component.core.output;

import org.apache.myfaces.adf.component.UIXObject;

/**
 * Unit tests for CoreObjectSpacer.
 *
 * @author John Fallows
 */
public class CoreObjectSpacerTest extends UIXObjectTestCase
{
  /**
   * Creates a new CoreObjectSpacerTest.
   *
   * @param testName  the unit test name
   */
  public CoreObjectSpacerTest(
    String testName)
  {
    super(testName);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CoreObjectSpacer component = new CoreObjectSpacer();
    assertEquals(true, component.isRendered());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    CoreObjectSpacer component = new CoreObjectSpacer();
    doTestAttributeTransparency(component, "width",
                                "50%", "100%");
    doTestAttributeTransparency(component, "height",
                                "25%", "75%");
  }

  /**
   * Tests the transparency of the component facets by comparing
   * bean accessor and mutator methods with facet map accessor
   * and mutator methods.
   */
  public void testFacetTransparency()
  {
    // no facets yet
  }

  protected UIXObject createTestComponent()
  {
    return new CoreObjectSpacer();
  }
}
