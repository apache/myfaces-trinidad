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
 * Unit tests for CorePanelBox.
 *
 * @author Gabrielle Crawford
 */
public class CorePanelBoxTest extends UIXPanelTestCase
{
  /**
   * Creates a new CorePanelBoxTest.
   *
   * @param testName  the unit test name
   */
  public CorePanelBoxTest(
    String testName)
  {
    super(testName);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CorePanelBox component = new CorePanelBox();

    assertTrue(component.isRendered());
    assertEquals("light", component.getBackground());
    assertNull(component.getIcon());
    assertNull(component.getText());
    assertNull(component.getContentStyle());
    //assertNull(component.getWidth());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    CorePanelBox component = new CorePanelBox();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "background",
                                "light", "medium");
    doTestAttributeTransparency(component, "text",
                                "foo", "bar");
    doTestAttributeTransparency(component, "icon",
                                "foo", "bar");
    doTestAttributeTransparency(component, "contentStyle",
		                        "foo", "bar");
    //doTestAttributeTransparency(component, "width", "10%", "20%");
  }

  protected UIXPanel createTestComponent()
  {
    return new CorePanelBox();
  }

  public static void main(String[] args)
  {
    TestRunner.run(CorePanelBoxTest.class);
  }

}
