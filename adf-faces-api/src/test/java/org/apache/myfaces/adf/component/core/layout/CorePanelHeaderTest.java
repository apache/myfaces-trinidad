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

import org.apache.myfaces.adf.component.UIXPanel;

/**
 * Unit tests for CorePanelHeader.
 *
 * @author Gabrielle Crawford
 */
public class CorePanelHeaderTest extends UIXPanelTestCase
{
  /**
   * Creates a new CorePanelHeaderTest.
   *
   * @param testName  the unit test name
   */
  public CorePanelHeaderTest(
    String testName)
  {
    super(testName);
  }

  /**
   * Tests the initial values for the component attributes.
   * @todo deal with size attr
   */
  public void testInitialAttributeValues()
  {
    CorePanelHeader component = new CorePanelHeader();

    assertTrue(component.isRendered());
    assertNull(component.getIcon());
    assertNull(component.getText());
    assertEquals("none", component.getMessageType());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   * @todo deal with size attr
   */
  public void testAttributeTransparency()
  {
    CorePanelHeader component = new CorePanelHeader();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "messageType",
                                "error", "warning");
    doTestAttributeTransparency(component, "text",
                                "foo", "bar");
    doTestAttributeTransparency(component, "icon",
                                "foo", "bar");
  }

  protected UIXPanel createTestComponent()
  {
    return new CorePanelHeader();
  }
}
