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
package org.apache.myfaces.trinidad.component.html;

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;

/**
 * Unit tests for HtmlRowLayout.
 *
 * @author John Fallows
 */
public class HtmlRowLayoutTest extends UIComponentTestCase
{
  /**
   * Creates a new HtmlRowLayoutTest.
   *
   * @param testName  the unit test name
   */
  public HtmlRowLayoutTest(
    String testName)
  {
    super(testName);
  }
  
  @Override
  public void setUp()
  {
    super.setUp();
  }
  
  @Override
  public void tearDown()
  {
    super.tearDown();
  }
  
  public static Test suite()
  {
    return new TestSuite(HtmlRowLayoutTest.class);
  }

  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    HtmlRowLayout component = new HtmlRowLayout();
    assertEquals(true, component.isRendered());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    HtmlRowLayout component = new HtmlRowLayout();

    doTestAttributeTransparency(component, "halign",
                                "left", "right");
    doTestAttributeTransparency(component, "valign",
                                "top", "bottom");
    doTestAttributeTransparency(component, "width",
                                "50%", "100%");
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

  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    HtmlRowLayout component = new HtmlRowLayout();
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    HtmlRowLayout component = new HtmlRowLayout();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    HtmlRowLayout component = new HtmlRowLayout();
    doTestUpdateModelValues(component);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    HtmlRowLayout component = new HtmlRowLayout();
    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    HtmlRowLayout component = new HtmlRowLayout();
    doTestRenderResponse(component);
  }
}
