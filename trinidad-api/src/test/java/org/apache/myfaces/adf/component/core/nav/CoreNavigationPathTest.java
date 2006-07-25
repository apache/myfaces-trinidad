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

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.adf.component.UIComponentTestCase;

/**
 * Unit tests for CoreNavigationPath.
 *
 * @author Gabrielle Crawford
 */
public class CoreNavigationPathTest extends UIComponentTestCase
{
  /**
   * Creates a new CoreNavigationPathTest.
   *
   * @param testName  the unit test name
   */
  public CoreNavigationPathTest(
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
    return new TestSuite(CoreNavigationPathTest.class);
  }
  
  /**
   * Tests the initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CoreNavigationPath component = new CoreNavigationPath();
    assertEquals(true, component.isRendered());
    assertEquals("horizontal", component.getOrientation());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
  public void testAttributeTransparency()
  {
    CoreNavigationPath component = new CoreNavigationPath();

    doTestAttributeTransparency(component, "rendered",
                                Boolean.TRUE, Boolean.FALSE);
    doTestAttributeTransparency(component, "orientation",
                                "horizontal",
                                "vertical");
  }


  /**
   * Tests the apply-request-values lifecycle phase.
   */
  public void testApplyRequestValues()
  {
    CoreNavigationPath component = new CoreNavigationPath();
    doTestApplyRequestValues(component);

    component = new CoreNavigationPath();
    component.setRendered(false);
    doTestApplyRequestValues(component);
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    CoreNavigationPath component = new CoreNavigationPath();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    CoreNavigationPath component = new CoreNavigationPath();
    doTestUpdateModelValues(component);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    CoreNavigationPath component = new CoreNavigationPath();
    doTestInvokeApplication(component, null);
  }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    CoreNavigationPath component = new CoreNavigationPath();
    doTestRenderResponse(component);
  }
}
