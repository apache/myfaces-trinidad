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
package org.apache.myfaces.trinidad.component.core.layout;

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.component.UIComponentTestCase;

/**
 * Unit tests for CoreShowOneAccordion
 *
 * @author Nidhi Shrotriya
 */

public class CoreShowOneAccordionTest extends UIComponentTestCase
{
  /**
   * Creates a new CoreShowOneAccordion.
   *
   * @param testName  the unit test name
   */
  public CoreShowOneAccordionTest(String testName)
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
    return new TestSuite(CoreShowOneAccordionTest.class);
  }

  /**
   * Tests the Initial values for the component attributes.
   */
  public void testInitialAttributeValues()
  {
    CoreShowOneAccordion component = new CoreShowOneAccordion();
    assertEquals(true, component.isRendered());
  }

  /**
   * Tests the values set for the component attributes.
   */
  public void testgetAttributeValues()
  {
    CoreShowOneAccordion component = new CoreShowOneAccordion();
    assertEquals("org.apache.myfaces.adf.ShowOne",component.getFamily());
  }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   *
   */
  public void testAttributeTransparency()
  {
    CoreShowOneAccordion component= new CoreShowOneAccordion();
    doTestAttributeTransparency(component,
                                "rendered",
                                Boolean.TRUE, Boolean.FALSE);
  }

  /**
   * Tests the Apply Request Values Phase
   */
  public void testApplyRequestValues()
  {
    CoreShowOneAccordion component= new CoreShowOneAccordion();
    doTestApplyRequestValues(component);
    component= new CoreShowOneAccordion();
    component.setRendered(false);
    doTestApplyRequestValues(component);
    component= new CoreShowOneAccordion();
  }

  /**
   * Tests the process-validations lifecycle phase.
   */
  public void testProcessValidations()
  {
    CoreShowOneAccordion component= new CoreShowOneAccordion();
    doTestProcessValidations(component);
  }

  /**
   * Tests the update-model-values lifecycle phase.
   */
  public void testUpdateModelValues()
  {
    CoreShowOneAccordion component= new CoreShowOneAccordion();
    doTestUpdateModelValues(component);
  }

  /**
   * Tests the invoke-application lifecycle phase.
   */
  public void testInvokeApplication()
  {
    CoreShowOneAccordion component= new CoreShowOneAccordion();
    doTestInvokeApplication(component, null);
  }
  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
  public void testRenderResponse() throws IOException
  {
    CoreShowOneAccordion component= new CoreShowOneAccordion();
    doTestRenderResponse(component);
  }
}

