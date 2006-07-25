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
package org.apache.myfaces.adf.component.core.layout;

import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.adf.component.UIComponentTestCase;

/**
 * Unit tests for CoreShowOneRadio
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/test/java/oracle/adf/view/faces/component/core/layout/CoreShowOneRadioTest.java#1 $) $Date: 16-aug-2005.15:12:21 $
 * @author Nidhi Shrotriya
 */

public class CoreShowOneRadioTest extends UIComponentTestCase
{
  /**
   * Creates a new CoreShowOneRadio.
   *
   * @param testName  the unit test name
   */
   public CoreShowOneRadioTest(String testName)
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
     return new TestSuite(CoreShowOneRadioTest.class);
   }

  /**
   * Tests the Initial values for the component attributes.
   */
   public void testInitialAttributeValues()
   {
     CoreShowOneRadio component = new CoreShowOneRadio();
     assertEquals(true, component.isRendered());
     assertNull(component.getLabel());
     assertEquals("start",component.getPosition());
     assertEquals("center", component.getAlignment());
   }

  /**
   * Tests the values set for the component attributes.
   */
   public void testgetAttributeValues()
   {
     CoreShowOneRadio component = new CoreShowOneRadio();
     assertEquals("org.apache.myfaces.adf.ShowOne",component.getFamily());
   }

  /**
   * Tests the transparency of the component attribute by comparing
   * bean accessor and mutator methods with attribute map accessor
   * and mutator methods.
   */
   public void testAttributeTransparency()
   {
     CoreShowOneRadio component= new CoreShowOneRadio();
     doTestAttributeTransparency(component, "rendered",
                                    Boolean.TRUE, Boolean.FALSE);
     doTestAttributeTransparency(component, "label", "foo", "bar");
     doTestAttributeTransparency(component, "position", "start", "top");
     doTestAttributeTransparency(component, "alignment", "center", "bottom");
   }

  /**
   * Tests the Apply Request Values Phase
   */
   public void testApplyRequestValues()
   {
     CoreShowOneRadio component= new CoreShowOneRadio();
     doTestApplyRequestValues(component);
     component= new CoreShowOneRadio();
     component.setRendered(false);
     doTestApplyRequestValues(component);
   }

  /**
   * Tests the process-validations lifecycle phase.
   */
   public void testProcessValidations()
   {
     CoreShowOneRadio component= new CoreShowOneRadio();
     doTestProcessValidations(component);
   }

  /**
   * Tests the update-model-values lifecycle phase.
   */
   public void testUpdateModelValues()
   {
     CoreShowOneRadio component= new CoreShowOneRadio();
     doTestUpdateModelValues(component);
   }

  /**
   * Tests the invoke-application lifecycle phase.
   */
   public void testInvokeApplication()
   {
     CoreShowOneRadio component= new CoreShowOneRadio();
     doTestInvokeApplication(component, null);
   }

  /**
   * Tests the render-response lifecycle phase.
   *
   * @throws IOException  when test fails
   */
   public void testRenderResponse() throws IOException
   {
     CoreShowOneRadio component= new CoreShowOneRadio();
     doTestRenderResponse(component);
   }

  }

