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

package org.apache.myfaces.adf.validator;
import java.util.HashMap;
import java.util.Locale;

import java.util.Map;
import javax.faces.component.StateHolder;
import javax.faces.component.UIViewRoot;
import javax.faces.validator.Validator;
import javax.faces.validator.ValidatorException;

import junit.framework.TestCase;

import javax.faces.component.MockUIComponent;
import javax.faces.context.MockFacesContext;

/**
 * Base class for unit tests for Validators
 *
 * @author vijay venkatarman
 */
public abstract class ValidatorTestCase extends TestCase
{
  public ValidatorTestCase(String testName)
  {
    super(testName);
  }

  protected static void setMockLabelForComponent(MockUIComponent component)
  {
    Map attributes = new HashMap();
    attributes.put("label", "label");
    component.setupGetAttributes(attributes);
  }

  /**
   * Tests that null returns immediately.
   *
   * @throws ValidatorException  when test fails
   */
  protected void doTestNull(
    MockFacesContext context,
    MockUIComponent component,
    Validator validator
    ) throws ValidatorException
  {
    validator.validate(context, component, null);

    context.verify();
    component.verify();
  }

  /**
   * if contex or component = null then should throw NullPointerException
   */
  protected void doTestNullContext(
    MockUIComponent component,
    Validator validator) throws NullPointerException
  {
    try
    {
      validator.validate(null, component , "dummy");
      fail("Expected NullpointerException - if context or component is null");
    }
    catch (NullPointerException npe)
    {
      // this is expected
    }
  }

  /**
   * if contex or component = null then should throw NullPointerException
   */
  protected void doTestNullComponent(MockFacesContext context,
    Validator validator ) throws NullPointerException
  {
    try
    {
      validator.validate(context, null, "dummy");
      fail("Expected NullpointerException - if context or component is null");
    }
    catch (NullPointerException npe)
    {
      // this is expected
    }
  }

  /**
   * Test the validate action on validators.
   * @param validator validator on which validate action is to be tested
   * @param context MockFaces context
   * @param component MockFaces component
   * @throws javax.faces.validator.ValidatorException
   */
  protected void doTestValidate(
    Validator validator,
    MockFacesContext context,
    MockUIComponent component,
    Object value
    )  throws ValidatorException
  {
    validator.validate(context, component, value );
    context.verify();
    component.verify();
  }

  /**
   * Test for equality or mismatch of Validators
   * @param thisValidator
   * @param otherValidator
   * @param isEqual - Identifies whether the comparison for equality of validators
   *        or mismatch of valiadtors
   */
  protected void doTestEquals(
    Validator thisValidator,
    Validator otherValidator,
    boolean isEqual)
  {
    assertEquals(isEqual, thisValidator.equals(otherValidator));
  }

  /**
   * Test to check for Validators which implements the StateHolder interface
   * @param thisValidator  Source validator
   * @param otherValidator The validator in which the state will be restored to
   * @param context MockFaces context
   * @param component MockFaces Component
   */
  protected void doTestStateHolderSaveRestore(
    Validator thisValidator,
    Validator otherValidator,
    MockFacesContext context,
    MockUIComponent component)
  {
    Object state = ((StateHolder)thisValidator).saveState(context);

    ((StateHolder)otherValidator).restoreState(context, state);
    // do all actions of save and restore
    doTestEquals(thisValidator, otherValidator, true);
    context.verify();
    component.verify();
  }

  public void doTestIsNotString(Validator validator)
  {
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();

    UIViewRoot root = new UIViewRoot();
    Locale mockLocale = new Locale("xx", "MOCK");
    root.setLocale(mockLocale);
    component.setupGetId("test");
    context.setupGetViewRoot(root);
    context.setupGetViewRoot(root);
    try
    {
      validator.validate(context, component, new Integer(1));
      // if exception is not thrown - mark it as an failure
      fail("Expected Validator Exception");
    }
    catch (IllegalArgumentException iae)
    {
      // if exception then fine.
    }
  }
}

 //////////////////////////////////////////////////////////////////////////////
  //                             MOCK OBJECTS
  // 1. Get a MockControl for the interface we would like to simulate
  // 2. get the MockObject from MockControl
  // 3. specify the behaviour of the Mock Object (record state)
  // 4. activate the MockObject via the control  (replay state)
  //
  //////////////////////////////////////////////////////////////////////////////
