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
import java.util.Locale;

import javax.faces.component.UIViewRoot;
import javax.faces.validator.ValidatorException;

import org.apache.myfaces.adf.validator.RegExpValidator;

import javax.faces.component.MockUIComponent;
import javax.faces.context.MockFacesContext;

import org.apache.myfaces.adfbuild.test.MockUtils;

/**
 * Unit tests for RegExpValidator
 *
 * @author vijay venkataraman
 */
public class RegExpValidatorTest extends ValidatorTestCase
{
  public RegExpValidatorTest(String testName)
  {
    super(testName);
  }

  /**
   * Tests that null returns immediately.
   *
   * @throws ValidatorException  when test fails
   */
  public void testNull() throws ValidatorException
  {
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = MockUtils.buildMockUIComponent();
    RegExpValidator validator = new RegExpValidator();

    doTestNull(context, component, validator);
  }

  /**
   * Test when context is set to null
   */
  public void testNullContext()
  {
    MockUIComponent component = MockUtils.buildMockUIComponent();
    RegExpValidator validator = new RegExpValidator();

    doTestNullContext(component, validator);
  }

  public void testNullComponent()
  {
    MockFacesContext context  = new MockFacesContext();
    RegExpValidator validator = new RegExpValidator();

    doTestNullComponent(context, validator);
  }

  /**
   * Tests that non String objects throw a ValidationException.
   */
  public void testNotString()
  {
    doTestIsNotString(new RegExpValidator());
  }

  /**
   * Test that pattern when not set throws null pointer exceptin
   */
  public void testPatternNotSet()
  {
    // since the pattern has not been set it will be null
    // let us push some arbitary value
    MockFacesContext context   = new MockFacesContext();
    MockUIComponent component = MockUtils.buildMockUIComponent();

    try
    {
      RegExpValidator validator = new RegExpValidator();
      validator.validate(context, component, "someValue");
      // test fails if it is here

      fail("Expected Null pointer exception");
    }
    catch (NullPointerException npe)
    {
      // suppress it - this is as expected
    }
    context.verify();
    component.verify();
  }

  /**
   * Test that pattern when set to "" should fail validation
   */
  public void testBlankValueOnPattern()
  {
    // some very basic sanity test
    MockFacesContext context   = new MockFacesContext();
    MockUIComponent component = MockUtils.buildMockUIComponent();
    setMockLabelForComponent(component);
    UIViewRoot root = new UIViewRoot();
    Locale mockLocale = new Locale("xx", "MOCK");
    root.setLocale(mockLocale);
    context.setupGetViewRoot(root);
    context.setupGetViewRoot(root);

    try
    {
      RegExpValidator validator = new RegExpValidator();
      String value = "999999";
      validator.setPattern("");
      validator.validate(context, component, value);
      fail("Expected ValidatorException");
    }
    catch (ValidatorException ve)
    {
      // if exception then fine.
    }

    context.verify();
    component.verify();
  }

  /**
   * Simple test case which is expected to pass
   * @todo need to add many test cases - add string to the values and
   *       patterns to patterns array.
   *
   */
  public void testSanitySuccess()
  {
    //some very basic sanity test
    //
    RegExpValidator validator = new RegExpValidator();
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = MockUtils.buildMockUIComponent();

    String values[]   = {"9123456","9x"};
    String patterns[] = {"[0-9]*","[9][x]"};
    for (int i = 0; i < values.length ; i++)
    {
      validator.setPattern(patterns[i]);
      doTestValidate(validator, context, component, values[i]);
    }
  }

  /**
   * Tests that dates after the date range cause a ValidationException.
   */
  public void testStateHolderSaveRestore()
  {
    RegExpValidator validator = new RegExpValidator();
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = MockUtils.buildMockUIComponent();

    validator.setPattern("TestSaveRestore");
    validator.setNoMatchMessageDetail("\"{0}\" in \"{1}\" failed!! {4}");
    RegExpValidator restoreValidator = new  RegExpValidator();

    doTestStateHolderSaveRestore(validator, restoreValidator,
                                 context, component);
  }

  /**
   * Test for equality of validators
   */
  public void testIsEqual()
  {
    RegExpValidator validator = new RegExpValidator();
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = MockUtils.buildMockUIComponent();
    RegExpValidator otherValidator = new RegExpValidator();
    doTestEquals(validator, otherValidator, true);
    assertEquals(validator.hashCode(), otherValidator.hashCode());

    validator.setPattern("[0-9]");
    validator.setNoMatchMessageDetail("\"{0}\" in \"{1}\" failed!! {4}");
    otherValidator.setPattern("[0-9]");
    otherValidator.setNoMatchMessageDetail("\"{0}\" in \"{1}\" failed!! {4}");
    doTestEquals(validator, otherValidator, true);
    assertEquals(validator.hashCode(), otherValidator.hashCode());

    otherValidator.setPattern(null);
    doTestEquals(validator, otherValidator, false);
    assertEquals(false, (validator.hashCode() == otherValidator.hashCode()));
  }

  public void testCustomMessageIsSet()
  {
    MockFacesContext context      = new MockFacesContext();
    MockUIComponent component     = MockUtils.buildMockUIComponent();
    UIViewRoot root = new UIViewRoot();
    Locale usLoc =  Locale.US;
    root.setLocale(usLoc);
    context.setupGetViewRoot(root);
    context.setupGetViewRoot(root);
    setMockLabelForComponent(component);
    RegExpValidator validator = new RegExpValidator();

    validator.setPattern("[0-9]*");
    validator.setNoMatchMessageDetail("\"{0}\" in \"{1}\" failed!! {4}");
    //some very basic sanity test

    try
    {
      validator.validate(context, component, "9123456");
    }
    catch (ValidatorException ve)
    {
      String msg = ve.getFacesMessage().getDetail();
      assertEquals(msg, "\"four\" in \"label\" failed!! [0-9]*");
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
