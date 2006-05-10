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

package org.apache.myfaces.adf.convert;
import java.util.Locale;

import javax.faces.component.UIViewRoot;
import javax.faces.convert.ConverterException;
import javax.faces.convert.NumberConverter;

import org.apache.myfaces.adf.convert.ConverterTestCase;
import org.apache.myfaces.adfbuild.test.MockUtils;

import javax.faces.application.MockApplication;
import javax.faces.component.MockUIComponent;
import javax.faces.context.MockExternalContext;
import javax.faces.context.MockFacesContext;

/**
 * Test NumberConverter
 * @author Vijay Venaktaraman (vijay.venkataraman@oracle.com)
 * @version $Name: $ ($version: $) $Date: 16-aug-2005.15:12:23 $
 */
public abstract class NumberConverterTestCase extends ConverterTestCase
{
  public NumberConverterTestCase(String name)
  {
    super(name);
  }

  /**
   * Test when context is set to null
   */
  public void testNullContext()
  {
    MockUIComponent component    = new MockUIComponent();
    NumberConverter converter = getNumberConverter();

    doTestNullContext(component, converter);
  }

  public void testNullComponent()
  {
    MockFacesContext context = new MockFacesContext();
    NumberConverter converter  = getNumberConverter();

    doTestNullComponent(context, converter);
  }

  /**
   * Tests that null returns immediately.
   *
   * @throws ConverterException  when test fails
   */
  public void testNullInputValue() throws ConverterException
  {
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();

    NumberConverter converter  = getNumberConverter();

    doTestNull(context, component, converter);
  }

  public void testEmptyValueConversion()
  {
    super.doTestBlankValue(getNumberConverter());
  }

  public void testValueType()
  {
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();
    UIViewRoot root = new UIViewRoot();
    root.setLocale(Locale.US);
    context.setupGetViewRoot(root);
    context.setupGetViewRoot(root);

    MockUtils.setFacesContext(context);
    try
    {
      String input = "123";
      NumberConverter converter = getNumberConverter();
      Object number = converter.getAsObject(context, component, input);
      assertEquals(true, number instanceof Number);
      assertEquals(true, (((Number)number).intValue() == 123));

      String outVal = converter.getAsString(context, component, number);
      assertEquals(input, outVal);
    }
    finally
    {
      MockUtils.setFacesContext(null);
    }

    context.verify();
    component.verify();
  }

  public void testAppropriateFormatsArePicked()
  {
    // check if appropriate formtas based on the types are chosen.
    // like numeric, currency, percent are picked. Chose pattern if set avoiding
    // types

    String[] patterns = {"##,##",null, null, null,null};

    //pick pattern, numeric, percent, currency
    String[] types = {null,"number", "percent", "currency", "currency"};
    String[] inputValues = {"99,99","99", "99%","$99", "$99.00"} ;
    Number[] expectedValues = {new Long(9999), new Long(99), new Double(0.99), new Long(99), new Long(99)};
    String[] expectedStringValues = {"99,99","99", "99%","$99.00", "$99.00"} ;
    Locale usLocl = Locale.US;
    Locale[] locales = {usLocl, usLocl, usLocl, usLocl,Locale.CANADA};

    NumberConverter nconv = getNumberConverter();

    for (int i = 0; i < patterns.length; i++)
    {
      MockFacesContext context  = new MockFacesContext();
      MockUIComponent component = new MockUIComponent();
      
      MockUtils.setFacesContext(context);
      try
      {
        nconv.setPattern(patterns[i]);
        nconv.setType(types[i]);
        nconv.setLocale(locales[i]);
        
        Object convValue = nconv.getAsObject(context, component, inputValues[i]);
        assertEquals(expectedValues[i], convValue);
        
        String outValue = nconv.getAsString(context, component, expectedValues[i]);
        
        assertEquals(expectedStringValues[i], outValue);
      }
      finally
      {
        MockUtils.setFacesContext(null);
      }
      
      context.verify();
      component.verify();
    }
  }

  public void testStateHolderSaveRestore()
  {
    NumberConverter converter = getNumberConverter();
    MockFacesContext context    = new MockFacesContext();
    MockUIComponent component   = new MockUIComponent();

    NumberConverter restoreConverter = getNumberConverter();
    MockFacesContext ctx = new MockFacesContext();
    MockUIComponent comp = new MockUIComponent();

    for (int i = 0; i < _LOCALES.length; i++)
    {
      converter.setLocale(_LOCALES[i]);
      restoreConverter.setLocale(_LOCALES[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter, ctx, comp);

      converter.setCurrencyCode( _CURRENCY_CODES[i]);
      restoreConverter.setCurrencyCode( _CURRENCY_CODES[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter, ctx, comp);

      converter.setCurrencySymbol(_CURRENCY_SYMBOLS[i]);
      restoreConverter.setCurrencySymbol(_CURRENCY_SYMBOLS[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter, ctx, comp);


      converter.setIntegerOnly(_INTEGER_ONLY[1]);
      restoreConverter.setIntegerOnly(_INTEGER_ONLY[1]);
      doTestStateHolderSaveRestore(converter, restoreConverter, ctx, comp);

      converter.setMaxFractionDigits(_MAX_FRACTION_DIGITS[i]);
      restoreConverter.setMaxFractionDigits(_MAX_FRACTION_DIGITS[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter, ctx, comp);

      converter.setMaxIntegerDigits(_MAX_INT_DIGITS[i]);
      restoreConverter.setMaxIntegerDigits(_MAX_INT_DIGITS[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter, ctx, comp);

      converter.setMinFractionDigits(_MIN_FRACT_DIGITS[i]);
      restoreConverter.setMinFractionDigits(_MIN_FRACT_DIGITS[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter, ctx, comp);

      converter.setMinIntegerDigits(_MIN_INT_DIGITS[i]);
      restoreConverter.setMinIntegerDigits(_MIN_INT_DIGITS[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter, ctx, comp);

      converter.setPattern( _PATTTERNS[i]);
      restoreConverter.setPattern(_PATTTERNS[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter, ctx, comp);

      converter.setTransient(_TRANSIENT[i]);
      restoreConverter.setTransient(_TRANSIENT[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter, ctx, comp);

      converter.setType(_TYPES[i]);
      doTestStateHolderSaveRestore(converter, restoreConverter, ctx, comp);

    }
  }

  public void testCurrencyCodeIsHonoured()
  {
    NumberConverter converter = getNumberConverter();
    MockFacesContext context    = new MockFacesContext();
    MockUIComponent component   = new MockUIComponent();
    converter.setLocale(Locale.US);
    converter.setType("currency");
    Double  value = new Double(99);

    MockUtils.setFacesContext(context);
    try
    {
      String outPut = converter.getAsString(context, component, value);
      assertEquals("$99.00", outPut);
      //Locale is US. By general convention the output prefix would be '$'
      // since we set the currency code to 'DEM' value should be DEM[value]
      converter.setCurrencyCode("DEM");
      
      outPut = converter.getAsString(context, component, value);
      assertEquals("DEM99.00", outPut);
    }
    finally
    {
      MockUtils.setFacesContext(null);
    }
    context.verify();
    component.verify();
  }

  public void testCurrencyCodeIsHonouredWhenCurrencyCodeAndCurrencySymbolIsSet()
  {
    NumberConverter converter   = getNumberConverter();
    MockFacesContext context    = new MockFacesContext();
    MockApplication application = new MockApplication();
    context.setupGetApplication(application);
    application.setupGetMessageBundle(null);

    MockUIComponent component = MockUtils.buildMockUIComponent(2);
    
    converter.setLocale(Locale.US);
    converter.setType("currency");
    Double  value = new Double(99);

    MockUtils.setFacesContext(context);
    try
    {
      String outPut = converter.getAsString(context, component, value);
      assertEquals("$99.00", outPut);
      //Locale is US. By general convention the output prefix would be '$'
      // since we set the currency code to 'DEM' value should be DEM[value]
      converter.setCurrencyCode("DEM");
      
      // Let us set the symbol to '*'. This should not take effect, since currency
      // code is set.
      converter.setCurrencySymbol("*");
      
      outPut = converter.getAsString(context, component, value);
      assertEquals("DEM99.00", outPut);
      try
      {
        UIViewRoot root = new UIViewRoot();
        root.setLocale(Locale.US);
        context.setupGetViewRoot(root);
        context.setupGetViewRoot(root);
        MockExternalContext external = new MockExternalContext();
        context.setupGetExternalContext(external);
        
        Number outValue = (Number)converter.getAsObject(context, component, "DEM99.00");
        fail("Exception should occur - since currency should not be considered while formatting");
      }
      catch(Exception e)
      {
        ;//Expected to fail.
      }
    }
    finally
    {
      MockUtils.setFacesContext(null);
    }

    context.verify();
    component.verify();
  }

  public void testCurrencySymbolIsHonoured()
  {
    NumberConverter converter = getNumberConverter();
    MockFacesContext context    = new MockFacesContext();
    MockUIComponent component   = new MockUIComponent();
    converter.setLocale(Locale.US);
    converter.setType("currency");
    Double  value = new Double(99);
    //Locale is US. By general convention the output prefix would be '$'
    // since we set currency symbol to '*' we should get the value to be *99.00
    converter.setCurrencySymbol("*");
    
    MockUtils.setFacesContext(context);
    try
    {
      String outPut = converter.getAsString(context, component, value);
      assertEquals("*99.00", outPut);
    }
    finally
    {
      MockUtils.setFacesContext(null);
    }

    context.verify();
    component.verify();
  }

  public void testIntegerOnlyIsHonoured()
  {
    // integerOnly is used only while parsing to create number objects
    NumberConverter converter = getNumberConverter();
    MockFacesContext context    = new MockFacesContext();
    MockUIComponent component   = new MockUIComponent();
    converter.setLocale(Locale.US);

    String[] inputs = {"23.10", "44.90876", "11111", "67859.0001"};
    Number[] expectedValues = {new Long(23), new Long(44), new Long(11111), new Long(67859)};

    MockUtils.setFacesContext(context);
    try
    {
      for (int i = 0; i < inputs.length; i++)
      {
        converter.setIntegerOnly(true);
        Number num = (Number) converter.getAsObject(context, component, inputs[i]);
        assertEquals(expectedValues[i], num);
      }
    }
    finally
    {
      MockUtils.setFacesContext(null);
    }

    context.verify();
    component.verify();
  }


  public void testSettingFractDigitsAndSettingMinDigitsDoesNotAffectParsing()
  {
    // integerOnly is used only while parsing to create number objects
    NumberConverter converter = getNumberConverter();
    MockFacesContext context    = new MockFacesContext();
    MockUIComponent component   = new MockUIComponent();
    converter.setLocale(Locale.US);

    String[] inputs = {"23.10", "44.90876", "11111", "67859.0001"};
    Number[] expectedValues = {new Long(23), new Long(44), new Long(11111), new Long(67859)};

    MockUtils.setFacesContext(context);
    try
    {
      for (int i = 0; i < inputs.length; i++)
      {
        // setting these values should not affect parsing.
        converter.setMaxFractionDigits(10);
        converter.setMaxIntegerDigits(1);
        converter.setMinFractionDigits(1);
        converter.setMinFractionDigits(0);
        
        // this should be taken care by the parsing code
        converter.setIntegerOnly(true);
        Number num = (Number) converter.getAsObject(context, component, inputs[i]);
        assertEquals(expectedValues[i], num);
      }
    }
    finally
    {
      MockUtils.setFacesContext(null);
    }

    context.verify();
    component.verify();
  }

  public void testLocaleIsPickedUpFromViewRoot()
  {

    NumberConverter converter = getNumberConverter();
    MockFacesContext context    = new MockFacesContext();
    MockUIComponent component   = new MockUIComponent();

    UIViewRoot root = new UIViewRoot();
    root.setLocale(Locale.US);
    context.setupGetViewRoot(root);
    context.setupGetViewRoot(root);

    String input = "1234.56";

    MockUtils.setFacesContext(context);
    try
    {
      // if we get a valid object, implies locale was indeed picked up.
      // otherwise we would have got a null pointer exception or other exception
      Object value = converter.getAsObject(context, component, input);
      assertEquals(new Double(1234.56), value);
    }
    finally
    {
      MockUtils.setFacesContext(null);
    }
  }

  public void testGroupingIsHonoured()
  {
    Number[] inputValues = {new Long(9999), new Long(99), new Double(0.99), new Double(99999.567), new Long(9999)};
    boolean [] isGroupingUsed = {true, true, true, false, false };
    String[] expectedValues = {"9,999", "99", "0.99", "99999.567", "9999"};

    NumberConverter converter = getNumberConverter();
    MockFacesContext context    = new MockFacesContext();
    MockUIComponent component   = new MockUIComponent();
    converter.setLocale(Locale.US);
    MockUtils.setFacesContext(context);
    try
    {
      for (int i = 0; i < inputValues.length; i++)
      {
        converter.setGroupingUsed(isGroupingUsed[i]);
        String out = converter.getAsString(context, component, inputValues[i]);
        assertEquals(expectedValues[i], out);
      }
    }
    finally
    {
      MockUtils.setFacesContext(null);
    }

    context.verify();
    component.verify();
  }

  public void testStrictnessOfConversion()
  {
    String[] inputValues = {"123ABC", "22.22.2" };
    MockFacesContext context    = new MockFacesContext();
    MockUIComponent component
      = MockUtils.buildMockUIComponent(inputValues.length * 3);

    for (int i = 0; i < inputValues.length; i++)
    {
      doTestStrictNess(context, component, Locale.US, inputValues[i]);
    }
    context.verify();
    component.verify();
  }

  public void testSettingFractDigitsAndSettingMinDigitsAreHononured()
  {
    Number[] inputValues = {new Long(1234), new Double(1234.5678), new Double(1234), new Double(10.00)};
    String[] expectedValues = {"1,234", "34.57", "1,234", "10.00"};

    int[] maxFractDigits = {0, 2, 2, 2};
    int[] maxIntDigits   = {4, 2, 4, 3};
    int[] minIntDigits   = {4, 1, 2, 1};
    int[] minFractDigits = {0, 2, 0, 2};

    NumberConverter converter   = getNumberConverter();
    MockFacesContext context    = new MockFacesContext();
    MockUIComponent component   = new MockUIComponent();

    MockUtils.setFacesContext(context);
    try
    {
      converter.setLocale(Locale.US);
      for (int i = 0; i < maxFractDigits.length; i++)
      {
        converter.setMaxFractionDigits(maxFractDigits[i]);
        converter.setMaxIntegerDigits(maxIntDigits[i]);
        converter.setMinFractionDigits(minFractDigits[i]);
        converter.setMinIntegerDigits(minIntDigits[i]);
        
        String out = converter.getAsString(context, component, inputValues[i]);
        assertEquals(expectedValues[i], out);   
      }
    }
    finally
    {
      MockUtils.setFacesContext(null);
    }
    context.verify();
    component.verify();
  }

  protected abstract NumberConverter getNumberConverter();

  protected abstract void doTestStrictNess(
    MockFacesContext context,
    MockUIComponent component,
    Locale locale,
    String inputValue);

  private static final String[] _CURRENCY_CODES = {"USD", "DEM" };

  private static final String[] _CURRENCY_SYMBOLS = {"*", "!"};

  private static final Locale[] _LOCALES = {Locale.US, Locale.GERMAN};

  private static final int[] _MAX_FRACTION_DIGITS = {2, 3};

  private static final int[] _MAX_INT_DIGITS = {5, 6};

  private static final int[] _MIN_FRACT_DIGITS = {2, 3};

  private static final int[] _MIN_INT_DIGITS = {2, 3};

  private static final String[] _PATTTERNS = {"##,##", null};

  private static final String[] _TYPES = {"currency","percent"};

  private static final boolean[] _GROUPING = {true, false};

  private static final boolean[] _INTEGER_ONLY = {true, false};

  private static final boolean[] _TRANSIENT = {true, false};

}
// DONOT DELETE LET THESE STAY HERE.
// CurrencyCode
// CurrencySymbol
// Locale
// MaxFractionDigits
// MaxIntegerDigits
// MinFractionDigits
// MinIntegerDigits
// Pattern
// Type
// GroupingUsed
// IntegerOnly

//Currency Code | Country Currency
//USD - United States Dollar
//
//ITL - Italian Lira
//
//DEM - German Mark
//
//HKG - Hong Kong Dollar
//
//MXN - Mexican Peso
//
//EUR - Euro

// CurrencyCode        tested
// CurrencySymbol      tested
// Locale              tested  to pick up from viewRoot


// MaxFractionDigits      only while formatting
// MaxIntegerDigits       only while formatting
// MinFractionDigits      only while formatting
// MinIntegerDigits       only while formatting

// Pattern             tested
// Type                tested
// GroupingUsed        tested
// IntegerOnly         tested   // only while parsing
