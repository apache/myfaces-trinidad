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
import java.text.DecimalFormatSymbols;

import java.util.Locale;

import javax.faces.component.UIViewRoot;
import javax.faces.convert.ConverterException;
import javax.faces.convert.NumberConverter;

import org.apache.myfaces.adf.context.AdfFacesContext;
import org.apache.myfaces.adf.context.MockAdfFacesContext;
import org.apache.myfaces.adfbuild.test.MockUtils;

import javax.faces.component.MockUIComponent;
import javax.faces.context.MockFacesContext;

/**
 * Test ADF NumberConverter
 * @author Vijay Venaktaraman (vijay.venkataraman@oracle.com)
 * @version $Name: $ ($version: $) $Date: 16-aug-2005.15:12:23 $
 */
public class AdfNumberConverterTest extends NumberConverterTestCase
{
  public AdfNumberConverterTest(String name)
  {
    super(name);
  }

  protected NumberConverter getNumberConverter()
  {
    return new org.apache.myfaces.adf.convert.NumberConverter();
  }

  protected void setUp()
  {
    _mafct = new MockAdfFacesContext();
    //12 attrs * 2
    for (int i = 0; i < 24; i++)
    {
      _mafct.setupGetDecimalSeparator('.');
      _mafct.setupGetNumberGroupingSeparator(',');
      _mafct.setupGetCurrencyCode(null);
    }
    _afct = getCustomMockAdfFacesContext(_mafct);
  }

  protected void tearDown()
  {

   // AdfFacesContext uses a thread local variable to hold itself and has a
   // check in it. So you need to release, since all instances for tests
   // are created on the same thread by Junit.
    _afct.release();
    _mafct.verify();
    _mafct = null;
    _afct = null;
  }

  public void testCurrencyCodeIsHonoured()
  {
     DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.US);
    _mafct.setupGetDecimalSeparator(symbols.getMonetaryDecimalSeparator());
    _mafct.setupGetNumberGroupingSeparator(symbols.getGroupingSeparator());
    super.testCurrencyCodeIsHonoured();
  }

  public void testValueSetInAdfFacesContextIsHonoured()
  {
    tearDown();
    _mafct = new MockAdfFacesContext();
    _afct = super.getCustomMockAdfFacesContext(_mafct);

    _mafct.setupGetDecimalSeparator('*');
    _mafct.setupGetNumberGroupingSeparator('!');
    _mafct.setupGetCurrencyCode(null);
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();

    NumberConverter conv = getNumberConverter();

    conv.setLocale(Locale.US);
    Number inputValue =  new Double(8989.789);
    String out = conv.getAsString(context, component, inputValue);
    assertEquals("8!989*789", out);

    context.verify();
    component.verify();
  }

  protected void doTestStrictNess(
    MockFacesContext context,
    MockUIComponent component,
    Locale locale,
    String inputValue)
  {
     NumberConverter converter = getNumberConverter();
     converter.setLocale(locale);
     UIViewRoot root = new UIViewRoot();
     root.setLocale(locale);
     context.setupGetViewRoot(root);
     context.setupGetViewRoot(root);
     try
     {
       // ADF Converter is not lenient.
       Object obj = converter.getAsObject(context, component, inputValue);
       fail("Expected converter exception");
     }
     catch (ConverterException ce)
     {
       ; // We expected a exception to occur
     }
  }

  public void testCustomMessageIsSet()
  {
    String[] failingValues = {"222.22.2", "3,",       "23.3.3",   "10e.04"};
    String[] types         = {"number",   "percent",  "currency", "pattern"};
    String[] customMessage = {"number", "percent",    "currency", "pattern"};

    for (int i = 0; i < failingValues.length ; i++)
    {
      MockFacesContext context  = new MockFacesContext();
      MockUIComponent component = MockUtils.buildMockUIComponent(3);

      org.apache.myfaces.adf.convert.NumberConverter converter =
        new org.apache.myfaces.adf.convert.NumberConverter();

      UIViewRoot root = new UIViewRoot();
      root.setLocale(Locale.US);

      for (int j = 0; j < 3; j++)
      {
        context.setupGetViewRoot(root);
      }

      try
      {
         // ADF Converter is not lenient.
         converter.setConvertNumberMessageDetail(customMessage[0]);
         converter.setConvertPercentMessageDetail(customMessage[1]);
         converter.setConvertCurrencyMessageDetail(customMessage[2]);
         converter.setConvertPatternMessageDetail(customMessage[3]);

         if ("pattern".equals(types[i]))
            converter.setPattern("##.000");
         else
          converter.setType(types[i]);

         Object obj = converter.getAsObject(context, component, failingValues[i]);
         fail("Expected converter exception");
      }
      catch (ConverterException ce)
      {
        // We expected a exception to occur
        assertEquals(ce.getFacesMessage().getDetail(), customMessage[i]);
      }
    }
  }

  private AdfFacesContext _afct;

  private MockAdfFacesContext _mafct;

  //checkForSettingsInAdfFacesContext - dec sep, currencyCode, NumGrpSptr
}
