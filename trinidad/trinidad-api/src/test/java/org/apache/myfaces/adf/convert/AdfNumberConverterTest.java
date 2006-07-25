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

import javax.faces.convert.ConverterException;
import javax.faces.convert.NumberConverter;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;

import org.apache.myfaces.adf.context.MockAdfFacesContext;
import org.apache.myfaces.adfbuild.test.MockUIComponentWrapper;
import org.apache.shale.test.mock.MockFacesContext;
import org.jmock.Mock;

import junit.framework.Test;
import junit.framework.TestSuite;

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

  public void setUp()
  {
    super.setUp();
    _mafct = new MockAdfFacesContext();
    _mafct.setDecimalSeparator('.');
    _mafct.setNumberGroupingSeparator(',');
    _mafct.setCurrencyCode(null);
  }

  public void tearDown()
  {
    
    // AdfFacesContext uses a thread local variable to hold itself and has a
    // check in it. So you need to release, since all instances for tests
    // are created on the same thread by Junit.
    _mafct.release();
    _mafct = null;
    super.tearDown();
  }

  public static Test suite()
  {
    return new TestSuite(AdfNumberConverterTest.class);
  }
  
  public void testCurrencyCodeIsHonoured()
  {
     DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.US);
    _mafct.setDecimalSeparator(symbols.getMonetaryDecimalSeparator());
    _mafct.setNumberGroupingSeparator(symbols.getGroupingSeparator());
    super.testCurrencyCodeIsHonoured();
  }

  public void testValueSetInAdfFacesContextIsHonoured()
  {
    //ugly ?
    _mafct.release();
    _mafct = null;
    _mafct = new MockAdfFacesContext();
    _mafct.setDecimalSeparator('*');
    _mafct.setNumberGroupingSeparator('!');
    _mafct.setCurrencyCode(null);
    Mock mock = mock(UIComponent.class);
    UIComponent component = (UIComponent) mock.proxy();

    NumberConverter conv = getNumberConverter();

    conv.setLocale(Locale.US);
    Number inputValue =  new Double(8989.789);
    String out = conv.getAsString(facesContext, component, inputValue);
    assertEquals("8!989*789", out);

    mock.verify();
  }

  protected void doTestStrictNess(
    MockFacesContext context,
    MockUIComponentWrapper wrapper,
    Locale locale,
    String inputValue)
  {
     NumberConverter converter = getNumberConverter();
     converter.setLocale(locale);
     context.getViewRoot().setLocale(locale);
     try
     {
       // ADF Converter is not lenient.
       Object obj = converter.getAsObject(context, wrapper.getUIComponent(), inputValue);
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
      Mock mock = buildMockUIComponent(3);
      UIComponent component = (UIComponent) mock.proxy();
      MockUIComponentWrapper wrapper = new MockUIComponentWrapper(mock, component);


      org.apache.myfaces.adf.convert.NumberConverter converter =
        new org.apache.myfaces.adf.convert.NumberConverter();

      UIViewRoot root = facesContext.getViewRoot();
      root.setLocale(Locale.US);
      

      for (int j = 0; j < 3; j++)
      {
        context.setViewRoot(root);
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

  private MockAdfFacesContext _mafct;

  //checkForSettingsInAdfFacesContext - dec sep, currencyCode, NumGrpSptr
}
