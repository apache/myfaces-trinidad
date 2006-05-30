/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adf.convert;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;

import javax.faces.component.UIViewRoot;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.adf.context.AdfFacesContext;
import org.apache.myfaces.adf.context.MockAdfFacesContext;

import javax.faces.component.MockUIComponent;
import javax.faces.context.MockFacesContext;

import org.apache.myfaces.adfbuild.test.MockUtils;

public class AdfDateTimeConverterTest extends DateTimeConverterTestCase
{
  public AdfDateTimeConverterTest(String name)
  {
    super(name);
  }

  /**
   * @todo move this to the parent class once JSF fixes the bug
   */
  public void testEarlyExits()
  {
    checkNullComponent();
    checkNullContext();
  }

  public void testShortishForDatePatern()
  {
    GregorianCalendar gcal = new GregorianCalendar();
    gcal.set(2999,Calendar.JUNE,4,0,0,0);

    gcal.setTimeZone(DEFAULT_TIME_ZONE);
    // Make use of this date for testing.
    Date date = gcal.getTime();

    DateTimeConverter dtConv   = new DateTimeConverter();
    MockFacesContext context   = new MockFacesContext();
    MockUIComponent component  = MockUtils.buildMockUIComponent();
    String inputValue          = "6/4/2999";

    dtConv.setDateStyle("shortish");
    dtConv.setLocale(Locale.ENGLISH);

    Date dt = (Date) dtConv.getAsObject(context, component, inputValue);
    assertEquals(true, isEqual(date, dt));

    String exptectedStr = dtConv.getAsString(context, component, dt);
    assertEquals(inputValue, exptectedStr);
    context.verify();
    component.verify();
  }

  public void testShortishDateStyle()
  {
    doTestStyleValidity(_DATE_STYLE, new String[]{"shortish"});
  }

  public void testSecondaryPattern()
  {
    // Get as object should work fine - while getAsString is expected to fail
    GregorianCalendar gcal = new GregorianCalendar();
    gcal.set(1600,Calendar.JUNE,4,0,0,0);

    gcal.setTimeZone(DEFAULT_TIME_ZONE);
    // Make use of this date for testing.
    Date date = gcal.getTime();

    DateTimeConverter dtConv   = new DateTimeConverter();
    MockFacesContext context   = new MockFacesContext();
    MockUIComponent component  = MockUtils.buildMockUIComponent();
    String inputValue          = "6/4/1600";
    String secondaryPattern    = "MM/d/yyyy";

    dtConv.setLocale(Locale.US);
    dtConv.setDateStyle("Let us unset it ");
    dtConv.setType("Let us un set it");
    dtConv.setSecondaryPattern(secondaryPattern);
    // This should work fine
    Date dt = (Date) dtConv.getAsObject(context, component, inputValue);
    assertEquals(true, isEqual(date, dt));

    try
    {
      dtConv.getAsString(context, component, dt);
      fail("Use of secondary pattern in the above fashion is expected to fail here");
    }
    catch (RuntimeException ce)
    {
      // Just proceed . This is the expected state
    }

    dtConv.setDateStyle("shortish");
    dtConv.setType("date");

    // now we set date and type so this is expected to work fine.

    String expectedOut = dtConv.getAsString(context, component, date);
    assertEquals(inputValue, expectedOut);

    context.verify();
    component.verify();
  }

  public void testLeniencyOnPrimaryPattern()
  {
    String primaryPattern = "MMM/d/yyyy";
    String secondaryPattern = null;
    dotestLeniencyOnPattern(primaryPattern, secondaryPattern);
  }

  public void testLeniencyOnSecondaryPattern()
  {
    String primaryPattern = null;
    String secondaryPattern = "MMM/d/yyyy";
    dotestLeniencyOnPattern(primaryPattern, secondaryPattern);
  }

  private void _setupFacesContext(MockFacesContext context, int count)
  {
    UIViewRoot uiRoot = new UIViewRoot();
    uiRoot.setLocale(Locale.ENGLISH);

    for (int j = 0; j < count; j++)
      context.setupGetViewRoot(uiRoot);
  }

  protected void dotestLeniencyOnPattern(
    String primaryPattern,
    String secondaryPatttern
    )
  {
    // inputs such as 6/7/2004 is also valid. Avoiding it since - equality
    // of the output is also compared.

    // Each of these inputs ends up causing the MessageFactory to grab the
    // label from the component a different number of times, so the number of
    // iterations to set the component up for is difficult to figure. The
    // numbers after the input string are the number of iterations of
    // getAttribute that we need to set up for.
    String[] validInputs =
      {
        "Jun/4/2004" /* 0 */, "Jun-4-2004" /* 10 */, "Jun.4.2004" /* 8 */,
        "06/4/2004"  /* 4 */, "06-04-2004" /* 12 */, "06.04.2004" /* 2 */,
        "6/4/2004"   /* 4 */, "6-4-2004"   /* 12 */, "6.4.2004"   /* 2 */
      };

    int iterations = (0 + 10 + 8 + 4 + 12 + 2 + 4 + 12 + 2);
    GregorianCalendar cal = new GregorianCalendar(2004, Calendar.JUNE, 4);
    cal.setTimeZone(DEFAULT_TIME_ZONE);
    Date dt = cal.getTime();
    MockUIComponent component
       = MockUtils.buildMockUIComponent(iterations);
    for (int i = 0; i < validInputs.length; i++)
    {
      MockFacesContext context  = new MockFacesContext();
      DateTimeConverter
        dtConv = (DateTimeConverter) getDateTimeConverter();
      dtConv.setLocale(Locale.ENGLISH);
      dtConv.setPattern(primaryPattern);
      dtConv.setSecondaryPattern(secondaryPatttern);
      dtConv.setTimeZone(DEFAULT_TIME_ZONE);
      dtConv.setType("INVALID"); // make this type invalid

      _setupFacesContext(context, validInputs.length * 3);

      Date convDate = (Date) dtConv.getAsObject(context, component,
                                                validInputs[i]);
      assertEquals(convDate, dt);
      context.verify();
      component.verify();
    }
  }

  public void testCompareDateTimeConverter()
  {
    Object[][] data = _getDataForPatterns();

    for (int i = 0; i < data.length ; i++)
    {
      DateTimeConverter dtConv = new DateTimeConverter();
      dtConv.setPattern((String)data[i][0]);
      dtConv.setLocale((Locale)data[i][2]);
      dtConv.setTimeZone((TimeZone)data[i][3]);
      String inputValue = (String)data[i][1];

      javax.faces.convert.DateTimeConverter fdtConv
        = new javax.faces.convert.DateTimeConverter();
      fdtConv.setPattern((String)data[i][0]);
      fdtConv.setLocale((Locale)data[i][2]);
      fdtConv.setTimeZone((TimeZone)data[i][3]);

      MockFacesContext context  = new MockFacesContext();
      _setupFacesContext(context, 4);
      MockUIComponent component = MockUtils.buildMockUIComponent();
      Date dtConvDate  = (Date)dtConv.getAsObject(context, component, inputValue);
      Date fdtConvDate = (Date)fdtConv.getAsObject(context, component, inputValue);
      //      assertEquals(dtConvDate, fdtConvDate);

      String dtConvPattern  = dtConv.getAsString(context, component, dtConvDate);
      String fdtConvPattern = fdtConv.getAsString(context, component, dtConvDate);
      //      assertEquals(dtConvPattern, fdtConvPattern);
    }
  }

  protected javax.faces.convert.DateTimeConverter getDateTimeConverter()
  {
    return new DateTimeConverter();
  }

  protected void setSecondaryPattern(
    javax.faces.convert.DateTimeConverter converter,
    String secondaryPattern
    )
  {
    ((DateTimeConverter)converter).setSecondaryPattern(secondaryPattern);
  }

  protected void doTestStateHolderSaveRestore(
    Converter conv1,
    Converter conv2,
    MockFacesContext context,
    MockUIComponent component
    )
  {
    super.doTestStateHolderSaveRestore(conv1, conv2, context, component);
  }

  public void testCustomMessageIsSet()
  {
    //default is shortish - M/d/yyyy
    //default time is short hh:m A.M/P.M
    //default both
    //let us choose pattern as M/d/yyyy
    String[] failingValues = {"15/2/2002", "02;30 A.M,", "15/2/2002 22:22 A*M.", "M/d/yyyy"};
    String[] types         = {"date",   "time",  "both", "pattern"};
    String[] customMessage = {"date",   "time",  "both", "pattern"};

    for (int i = 0; i < failingValues.length ; i++)
    {
      MockFacesContext context  = new MockFacesContext();
      MockUIComponent component = MockUtils.buildMockUIComponent(3 * 4);

      org.apache.myfaces.adf.convert.DateTimeConverter converter =
        new org.apache.myfaces.adf.convert.DateTimeConverter();

      UIViewRoot root = new UIViewRoot();
      root.setLocale(Locale.US);

      for (int j = 0; j < 3; j++)
      {
        for (int k = 0; k < 4; k++)
          context.setupGetViewRoot(root);
      }

      try
      {
        // ADF Converter is not lenient.
        converter.setConvertDateMessageDetail(customMessage[0]);
        converter.setConvertTimeMessageDetail(customMessage[1]);
        converter.setConvertBothMessageDetail(customMessage[2]);
        // pattern and date type is driven using the same message.


        if ("pattern".equals(types[i]))
        {
          converter.setPattern("M/d/yyyy");
          // There is no specific messaging scheme for pattern. So use the
          // dateMessageDetail itself for this.
          converter.setConvertDateMessageDetail(customMessage[3]);
        }
        else
          converter.setType(types[i]);

        Object obj = converter.getAsObject(context, component, failingValues[i]);
        fail("Expected converter exception");
      }
      catch (ConverterException ce)
      {
        // We expected a exception to occur
        String msg = ce.getFacesMessage().getDetail();
        assertEquals(msg, customMessage[i]);
      }
    }
  }

  protected void setUp()
  {
    _mafct = new MockAdfFacesContext();
    _mafct.setTwoDigitYearStart(1950);
    _mafct.setTimeZone(DEFAULT_TIME_ZONE);
  }

  protected void tearDown()
  {
    _mafct.release();
    _mafct = null;
  }

  private Object[][] _getDataForPatterns()
  {
    // pattern, inputvalue,locale,timezone
    Object[][] data =
    {
      {"yyyy.MM.dd G 'at' HH:mm:ss z", "2001.07.04 AD at 12:08:56 PDT", Locale.US, null },

      {"EEE, MMM d, ''yy","Wed, Jul 4, '01", Locale.ENGLISH, getTzone("GMT")},

      {"h:mm a","12:08 PM", Locale.GERMAN, getTzone("GMT+1")},

      {"hh 'o''clock' a, zzzz","12 o'clock PM, Pacific Standard Time", Locale.CANADA, getTzone("GMT-8")},

      {"K:mm a, z","0:08 PM, PST", Locale.US, getTzone("PST")},

      {"yyyyy.MMMMM.dd GGG hh:mm aaa","02001.July.04 AD 12:08 PM", Locale.US, null},
      {"EEE, d MMM yyyy HH:mm:ss Z","Wed, 4 Jul 2001 12:08:56 GMT",Locale.US, getTzone("GMT")},
      {"yyMMddHHmmss", "010704120856", Locale.ENGLISH, null, null},

    };
    return data;
  }

  private MockAdfFacesContext _mafct;

  private static final TimeZone DEFAULT_TIME_ZONE = TimeZone.getDefault();
}

