/*
 * Copyright 2000-2003,2006 The Apache Software Foundation.
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

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import javax.faces.component.UIViewRoot;
import javax.faces.convert.ConverterException;
import javax.faces.component.MockUIComponent;
import javax.faces.context.MockFacesContext;;

/**
 * Unit tests for ColorConverter
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/test/java/oracle/adf/view/faces/convert/ColorConverterTest.java#1 $) $Date: 16-aug-2005.15:12:23 $
 * @author vijay venkataraman (vijay.venkataraman@oracle.com)
 */
public class ColorConverterTest extends ConverterTestCase
{
  public ColorConverterTest(String testName)
  {
    super(testName);
  }

  /**
   * Tests that null returns immediately.
   *
   * @throws ConverterException  when test fails
   */
  public void testNull() throws ConverterException
  {
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();
    ColorConverter converter  = new ColorConverter();

    doTestNull(context, component, converter);
  }

  /**
   * Test when context is set to null
   */
  public void testNullContext()
  {
    MockUIComponent component = new MockUIComponent();
    ColorConverter converter  = new ColorConverter();

    doTestNullContext(component, converter);
  }

  public void testNullComponent()
  {
    MockFacesContext context  = new MockFacesContext();
    ColorConverter converter  = new ColorConverter();

    doTestNullComponent(context, converter);
  }

  public void testEmptyValueConversion()
  {
    super.doTestBlankValue(new ColorConverter());
  }

  /**
   * Test for equality of converters
   */
  public void testEquals()
  {
    ColorConverter converter  = new ColorConverter();
    ColorConverter otherConverter = new ColorConverter();
    doTestEquals(converter, otherConverter, true);

    String[] patterns = {"rrr,ggg,bbb", "rrr-ggg-bbb"};
    String[] otherPatterns = {"rrr,ggg,bbb", "rrr-ggg-bbb"};
    converter.setPatterns(patterns);
    converter.setConvertMessageDetail("Test message detail");
    otherConverter.setConvertMessageDetail("Test message detail");
    otherConverter.setPatterns(otherPatterns);
    doTestEquals(converter, otherConverter, true);

    String[] newPattern = {"#RRGGBB", "RR.GG.BB"};
    otherConverter.setPatterns(newPattern);
    doTestEquals(converter, otherConverter, false);

    // check by modifiying the tranisent to be differnt
    // patterns are same
    otherConverter.setPatterns(otherPatterns);
    otherConverter.setTransient(true);
    doTestEquals(converter, otherConverter, false);


    // transient same, patterns same, but allowsTransparent diff
    otherConverter.setTransient(false);
    otherConverter.setTransparentAllowed(true);
    doTestEquals(converter, otherConverter, false);
  }

  public void testDefaultColorPatternWorks()
  {
    ColorConverter converter  = new ColorConverter();
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();
    String value = "#FFFFFF";
    Color expectedColor = new Color(255,255,255);
    doTestGetAsObject(converter, context, component, value, expectedColor);
  }

  /**
   * Tests that dates after the date range cause a ValidationException.
   */
  public void testStateHolderSaveRestore()
  {
    ColorConverter converter = new ColorConverter();
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();

    String[] patterns = {"#RRGGBB","RR.GG.BB"};
    converter.setPatterns(patterns);
    converter.setConvertMessageDetail("Works fine");
    ColorConverter restoreConverter = new  ColorConverter();

    doTestStateHolderSaveRestore(converter, restoreConverter,
                                 context, component);
    context.verify();
    component.verify();
  }
  /**
   * Test ColorConverte's getAsObject(FacesContext, UIComponent, String) method
   * works fine.
   */
  public void testGetAsObjectConversion()
  {
    ColorConverter converter = new ColorConverter();
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();

    String[] patterns = { "#RRGGBB",
                          "RR.GG.BB",
                          "RRGGBB",
                          "RR GG BB",
                          "rrr-ggg-bbb",
                          "rrr ggg bbb",
                          "r-g-b",
                          };

    String values[]   = { "#FF0204",
                          "FF0206",
                          "FF FF FF",
                          "0-0-0",
                          "105 105 105",
                        };

   Color[] matchColors = {  new Color(255,2,4),
                            new Color(255,2,6),
                            new Color(255,255,255),
                            new Color(0,0,0),
                            new Color(105,105,105)
                         };

    converter.setPatterns(patterns);
    for (int i = 0; i < values.length; i++)
    {
      doTestGetAsObject(converter, context, component,values[i], matchColors[i]);
    }
    context.verify();
    component.verify();
  }

  /**
   * Test Color conveters getAsString(FacesContext, UIComponent, Object) method
   * works fine
   */
  public void testGetAsString()
  {
    ColorConverter converter  = new ColorConverter();
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();

    Color[] colors = {  new Color(255,2,4),
                        new Color(255,2,6),
                        new Color(255,255,255),
                        new Color(0,0,0),
                        new Color(105,105,105),
                      };

    List patternsHoloder = new ArrayList();
    patternsHoloder.add(new String[]{"#RRGGBB", "RRGGBB"});
    patternsHoloder.add(new String[]{"RR.GG.BB", "#RRGGBB" });
    patternsHoloder.add(new String[]{"RRGGBB", "r-g-b"});
    patternsHoloder.add(new String[]{"RR GG BB", "rrr ggg bbb"});
    patternsHoloder.add(new String[]{"rrr-ggg-bbb", "rrr ggg bbb" });

    String matchValues[]   = {  "#FF0204",
                                "FF.02.06",
                                "FFFFFF",
                                "00 00 00",
                                "105-105-105",
                             };

    for (int i = 0; i < patternsHoloder.size(); i++)
    {
      String[] patterns = (String[]) patternsHoloder.get(i);
      converter.setPatterns(patterns);
      doTestGetAsString(converter, context, component,
                                    colors[i], matchValues[i] );
    }
    context.verify();
    component.verify();
  }

  /**
   * Test that try to set null value for patterns throw IllegalArgumentException
   */
  public void testNullValueForPatterns()
  {
    ColorConverter converter = new ColorConverter();
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();

    try
    {
      converter.setPatterns(null);
      fail("Expected IllegalArgumentException for null value of patterns");
    }
    catch (IllegalArgumentException ex)
    {
      // expected fine
    }
    context.verify();
    component.verify();
  }

  public void testGetAsObjectIllegalValue()
  {
    ColorConverter converter = new ColorConverter();
    MockFacesContext context  = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();
    UIViewRoot root = new UIViewRoot();
    root.setLocale(new Locale("xx","MOCK"));
    component.setupGetId("test");
    context.setupGetViewRoot(root);
    context.setupGetViewRoot(root);
    try
    {
      converter.getAsString(context, component, new Integer(1));
      fail("Expected a converter exception");
    } catch (IllegalArgumentException ex)
    {
      // expected
    }
    context.verify();
    component.verify();
  }
}
