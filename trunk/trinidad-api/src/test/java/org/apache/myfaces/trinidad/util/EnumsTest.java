/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidad.util;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.myfaces.trinidad.util.Enums.EnumParser;
import org.apache.myfaces.trinidad.util.Enums.StringProducer;

public class EnumsTest extends TestCase
{
  enum Clash
  {
    JOE("joe", "mic"),
    MICK("mick", "guitar"),
    PAUL("paul", "bass"),
    TOPPER("topper", "drums");
    
    Clash(
      String displayName,
      String instrument
      )
    {
      _displayName = displayName;
      _instrument = instrument;
    }
    
    public String displayName()
    {
      return _displayName;
    }
    
    public String instrument()
    {
      return _instrument;
    }
    
    public static Clash valueOfDisplayName(String displayName)
    {
      for (Clash clash : Clash.values())
      {
        if (clash.displayName().equals(displayName))
        {
          return clash;
        }
      }
      
      throw new IllegalArgumentException();
    }

    private final String _displayName;
    private final String _instrument;
  }

  private static class InstrumentStringProducer implements StringProducer<Clash>
  {
    @Override
    public String toString(Clash clash)
    {
      return clash.instrument();
    }
  }
  
  private static class InstrumentEnumParser implements EnumParser<Clash>
  {
    @Override
    public Clash parse(String value)
    {
      for (Clash clash : Clash.values())
      {
        if (value.equals(clash.instrument()))
        {
          return clash;
        }
      }

      throw new IllegalArgumentException();
    }
  }

  public static final Test suite()
  {
    return new TestSuite(EnumsTest.class);
  }

  public static void main(String[] args)
    throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }
  
  public EnumsTest(String string)
  {
    super(string);
  }
  
  public void testCreateStringKeyMap()
  {    
    Map<String,Clash> clashMap = Enums.createStringKeyMap(Clash.class,
                                            new InstrumentStringProducer());
    
    assertEquals(clashMap.size(), Clash.values().length);
    
    for (String key : clashMap.keySet())
    {
      assertEquals(key, clashMap.get(key).instrument());
    }
  }
  
  public void testStringToEnum()
  {
    Map<String,Clash> clashMap = Enums.createStringKeyMap(Clash.class,
                                            new InstrumentStringProducer());

    for (Clash clash : Clash.values())
    {
      assertEquals(clash, clashMap.get(clash.instrument()));
    }
  }

  public void testStringToEnumInvalidValue()
  {
    Map<String,Clash> clashMap = Enums.createStringKeyMap(Clash.class,
                                            new InstrumentStringProducer());

    try
    {
      Enums.stringToEnum(clashMap, "clarinet", Clash.class);
      fail("IllegalArgumentException expected");
    }
    catch(IllegalArgumentException e)
    {
      // this is expected
    }
  }

  public void testPatternOf()
  {
    String pattern = Enums.patternOf(Clash.class, new InstrumentStringProducer());
    assertEquals(pattern, "(mic|guitar|bass|drums)");
  }
  
  public void testParseEnumValues()
  {
    Collection<Clash> values = Enums.parseEnumValues(
                                 Arrays.asList("drums", "bass"),
                                 Clash.class,
                                 new InstrumentEnumParser(),
                                 Clash.JOE);
    
    assertEquals(values.size(), 2);
    assertTrue(values.contains(Clash.TOPPER));
    assertTrue(values.contains(Clash.PAUL));
  }
  
  public void testlParseEnumValuesInvalidValues()
  {
    String harmonica = "harmonica";

    try
    {
      Enums.parseEnumValues(Arrays.asList("drums", harmonica, "bass"),
                              Clash.class,
                              new InstrumentEnumParser(),
                              Clash.JOE);
      fail("EnumParseException expected");
    }
    catch (EnumParseException e)
    {
      assertEquals(harmonica, e.getIllegalValue());
    }
  }
  
  public void testDefaultParseEnumValues()
  {
    Collection<Clash> values = Enums.parseEnumValues(
                                 Collections.<String>emptyList(),
                                 Clash.class,
                                 new InstrumentEnumParser(),
                                 Clash.JOE);
    
    assertEquals(values.size(), 1);
    assertTrue("default value is JOE", values.contains(Clash.JOE));
  }
  
  public void testCreateDisplayNameMap()
  {
    Map<String,Clash> clashMap = Enums.createDisplayNameMap(Clash.class);
    assertEquals(clashMap.size(), Clash.values().length);
    
    for (String key : clashMap.keySet())
    {
      assertEquals(key, clashMap.get(key).displayName());
    }
  }
  
  public void testDisplayNameStringProducer()
  {
    StringProducer<Clash> producer = Enums.displayNameStringProducer(Clash.class);
    
    for (Clash clash : Clash.values())
    {
      assertEquals(clash.displayName(), producer.toString(clash));
    }
  }
  
  public void testDisplayNameEnumParser()
  {
    EnumParser<Clash> parser = Enums.displayNameEnumParser(Clash.class);
    
    for (Clash clash : Clash.values())
    {
      assertEquals(clash, parser.parse(clash.displayName()));
    }
  }
  
  public void testDisplayNameEnumParserInvalidValue()
  {
    EnumParser<Clash> parser = Enums.displayNameEnumParser(Clash.class);
    
    try
    {
      parser.parse("ian");
      fail("IllegalArgumentException expected");
    }
    catch (IllegalArgumentException e)
    {
      // this is expected
    }
  }
  
  public void testMethodNameStringProducer()
  {
    StringProducer<Clash> producer = Enums.methodNameStringProducer(Clash.class, "instrument");
    
    for (Clash clash : Clash.values())
    {
      assertEquals(clash.instrument(), producer.toString(clash));
    }
  }
  
  public void testMethodNameStringProducerInvalidName()
  {
    try
    {
      Enums.methodNameStringProducer(Clash.class, "height");
      fail("IllegalArgumentException expected");
    }
    catch (IllegalArgumentException e)
    {
      // this is expected
    }
  }

  public void testMethodNameEnumParser()
  {
    EnumParser<Clash> parser = Enums.methodNameEnumParser(Clash.class, "valueOfDisplayName");

    for (Clash clash : Clash.values())
    {
      assertEquals(clash, parser.parse(clash.displayName()));
    }
  }
  
  public void testMethodNameEnumParserInvalidValue()
  {
    EnumParser<Clash> parser = Enums.methodNameEnumParser(Clash.class, "valueOfDisplayName");

    try
    {
      parser.parse("henry");
      fail("IllegalArgumentException expected");
    }
    catch (IllegalArgumentException e)
    {
      // this is expected
    }
  }

  public void testMethodNameEnumParserInvalidName()
  {
    try
    {
      Enums.methodNameEnumParser(Clash.class, "valueOfHeight");
      fail("IllegalArgumentException expected");
    }
    catch (IllegalArgumentException e)
    {
      // this is expected
    }
  }
}
