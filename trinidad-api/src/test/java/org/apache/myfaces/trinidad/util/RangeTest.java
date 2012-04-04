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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class RangeTest extends TestCase
{
  public static final Test suite()
  {
    return new TestSuite(RangeTest.class);
  }

  public static void main(String[] args)
    throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }
  public RangeTest(String string)
  {
    super(string);
  }

  public void testOf()
  {
    String start = "a";
    String end = "z";
    
    Range<String> range = Range.of(start, end);
    assertEquals(start, range.getStart());
    assertEquals(end, range.getEnd());
  }

  public void testOfNullStart()
  {
    try
    {
      Range.of(null, "z");
      fail("IllegalArgumentException expected");      
    }
    catch (IllegalArgumentException e)
    {
      // this is expected
    }
  }

  public void testOfNullEnd()
  {
    try
    {
      Range.of("a", null);
      fail("IllegalArgumentException expected");      
    }
    catch (IllegalArgumentException e)
    {
      // this is expected
    }
  }
  
  public void testOfStartGreaterThanEnd()
  {
    assertTrue(Range.of("z", "a").isEmpty());
  }

  public void testIntersect()
  {
    Range<String> r1  = Range.of("a", "m");
    Range<String> r2 = Range.of("l", "z");
    Range<String> intersection = r1.intersect(r2);
    
    assertEquals(intersection.getStart(), r2.getStart());
    assertEquals(intersection.getEnd(), r1.getEnd());    
  }

  public void testEmptyIntersect()
  {
    Range<String> r1  = Range.of("a", "l");
    Range<String> r2 = Range.of("m", "z");
    
    assertTrue(r1.intersect(r2).isEmpty());
  }
  
  public void testSelfIntersect()
  {
    Range r1 = Range.of("a", "c");
    
    assertEquals(r1.intersect(r1), r1);
  }
  
  public void testContains()
  {
    String start = "b";
    String end = "d";
    String middle = "c";    
    String before = "a";
    String after = "e";
    
    Range<String> r1 = Range.of(start, end);
    assertTrue(r1.contains(start));
    assertTrue(r1.contains(middle));
    assertTrue(r1.contains(end));
    assertFalse(r1.contains(before));
    assertFalse(r1.contains(after));    
  }
}
