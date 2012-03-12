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
package org.apache.myfaces.trinidad.context;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class VersionTest extends TestCase
{
  public static final Test suite()
  {
    return new TestSuite(VersionTest.class);
  }

  public static void main(String[] args)
    throws Throwable
  {
    junit.textui.TestRunner.run(suite());
  }
  public VersionTest(String string)
  {
    super(string);
  }

  public void testCompareTo()
  {
    Version v9 = new Version("9");
    Version v10 = new Version("10");
    
    assertTrue("v9 < v10", v9.compareTo(v10) < 0);
    assertTrue("v9 == v9", v9.compareTo(new Version("9")) == 0);
  }
  
  public void testComapreToWildcard()
  {
    Version v9x = new Version("9.*");
    Version v91 = new Version("9.1");
    Version v8 = new Version("8");
    Version v10 = new Version("10");
    
    assertTrue("v9.* == v9.1", v9x.compareTo(v91) == 0);
    assertTrue("v9.1 == v9.*", v91.compareTo(v9x) == 0);
    assertTrue("v9.* > v8", v9x.compareTo(v8) > 0);
    assertTrue("v8 < v9.*", v8.compareTo(v9x) < 0);    
    assertTrue("v9.* < v10", v9x.compareTo(v10) < 0);
    assertTrue("v10 > v9.*", v10.compareTo(v9x) > 0);
  }
  
  public void testMinimumVersion()
  {
    Version v9 = new Version("9");
    Version v9x = new Version("9.*");

    assertTrue("trailing wildcard removal", v9x.toMinimumVersion().equals(v9));

    Version v900 = new Version("9.0.0");
    Version v9x0 = new Version("9.*.0");

    assertTrue("interior wildcard removal", v9x0.toMinimumVersion().equals(v900));
    
    Version v9padded = new Version("9", "*");
    assertTrue("wildcard padding match", v9padded.compareTo(v900) == 0);
    
    Version v9paddedMin = v9padded.toMinimumVersion();
    assertTrue("wildcard padding removed", v9paddedMin.compareTo(v900) != 0);
    assertTrue("wildcard padding removed equality ", v9paddedMin.equals(v9));
  }
  
  public void testMaximumVersion()
  {
    String intMax = Integer.toString(Integer.MAX_VALUE);
  
    Version v9max = new Version("9." + intMax, intMax);
    Version v9x = new Version("9.*");

    assertTrue("trailing wildcard removal", v9x.toMaximumVersion().equals(v9max));

    Version v9max0 = new Version("9." + intMax + ".0", intMax);
    Version v9x0 = new Version("9.*.0");

    assertTrue("interior wildcard removal", v9x0.toMaximumVersion().equals(v9max0));
    
    Version v9padded = new Version("9", "*");
    Version v9paddedMax = v9padded.toMaximumVersion();    
    assertTrue("wildcard padding match", v9padded.compareTo(v9max0) == 0);
    assertTrue("wildcard padding removed non-match", v9paddedMax.compareTo(v9max0) != 0);
    assertTrue("wildcard padding removed equality ", v9paddedMax.equals(new Version("9", intMax)));
  }
}
