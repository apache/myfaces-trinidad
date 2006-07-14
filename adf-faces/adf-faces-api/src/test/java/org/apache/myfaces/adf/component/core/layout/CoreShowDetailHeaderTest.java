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
package org.apache.myfaces.adf.component.core.layout;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.adf.component.UIXShowDetail;
import org.apache.myfaces.adf.component.UIXShowDetailTest;

/**
 * Unit tests for CoreShowDetailHeader.
 *
 * @author John Fallows
 */
public class CoreShowDetailHeaderTest extends UIXShowDetailTest
{
  /**
   * Creates a new CoreShowDetailHeader.
   *
   * @param testName  the unit test name
   */
  public CoreShowDetailHeaderTest(
    String testName)
  {
    super(testName);
  }
  
  public void setUp()
  {
    super.setUp();
  }
  
  public void tearDown()
  {
    super.tearDown();
  }
  
  public static Test suite()
  {
    return new TestSuite(CoreShowDetailHeaderTest.class);
  }

  protected UIXShowDetail createHideShow()
  {
    return new CoreShowDetailHeader();
  }
}
