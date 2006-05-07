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
package org.apache.myfaces.adf.component.core.input;

import org.apache.myfaces.adf.component.UIXEditableValue;
import org.apache.myfaces.adf.component.UIXEditableValueTestCase;

/**
 * Unit tests for CoreInputHidden.
 *
 * @author Adam Winer
 * @author John Fallows
 */
public class CoreInputHiddenTest extends UIXEditableValueTestCase
{
  /**
   * Creates a new UIXInputHiddenTest.
   *
   * @param testName  the unit test name
   */
  public CoreInputHiddenTest(
    String testName)
  {
    super(testName);
  }

  protected final UIXEditableValue createEditableValue()
  {
    return new CoreInputHidden();
  }
}
