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

package org.apache.myfaces.adfinternal.validator;
import java.util.Locale;

import javax.faces.component.UIViewRoot;

import junit.framework.TestCase;

import javax.faces.component.MockUIComponent;
import javax.faces.context.MockFacesContext;

import org.apache.myfaces.adfbuild.test.MockUtils;

/**
 * Tests for ByteLenghtValidator which renderes the client side scripts.
 * the ByteLengthValidator class.
 * @author Vijay Venkataraman (vijay.venkataraman@oracle.com)
 */
public class ByteLengthValidatorTest extends TestCase
{
  public ByteLengthValidatorTest(String name)
  {
    super(name);
  }

  public void testPickedUpClientByteLengthValidator()
  {
    _doTestPickedUpClientByteLengthValidator("ISO-8859-1","SBFormat");
    _doTestPickedUpClientByteLengthValidator("ms_kanji","CjkFormat");
    _doTestPickedUpClientByteLengthValidator("UTF-8","Utf8Format");
  }

  private void _doTestPickedUpClientByteLengthValidator(
    String encoding,
    String expectedConstructorName
    )
  {
    MockFacesContext context = new MockFacesContext();
    MockUIComponent component = MockUtils.buildMockUIComponent();

    UIViewRoot uiRoot = new UIViewRoot();
    uiRoot.setLocale(Locale.US);
    for (int i = 0; i < 4; i++)
      context.setupGetViewRoot(uiRoot);

    ByteLengthValidator blv = new ByteLengthValidator();
    blv.setEncoding(encoding);

    String libKey = blv.getLibKey(context, component);
    String constructorInfo = blv.getClientValidation(context, component);
    String clientScript = blv.getClientScript(context, component);

    assertEquals(null, clientScript);
    assertEquals(true, libKey.equals(expectedConstructorName + "()"));
    assertEquals(true, constructorInfo.startsWith("new "
                                                  + expectedConstructorName));

    context.verify();
    component.verify();
  }

  /**
   * @todo Do we have any way to check whether for the given
   * ByteLenght.VALIDATOR_ID if the expected client side ByteLengthValidator
   * is picked.
   */
}
