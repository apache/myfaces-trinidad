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
package org.apache.myfaces.adfinternal.uinode.nav;

import junit.framework.TestCase;

import javax.faces.context.MockExternalContext;
import javax.faces.context.MockFacesContext;

import org.apache.myfaces.adfinternal.ui.UIConstants;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.FacesBeanFactory;
import org.apache.myfaces.adf.component.UIXComponent;
import org.apache.myfaces.adf.component.core.nav.CoreGoButton;

//import org.apache.myfaces.adfinternal.MockUIXComponent;
import org.apache.myfaces.adfinternal.uinode.UINodeFacesBean;
import org.apache.myfaces.adfinternal.uinode.UINodeFacesBeanTestCase;
import org.apache.myfaces.adfinternal.uinode.UINodePropertyKey;

/**
 * Unit tests for GoButtonFacesBean.
 *
 * @author Adam Winer
 * @author John Fallows
 */
public class GoButtonFacesBeanTest extends UINodeFacesBeanTestCase
{
  /**
   * Creates a new GoButtonFacesBeanTest.
   *
   * @param testName  the unit test name
   */
  public GoButtonFacesBeanTest(
    String testName)
  {
    super(testName);
  }


  /**
   * Tests that UINode property keys correctly update UINode attributes.
   */
  public void testDestination()
  {
    CoreGoButton component = new CoreGoButton();

    UINodeFacesBean fb = createUINodeFacesBean(component,
                                               CoreGoButton.TYPE);
    fb.setProperty(CoreGoButton.DESTINATION_KEY, "someURL");
    // Verify the destination attr is set (non-context-relative)
    assertEquals(fb.getUINode().getAttributeValue(
                                       null, UIConstants.DESTINATION_ATTR),
                 "someURL");

    MockExternalContext external = new MockExternalContext();
    MockFacesContext context = new MockFacesContext();
    setCurrentContext(context);

    context.setupGetExternalContext(external);
    external.setupGetRequestContextPath("/foo");

    fb.setProperty(CoreGoButton.DESTINATION_KEY, "/someURL");
    // Verify a context-relative URL gets recorded correctly
    assertEquals(fb.getUINode().getAttributeValue(
                                       null, UIConstants.DESTINATION_ATTR),
                 "/foo/someURL");

    setCurrentContext(null);
  }
}
