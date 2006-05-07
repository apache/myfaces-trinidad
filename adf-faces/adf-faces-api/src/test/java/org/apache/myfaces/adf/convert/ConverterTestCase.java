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

import java.util.Map;
import java.util.TimeZone;
import javax.faces.component.StateHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import junit.framework.TestCase;

import org.apache.myfaces.adf.change.ChangeManager;
import org.apache.myfaces.adf.config.RegionManager;
import org.apache.myfaces.adf.context.AdfFacesContext;
import org.apache.myfaces.adf.context.Agent;
import org.apache.myfaces.adf.context.PageResolver;
import org.apache.myfaces.adf.context.PageFlowScopeProvider;
import org.apache.myfaces.adf.context.DialogService;
import org.apache.myfaces.adf.webapp.UploadedFileProcessor;
import org.apache.myfaces.adf.event.ReturnEvent;

import javax.faces.component.MockUIComponent;
import javax.faces.context.MockFacesContext;

/**
 * Base class for unit tests of Converters
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/test/java/oracle/adf/view/faces/convert/ConverterTestCase.java#0 $) $Date: 17-oct-2005.16:28:54 $
 * @author vijay venkatarman (vijay.venkataraman@oracle.com)
 *
 */
public abstract class ConverterTestCase extends TestCase
{
  public ConverterTestCase(String testName)
  {
    super(testName);
  }

  /**
   * This test performs action on the method
   * javax.faces.convert.Converter.getAsObject(FacesContext, UIComponent, String)
   * and
   * javax.faces.convert.Converter.getAsString(FacesContext, UIComponent, Object)
   * for method getAsObject() should return a value of null while getAsString()
   * should return a empty string.
   * @throws ValidatorException  when test fails
   */
  protected void doTestNull(
    MockFacesContext context,
    MockUIComponent component,
    Converter converter
    ) throws ConverterException
  {
    Object obj = converter.getAsObject(context, component, null);
    assertEquals(null, obj);
    String str = converter.getAsString(context, component, null);
    assertEquals("",str);
    context.verify();
    component.verify();
  }

  /**
   * If contex or component = null then should throw NullPointerException
   */
  protected void doTestNullContext(
    MockUIComponent component,
    Converter converter) throws NullPointerException
  {
    try
    {
      converter.getAsObject(null, component , "dummy");
      fail("Expected NullpointerException - if context or component is null");
    }
    catch (NullPointerException npe)
    {
      // this is expected
    }
    try
    {
      converter.getAsString(null, component , "dummy");
      fail("Expected NullpointerException - if context or component is null");
    }
    catch (NullPointerException npe)
    {
      // this is expected
    }
  }

  /**
   * If contex or component = null then should throw NullPointerException
   */
  protected void doTestNullComponent(MockFacesContext context,
    Converter converter ) throws NullPointerException
  {
    try
    {
      converter.getAsObject(context, null, "dummy");
      fail("Expected NullpointerException - if context or component is null");
    }
    catch (NullPointerException npe)
    {
      // this is expected
    }
    try
    {
      converter.getAsString(context, null, "dummy");
      fail("Expected NullpointerException - if context or component is null");
    }
    catch (NullPointerException npe)
    {
      // this is expected
    }
  }

  protected void doTestBlankValue(Converter converter)
  {
    MockFacesContext context = new MockFacesContext();
    MockUIComponent component = new MockUIComponent();
    Object value = converter.getAsObject(context, component,"");
    assertEquals(null, value);
  }

  /**
   * Test the validity of method
   * javax.faces.convert.Converter.getAsObject(FacesContext, UIComponent, String)
   *
   * @param converter converter which is to be tested
   * @param context MockFaces context
   * @param component MockFaces component
   * @throws javax.faces.convert.ConvertException
   */
  protected void doTestGetAsObject(
    Converter converter,
    MockFacesContext context,
    MockUIComponent component,
    String value,
    Object expectedValue
    )  throws ConverterException
  {
    Object conv = converter.getAsObject(context, component, value);
    assertEquals(expectedValue, conv);

    context.verify();
    component.verify();
  }


  /**
   * Test the validity of call on the method
   * javax.faces.convert.Converter.getAsString(FacesContext, UIComponent, Object)
   * @param converter converter which is to be tested
   * @param context MockFaces context
   * @param component MockFaces component
   * @throws javax.faces.convert.ConvertException
   */
  protected void doTestGetAsString(
    Converter converter,
    MockFacesContext context,
    MockUIComponent component,
    Object value,
    String expectedValue
    )  throws ConverterException
  {
    Object conv = converter.getAsString(context, component, value);
    assertEquals(conv, expectedValue);
    context.verify();
    component.verify();
  }


  /**
   * Test for equality or mismatch of converters
   * Converter's that do not  implement equals should override to check
   * for it equality.
   * @param thisConverter
   * @param otherConverter
   * @param isEqual - Identifies whether the comparison for equality of
   *        converters or mismatch of converters
   */
  protected void doTestEquals(
    Converter thisConverter,
    Converter otherConverter,
    boolean isEqual)
  {
    assertEquals(isEqual, thisConverter.equals(otherConverter));
    assertEquals(isEqual, (thisConverter.hashCode() == otherConverter.hashCode()));
  }

  /**
   * Test to check for Validators which implements the StateHolder interface
   * @param thisValidator  Source converter
   * @param otherValidator The converter in which the state will be restored to
   * @param context MockFaces context
   * @param component MockFaces Component
   */
  protected void doTestStateHolderSaveRestore(
    Converter thisConverter,
    Converter otherConverter,
    MockFacesContext context,
    MockUIComponent component
    )
  {
    Object state = ((StateHolder)thisConverter).saveState(context);

    ((StateHolder)otherConverter).restoreState(context, state);
    // do all actions of save and restore
    doTestEquals(thisConverter, otherConverter, true);
    context.verify();
    component.verify();
  }


  /**
   * Compares two object relying on its equality.
   * If two objects are null returns true
   * @param o1
   * @param o2
   * @return
   */
  protected boolean equals(
    Object o1,
    Object o2
    )
  {
    return ( o1 == o2 || (o1 != null && o1.equals(o2)));
  }

  protected AdfFacesContext getCustomMockAdfFacesContext(
  AdfFacesContext mockAdfFacesContext)
  {
    return new CustomMockAdfFacesContext(mockAdfFacesContext);
  }
}

class CustomMockAdfFacesContext extends  AdfFacesContext
{
  private AdfFacesContext _adfCtx;

  CustomMockAdfFacesContext(AdfFacesContext mockAdfFacesContext)
  {
    _adfCtx = mockAdfFacesContext;
    super.attach();
  }

  public void launchDialog(
    UIViewRoot  dialogRoot,
    Map         dialogParameters,
    UIComponent source,
    boolean     useWindow,
    Map         windowProperties)
  {
    _adfCtx.launchDialog(dialogRoot, dialogParameters, source, useWindow, windowProperties);
  }


  public PageResolver getPageResolver()
  {
    return _adfCtx.getPageResolver();
  }

  public PageFlowScopeProvider getPageFlowScopeProvider()
  {
    return _adfCtx.getPageFlowScopeProvider();
  }

  public UploadedFileProcessor getUploadedFileProcessor()
  {
    return _adfCtx.getUploadedFileProcessor();
  }

  public boolean isDebugOutput()
  {
    return _adfCtx.isDebugOutput();
  }

  public String getOutputMode()
  {
    return _adfCtx.getOutputMode();
  }

  public boolean isPostback()
  {
    return _adfCtx.isPostback();
  }

  public boolean isClientValidationDisabled()
  {
    return _adfCtx.isClientValidationDisabled();
  }

  public String getSkinFamily()
  {
    return _adfCtx.getSkinFamily();
  }

  public String getAccessibilityMode()
  {
    return _adfCtx.getAccessibilityMode();
  }

  public boolean isRightToLeft()
  {
    return _adfCtx.isRightToLeft();
  }

  public char getNumberGroupingSeparator()
  {
    return _adfCtx.getNumberGroupingSeparator();
  }

  public char getDecimalSeparator()
  {
    return _adfCtx.getDecimalSeparator();
  }

  public String getCurrencyCode()
  {
    return _adfCtx.getCurrencyCode();
  }

  public int getTwoDigitYearStart()
  {
    return _adfCtx.getTwoDigitYearStart();
  }

  public String getOracleHelpServletUrl()
  {
    return _adfCtx.getOracleHelpServletUrl();
  }

  public Map getHelpTopic()
  {
    return _adfCtx.getHelpTopic();
  }

  public Map getHelpSystem()
  {
    return _adfCtx.getHelpSystem();
  }

  public TimeZone getTimeZone()
  {
    return _adfCtx.getTimeZone();
  }

  public void addPartialTarget(UIComponent newTarget)
  {
    _adfCtx.addPartialTarget(newTarget);
  }

  public void addPartialTriggerListeners(UIComponent listener,
                                         String[] trigger)
  {
    _adfCtx.addPartialTriggerListeners(listener, trigger);
  }

  public void partialUpdateNotify(UIComponent updated)
  {
    _adfCtx.partialUpdateNotify(updated);
  }

  public Map getColorPalette()
  {
    return _adfCtx.getColorPalette();
  }

  public Map getFormatter()
  {
    return _adfCtx.getFormatter();
  }

  public Agent getAgent()
  {
    return _adfCtx.getAgent();
  }

  public DialogService getDialogService()
  {
    return _adfCtx.getDialogService();
  }

  public void returnFromDialog(Object returnValue, Map returnParam)
  {
    _adfCtx.returnFromDialog(returnValue, returnParam);
  }
  public Map getPageFlowScope()
  {
    return _adfCtx.getPageFlowScope();
  }

  public ChangeManager getChangeManager()
  {
    return _adfCtx.getChangeManager();
  }

  public RegionManager getRegionManager()
  {
    return _adfCtx.getRegionManager();
  }

}
