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
package org.apache.myfaces.trinidad.context;

import java.util.Map;
import java.util.TimeZone;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.config.RegionManager;
import org.apache.myfaces.trinidad.context.AdfFacesContext;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.DialogService;
import org.apache.myfaces.trinidad.context.PageFlowScopeProvider;
import org.apache.myfaces.trinidad.context.PageResolver;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;


public class MockAdfFacesContext extends AdfFacesContext
{
  public MockAdfFacesContext()
  {
    attach();
  }

  // Support setting the agent so we can create one AdfFacesContext
  // and mutate it

  public void setAgent(Agent agent)
  {
    _agent = agent;
  }

  public Agent getAgent()
  {
    return _agent;
  }

  public PageResolver getPageResolver()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public PageFlowScopeProvider getPageFlowScopeProvider()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }


  public DialogService getDialogService()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public Map getPageFlowScope()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public void returnFromDialog(Object returnValue, Map returnParam)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public void launchDialog(UIViewRoot dialogRoot, Map dialogParameters, UIComponent source, boolean useWindow, Map windowProperties)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public boolean isPostback()
  {
    return false;
  }
  
  @Override
  public boolean isPartialRequest(FacesContext context)
  {
    return false;
  }

  public boolean isDebugOutput()
  {
    return false;
  }

  public boolean isClientValidationDisabled()
  {
    return false;
  }

  public String getOutputMode()
  {
    return null;
  }

  public void setSkinFamily(String skin)
  {
    _skin = skin;
  }

  public String getSkinFamily()
  {
    return _skin;
  }

  public String getAccessibilityMode()
  {
    return _accMode;
  }

  public void setAccessibilityMode(String accMode)
  {
    _accMode = accMode;
  }

  public boolean isRightToLeft()
  {
    return _rtl;
  }

  public void setRightToLeft(boolean rtl)
  {
    _rtl = rtl;
  }

  public char getNumberGroupingSeparator()
  {
    return _numberGroupingSeparator;
  }

  public void setNumberGroupingSeparator(char sep)
  {
    _numberGroupingSeparator = sep;
  }

  public char getDecimalSeparator()
  {
    return _decimalSeparator;
  }

  public void setDecimalSeparator(char sep)
  {
    _decimalSeparator = sep;
  }

  public String getCurrencyCode()
  {
    return _currencyCode;
  }

  public void setCurrencyCode(String code)
  {
    _currencyCode = code;
  }

  public int getTwoDigitYearStart()
  {
    return _twoDigitYearStart;
  }

  public void setTwoDigitYearStart(int start)
  {
    _twoDigitYearStart = start;
  }

  public String getOracleHelpServletUrl()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public Map getHelpTopic()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public Map getHelpSystem()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public TimeZone getTimeZone()
  {
    return _timeZone;
  }

  public void setTimeZone(TimeZone timeZone)
  {
    _timeZone = timeZone;
  }

  public void addPartialTarget(UIComponent newTarget)
  {
    // throw new UnsupportedOperationException("Not implemented yet");
  }

  public void addPartialTriggerListeners(UIComponent listener, String[] trigger)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public void partialUpdateNotify(UIComponent updated)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public UploadedFileProcessor getUploadedFileProcessor()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public Map getColorPalette()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public Map getFormatter()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public ChangeManager getChangeManager()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public RegionManager getRegionManager()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  static private final TimeZone _FIXED_TIME_ZONE =
    TimeZone.getTimeZone("America/Los_Angeles");

  private char _numberGroupingSeparator = ',';
  private char _decimalSeparator = '.';
  private String _currencyCode = null;
  private int _twoDigitYearStart = 1950;
  private TimeZone _timeZone = _FIXED_TIME_ZONE;

  private String _skin;
  private String _accMode;
  private Agent _agent;
  private boolean _rtl = false;
  
}