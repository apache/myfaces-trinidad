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
package org.apache.myfaces.adfinternal.renderkit;

import java.util.Map;
import java.util.TimeZone;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import org.apache.myfaces.adf.change.ChangeManager;
import org.apache.myfaces.adf.config.RegionManager;
import org.apache.myfaces.adf.context.AdfFacesContext;
import org.apache.myfaces.adf.context.Agent;
import org.apache.myfaces.adf.context.PageResolver;
import org.apache.myfaces.adf.context.PageFlowScopeProvider;
import org.apache.myfaces.adf.context.DialogService;
import org.apache.myfaces.adf.event.ReturnEvent;
import org.apache.myfaces.adf.webapp.UploadedFileProcessor;

import org.apache.myfaces.adfinternal.context.PageFlowScopeProviderImpl;
import org.apache.myfaces.adfinternal.context.PageResolverDefaultImpl;

public class MAdfFacesContext extends AdfFacesContext
{
  public MAdfFacesContext()
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
    return PageResolverDefaultImpl.sharedInstance();
  }

  public PageFlowScopeProvider getPageFlowScopeProvider()
  {
    return PageFlowScopeProviderImpl.sharedInstance();
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
    return ',';
  }

  public char getDecimalSeparator()
  {
    return '.';
  }

  public String getCurrencyCode()
  {
    return null;
  }

  public int getTwoDigitYearStart()
  {
    return 1950;
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
    return _FIXED_TIME_ZONE;
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

  private String _skin;
  private String _accMode;
  private Agent _agent;
  private boolean _rtl = false;
  static private TimeZone _FIXED_TIME_ZONE =
    TimeZone.getTimeZone("America/Los_Angeles");
}
