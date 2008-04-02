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
package org.apache.myfaces.trinidadinternal.renderkit;

import java.awt.Color;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.config.RegionManager;
import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.PageResolver;
import org.apache.myfaces.trinidad.context.PageFlowScopeProvider;
import org.apache.myfaces.trinidad.context.DialogService;

import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;

import org.apache.myfaces.trinidadinternal.context.PageFlowScopeProviderImpl;
import org.apache.myfaces.trinidadinternal.context.PageResolverDefaultImpl;

public class MRequestContext extends RequestContext
{
  public MRequestContext()
  {
    attach();
  }

  // Support setting the agent so we can create one RequestContext
  // and mutate it

  public void setAgent(Agent agent)
  {
    _agent = agent;
  }

  @Override
  public Agent getAgent()
  {
    return _agent;
  }

  @Override
  public PageResolver getPageResolver()
  {
    return PageResolverDefaultImpl.sharedInstance();
  }

  @Override
  public PageFlowScopeProvider getPageFlowScopeProvider()
  {
    return PageFlowScopeProviderImpl.sharedInstance();
  }

  @Override
  public DialogService getDialogService()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Map<String, Object> getPageFlowScope()
  {
    return Collections.emptyMap();
  }

  @Override
  public void returnFromDialog(
      Object returnValue, 
      Map<Object, Object> returnParam)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public void launchDialog(
      UIViewRoot dialogRoot, 
      Map<String, Object> dialogParameters, 
      UIComponent source, 
      boolean useWindow, 
      Map<String, Object> windowProperties)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public boolean isPostback()
  {
    return false;
  }
  
  @Override
  public boolean isPartialRequest(FacesContext context)
  {
    return false;
  }

  @Override
  public boolean isDebugOutput()
  {
    return false;
  }

  @Override
  public boolean isClientValidationDisabled()
  {
    return false;
  }

  @Override
  public String getOutputMode()
  {
    return null;
  }

  public void setSkinFamily(String skin)
  {
    _skin = skin;
  }

  @Override
  public String getSkinFamily()
  {
    return _skin;
  }

  @Override
  public Accessibility getAccessibilityMode()
  {
    return _accMode;
  }

  public void setAccessibilityMode(Accessibility accMode)
  {
    _accMode = accMode;
  }

  @Override
  public AccessibilityProfile getAccessibilityProfile()
  {
    return _accProfile;
  }

  public void setAccessibilityProfile(AccessibilityProfile accProfile)
  {
    _accProfile = accProfile;
  }

  @Override
  public ClientValidation getClientValidation()
  {
    return _clientValidation;
  }

  public void setClientValidation(ClientValidation clientValidation)
  {
    _clientValidation = clientValidation;
  }

  @Override
  public boolean isRightToLeft()
  {
    return _rtl;
  }

  public void setRightToLeft(boolean rtl)
  {
    _rtl = rtl;
  }

  public void setAnimationEnabled(boolean animationEnabled)
  {
    _animationEnabled = animationEnabled;
  }

   @Override
  public boolean isAnimationEnabled()
  {
    return _animationEnabled;
  }

  @Override
  public Locale getFormattingLocale()
  {
    return null;
  }

  @Override
  public char getNumberGroupingSeparator()
  {
    return ',';
  }

  @Override
  public char getDecimalSeparator()
  {
    return '.';
  }

  @Override
  public String getCurrencyCode()
  {
    return null;
  }

  @Override
  public int getTwoDigitYearStart()
  {
    return 1950;
  }

  @Override
  public String getOracleHelpServletUrl()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Map<String, Object> getHelpTopic()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Map<String, Object> getHelpSystem()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public TimeZone getTimeZone()
  {
    return _FIXED_TIME_ZONE;
  }

  @Override
  public void addPartialTarget(UIComponent newTarget)
  {
    // throw new UnsupportedOperationException("Not implemented yet");
  }
  
  /**
   * @see org.apache.myfaces.trinidad.context.RequestContext#addPartialTargets(javax.faces.component.UIComponent, java.lang.String[])
   */
  @Override
  public void addPartialTargets(UIComponent from, String... targets)
  {
    
  }

  @Override
  public void addPartialTriggerListeners(UIComponent listener, String[] trigger)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public void partialUpdateNotify(UIComponent updated)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public UploadedFileProcessor getUploadedFileProcessor()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  @Override
  public Map<String, List<Color>> getColorPalette()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Map<Object, Map<Object,String>> getFormatter()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public ChangeManager getChangeManager()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public RegionManager getRegionManager()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  @Override
  public Object saveComponent(UIComponent component)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }


  @Override
  public UIComponent restoreComponent(Object state)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  private String _skin;
  private Accessibility _accMode;
  private AccessibilityProfile _accProfile;
  private ClientValidation _clientValidation = ClientValidation.ALERT;
  private Agent _agent;
  private boolean _rtl = false;
  private boolean _animationEnabled = true;
  static private TimeZone _FIXED_TIME_ZONE =
    TimeZone.getTimeZone("America/Los_Angeles");
  
}
