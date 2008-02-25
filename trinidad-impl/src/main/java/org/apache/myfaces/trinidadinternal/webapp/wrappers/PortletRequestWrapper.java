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
package org.apache.myfaces.trinidadinternal.webapp.wrappers;

import java.security.Principal;
import java.util.Enumeration;
import java.util.Locale;
import java.util.Map;

import javax.portlet.PortalContext;
import javax.portlet.PortletMode;
import javax.portlet.PortletPreferences;
import javax.portlet.PortletRequest;
import javax.portlet.PortletSession;
import javax.portlet.WindowState;

/**
 * TODO: Document this
 *
 * @version $Revision$ $Date$
 */

public class PortletRequestWrapper implements PortletRequest
{
  public PortletRequestWrapper(PortletRequest request)
  {
    _req = request;
  }

  private PortletRequest _req;

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletRequest#getAttribute(java.lang.String)
   */
  public Object getAttribute(String arg0)
  {
    return _req.getAttribute(arg0);
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getAttributeNames()
   * -= Simon =-
   * TODO: Once portlet spec supports generics, change this signature to match it.
   */
  @SuppressWarnings("unchecked")
  public Enumeration getAttributeNames()
  {
    return _req.getAttributeNames();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getAuthType()
   */
  public String getAuthType()
  {
    return _req.getAuthType();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getContextPath()
   */
  public String getContextPath()
  {
    return _req.getContextPath();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getLocale()
   */
  public Locale getLocale()
  {
    return _req.getLocale();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getLocales()
   * -= Simon =-
   * TODO: Once portlet spec supports generics, change this signature to match it.
   */
  @SuppressWarnings("unchecked")
  public Enumeration getLocales()
  {
    return _req.getLocales();
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletRequest#getParameter(java.lang.String)
   */
  public String getParameter(String arg0)
  {
    return _req.getParameter(arg0);
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getParameterMap()
   * -= Simon =-
   * TODO: Once portlet spec supports generics, change this signature to match it.
   */
  @SuppressWarnings("unchecked")
  public Map getParameterMap()
  {
    return _req.getParameterMap();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getParameterNames()
   * -= Simon =-
   * TODO: Once portlet spec supports generics, change this signature to match it.
   */
  @SuppressWarnings("unchecked")
  public Enumeration getParameterNames()
  {
    return _req.getParameterNames();
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletRequest#getParameterValues(java.lang.String)
   */
  public String[] getParameterValues(String arg0)
  {
    return _req.getParameterValues(arg0);
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getPortalContext()
   */
  public PortalContext getPortalContext()
  {
    return _req.getPortalContext();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getPortletMode()
   */
  public PortletMode getPortletMode()
  {
    return _req.getPortletMode();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getPortletSession()
   */
  public PortletSession getPortletSession()
  {
    return _req.getPortletSession();
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletRequest#getPortletSession(boolean)
   */
  public PortletSession getPortletSession(boolean arg0)
  {
    return _req.getPortletSession(arg0);
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getPreferences()
   */
  public PortletPreferences getPreferences()
  {
    return _req.getPreferences();
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletRequest#getProperties(java.lang.String)
   * -= Simon =-
   * TODO: Once portlet spec supports generics, change this signature to match it.
   */
  @SuppressWarnings("unchecked")
  public Enumeration getProperties(String arg0)
  {
    return _req.getProperties(arg0);
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletRequest#getProperty(java.lang.String)
   */
  public String getProperty(String arg0)
  {
    return _req.getProperty(arg0);
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getPropertyNames()
   * -= Simon =-
   * TODO: Once portlet spec supports generics, change this signature to match it.
   */
  @SuppressWarnings("unchecked")
  public Enumeration getPropertyNames()
  {
    return _req.getPropertyNames();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getRemoteUser()
   */
  public String getRemoteUser()
  {
    return _req.getRemoteUser();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getRequestedSessionId()
   */
  public String getRequestedSessionId()
  {
    return _req.getRequestedSessionId();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getResponseContentType()
   */
  public String getResponseContentType()
  {
    return _req.getResponseContentType();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getResponseContentTypes()
   * -= Simon =-
   * TODO: Once portlet spec supports generics, change this signature to match it.
   */
  @SuppressWarnings("unchecked")
  public Enumeration getResponseContentTypes()
  {
    return _req.getResponseContentTypes();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getScheme()
   */
  public String getScheme()
  {
    return _req.getScheme();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getServerName()
   */
  public String getServerName()
  {
    return _req.getServerName();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getServerPort()
   */
  public int getServerPort()
  {
    return _req.getServerPort();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getUserPrincipal()
   */
  public Principal getUserPrincipal()
  {
    return _req.getUserPrincipal();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#getWindowState()
   */
  public WindowState getWindowState()
  {
    return _req.getWindowState();
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletRequest#isPortletModeAllowed(javax.portlet.PortletMode)
   */
  public boolean isPortletModeAllowed(PortletMode arg0)
  {
    return _req.isPortletModeAllowed(arg0);
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#isRequestedSessionIdValid()
   */
  public boolean isRequestedSessionIdValid()
  {
    return _req.isRequestedSessionIdValid();
  }

  /**
   * @return
   * @see javax.portlet.PortletRequest#isSecure()
   */
  public boolean isSecure()
  {
    return _req.isSecure();
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletRequest#isUserInRole(java.lang.String)
   */
  public boolean isUserInRole(String arg0)
  {
    return _req.isUserInRole(arg0);
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletRequest#isWindowStateAllowed(javax.portlet.WindowState)
   */
  public boolean isWindowStateAllowed(WindowState arg0)
  {
    return _req.isWindowStateAllowed(arg0);
  }

  /**
   * @param arg0
   * @see javax.portlet.PortletRequest#removeAttribute(java.lang.String)
   */
  public void removeAttribute(String arg0)
  {
    _req.removeAttribute(arg0);
  }

  /**
   * @param arg0
   * @param arg1
   * @see javax.portlet.PortletRequest#setAttribute(java.lang.String, java.lang.Object)
   */
  public void setAttribute(String arg0, Object arg1)
  {
    _req.setAttribute(arg0, arg1);
  }

  public PortletRequest getRequest()
  {
    return _req;
  }
}
