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

import java.io.IOException;
import java.util.Map;

import javax.portlet.ActionResponse;
import javax.portlet.PortletMode;
import javax.portlet.PortletModeException;
import javax.portlet.WindowState;
import javax.portlet.WindowStateException;

/**
 * TODO: Document this
 *
 * @version $Revision$ $Date$
 */

public class ActionResponseWrapper extends PortletResponseWrapper implements ActionResponse
{
  public ActionResponseWrapper(ActionResponse response)
  {
    super(response);
    _resp = response;
  }

  private ActionResponse _resp;

  /* (non-Javadoc)
   * @see javax.portlet.ActionResponse#sendRedirect(java.lang.String)
   */
  public void sendRedirect(String arg0) throws IOException
  {
    _resp.sendRedirect(arg0);
  }

  /* (non-Javadoc)
   * @see javax.portlet.ActionResponse#setPortletMode(javax.portlet.PortletMode)
   */
  public void setPortletMode(PortletMode arg0) throws PortletModeException
  {
    _resp.setPortletMode(arg0);
  }

  /* (non-Javadoc)
   * @see javax.portlet.ActionResponse#setRenderParameter(java.lang.String, java.lang.String)
   */
  public void setRenderParameter(String arg0, String arg1)
  {
    _resp.setRenderParameter(arg0, arg1);
  }

  /* (non-Javadoc)
   * @see javax.portlet.ActionResponse#setRenderParameter(java.lang.String, java.lang.String[])
   */
  public void setRenderParameter(String arg0, String[] arg1)
  {
    _resp.setRenderParameter(arg0, arg1);
  }

  /* (non-Javadoc)
   * @see javax.portlet.ActionResponse#setRenderParameters(java.util.Map)
   * -= Simon =-
   * TODO: When portlet is made generic, fix this signature to fit with 
   *       ActionResponse.setRenderParameters
   */
  @SuppressWarnings("unchecked")
  public void setRenderParameters(Map arg0)
  {
    _resp.setRenderParameters(arg0);
  }

  /* (non-Javadoc)
   * @see javax.portlet.ActionResponse#setWindowState(javax.portlet.WindowState)
   */
  public void setWindowState(WindowState arg0) throws WindowStateException
  {
    _resp.setWindowState(arg0);
  }
}
