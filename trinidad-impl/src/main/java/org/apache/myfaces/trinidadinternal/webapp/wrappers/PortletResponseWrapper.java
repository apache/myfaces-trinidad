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

import javax.portlet.PortletResponse;

/**
 * TODO: Document this
 *
 * @version $Revision$ $Date$
 */

public class PortletResponseWrapper implements PortletResponse
{
  public PortletResponseWrapper(PortletResponse response)
  {
   _resp = response;
  }

  private PortletResponse _resp;

  public PortletResponse getResponse()
  {
    return _resp;
  }

  /**
   * @param arg0
   * @param arg1
   * @see javax.portlet.PortletResponse#addProperty(java.lang.String, java.lang.String)
   */
  public void addProperty(String arg0, String arg1)
  {
    _resp.addProperty(arg0, arg1);
  }

  /**
   * @param arg0
   * @return
   * @see javax.portlet.PortletResponse#encodeURL(java.lang.String)
   */
  public String encodeURL(String arg0)
  {
    return _resp.encodeURL(arg0);
  }

  /**
   * @param arg0
   * @param arg1
   * @see javax.portlet.PortletResponse#setProperty(java.lang.String, java.lang.String)
   */
  public void setProperty(String arg0, String arg1)
  {
    _resp.setProperty(arg0, arg1);
  }
}
