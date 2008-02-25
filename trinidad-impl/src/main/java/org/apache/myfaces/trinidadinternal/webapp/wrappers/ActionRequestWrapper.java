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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import javax.portlet.ActionRequest;

/**
 * Wrapper for the native ActionRequest object.  Unlike the servlet wrappers,
 * Portlet Request/Response wrapping is not supported by JSR-160.  Therefore
 * it is important to use the PortletContextWrapper when retrieving the
 * PortletRequestDispatcher.  This will give you a special dispatcher that is
 * aware of the wrapping and will deploy the underlying PortletRequest/Response
 * implementations.
 *
 * @version $Revision$ $Date$
 */
public class ActionRequestWrapper extends PortletRequestWrapper implements ActionRequest
{
  public ActionRequestWrapper(ActionRequest request)
  {
    super(request);
   _req = request;
  }

  private ActionRequest _req;

  /* (non-Javadoc)
   * @see javax.portlet.ActionRequest#getCharacterEncoding()
   */
  public String getCharacterEncoding()
  {
    return _req.getCharacterEncoding();
  }

  /* (non-Javadoc)
   * @see javax.portlet.ActionRequest#getContentLength()
   */
  public int getContentLength()
  {
    return _req.getContentLength();
  }

  /* (non-Javadoc)
   * @see javax.portlet.ActionRequest#getContentType()
   */
  public String getContentType()
  {
    return _req.getContentType();
  }

  /* (non-Javadoc)
   * @see javax.portlet.ActionRequest#getPortletInputStream()
   */
  public InputStream getPortletInputStream() throws IOException
  {
    return _req.getPortletInputStream();
  }

  /* (non-Javadoc)
   * @see javax.portlet.ActionRequest#getReader()
   */
  public BufferedReader getReader() throws UnsupportedEncodingException, IOException
  {
    return _req.getReader();
  }

  /* (non-Javadoc)
   * @see javax.portlet.ActionRequest#setCharacterEncoding(java.lang.String)
   */
  public void setCharacterEncoding(String arg0) throws UnsupportedEncodingException
  {
    _req.setCharacterEncoding(arg0);
  }
}
