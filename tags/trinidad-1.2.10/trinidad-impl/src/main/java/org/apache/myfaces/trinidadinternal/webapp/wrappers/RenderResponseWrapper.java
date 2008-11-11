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
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Locale;

import javax.portlet.PortletURL;
import javax.portlet.RenderResponse;

/**
 * TODO: Document this
 *
 * @version $Revision$ $Date$
 */

public class RenderResponseWrapper extends PortletResponseWrapper implements RenderResponse
{
  public RenderResponseWrapper(RenderResponse response)
  {
    super(response);
    _resp = response;
  }

  private RenderResponse _resp;

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#createActionURL()
   */
  public PortletURL createActionURL()
  {
    return _resp.createActionURL();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#createRenderURL()
   */
  public PortletURL createRenderURL()
  {
    return _resp.createRenderURL();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#flushBuffer()
   */
  public void flushBuffer() throws IOException
  {
    _resp.flushBuffer();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#getBufferSize()
   */
  public int getBufferSize()
  {
    return _resp.getBufferSize();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#getCharacterEncoding()
   */
  public String getCharacterEncoding()
  {
    return _resp.getCharacterEncoding();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#getContentType()
   */
  public String getContentType()
  {
    return _resp.getContentType();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#getLocale()
   */
  public Locale getLocale()
  {
    return _resp.getLocale();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#getNamespace()
   */
  public String getNamespace()
  {
    return _resp.getNamespace();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#getPortletOutputStream()
   */
  public OutputStream getPortletOutputStream() throws IOException
  {
    return _resp.getPortletOutputStream();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#getWriter()
   */
  public PrintWriter getWriter() throws IOException
  {
    return _resp.getWriter();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#isCommitted()
   */
  public boolean isCommitted()
  {
    return _resp.isCommitted();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#reset()
   */
  public void reset()
  {
    _resp.reset();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#resetBuffer()
   */
  public void resetBuffer()
  {
    _resp.resetBuffer();
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#setBufferSize(int)
   */
  public void setBufferSize(int arg0)
  {
    _resp.setBufferSize(arg0);
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#setContentType(java.lang.String)
   */
  public void setContentType(String arg0)
  {
    _resp.setContentType(arg0);
  }

  /* (non-Javadoc)
   * @see javax.portlet.RenderResponse#setTitle(java.lang.String)
   */
  public void setTitle(String arg0)
  {
    _resp.setTitle(arg0);
  }

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidadinternal.webapp.wrappers.PortletResponseWrapper#getResponse()
   */
  @Override
  public RenderResponse getResponse()
  {
    return _resp;
  }
}
