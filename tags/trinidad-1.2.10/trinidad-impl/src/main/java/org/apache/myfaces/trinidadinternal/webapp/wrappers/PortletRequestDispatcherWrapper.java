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

import javax.portlet.PortletException;
import javax.portlet.PortletRequestDispatcher;
import javax.portlet.RenderRequest;
import javax.portlet.RenderResponse;

/**
 * TODO: Document this
 *
 * @version $Revision$ $Date$
 */

public class PortletRequestDispatcherWrapper implements PortletRequestDispatcher
{
  public PortletRequestDispatcherWrapper(PortletRequestDispatcher dispatcher)
  {
    _dispatch = dispatcher;
  }

  private PortletRequestDispatcher _dispatch;

  /* (non-Javadoc)
   * @see javax.portlet.PortletRequestDispatcher#include(javax.portlet.RenderRequest, javax.portlet.RenderResponse)
   */
  public void include(RenderRequest arg0, RenderResponse arg1) throws PortletException, IOException
  {
    //We need to dispatch the origional request/response objects.
    //So cut through all the wrappers and dispatch the origionals.
    //TODO: Try out some usecases.
    RenderRequest req = arg0;
    while(req instanceof RenderRequestWrapper)
    {
      req = ((RenderRequestWrapper)req).getRequest();
    }

    RenderResponse resp = arg1;
    while(resp instanceof RenderResponseWrapper)
    {
      resp = ((RenderResponseWrapper)resp).getResponse();
    }
    _dispatch.include(req, resp);

  }

}
