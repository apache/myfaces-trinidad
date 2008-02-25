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
package org.apache.myfaces.trinidadinternal.config.dispatch;

import java.io.IOException;
import java.util.Map;

import javax.faces.FacesException;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.portlet.PortletContext;
import javax.portlet.PortletException;
import javax.portlet.PortletRequestDispatcher;
import javax.portlet.RenderRequest;
import javax.portlet.RenderResponse;
import javax.servlet.ServletResponse;

import org.apache.myfaces.trinidad.config.Configurator;
import org.apache.myfaces.trinidad.context.ExternalContextDecorator;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidadinternal.webapp.wrappers.PortletContextWrapper;

/**
 * TODO: Document this
 *
 * @version $Revision$ $Date$
 */

public class DispatchResponseConfiguratorImpl extends Configurator
{

  @Override
  public ExternalContext getExternalContext(ExternalContext externalContext)
  {
    // TODO sobryan (dependency = JSF 1.2)
    // For JSF 1.2, we can probably simply wrap the request objects and set them on
    // the existing ExternalContext.
    if(!isApplied(externalContext))
    {
      if(ExternalContextUtils.isPortlet(externalContext))
      {
        if(!ExternalContextUtils.isAction(externalContext))
        {
          apply(externalContext);
          return new PortletExternalContext(externalContext);
        }
      }
      else
      {
        apply(externalContext);
        return new ServletExternalContext(externalContext);
      }
    }


    //return the origional
    return externalContext;
  }

  @SuppressWarnings("unchecked")
  static public String getContentType(
    FacesContext context)
  {
    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();
    return (String) requestMap.get(__CONTENT_TYPE_KEY);
  }

  static private class ServletExternalContext extends ExternalContextDecorator
  {
    public ServletExternalContext(ExternalContext ec)
    {
      _ec = ec;
    }

    @Override
    public Object getResponse()
    {
      if(_response == null)
      {
        _response = new DispatchServletResponse(_ec);
      }

      return _response;
    }

    @Override
    protected ExternalContext getExternalContext()
    {
      return _ec;
    }

    private ServletResponse  _response;
    private ExternalContext _ec;
  }

  static private class PortletExternalContext extends ExternalContextDecorator
  {
    public PortletExternalContext(ExternalContext ec)
    {
      _ec = ec;
    }

    @Override
    public Object getContext()
    {
      return _getPortletContext();
    }

    @Override
    public Object getResponse()
    {
      return _getRenderResponse();
    }

    @Override
    public void dispatch(String path) throws IOException
    {
      final PortletRequestDispatcher requestDispatcher =
        ((PortletContext)getContext()).getRequestDispatcher(path);
      try
      {
        requestDispatcher.include((RenderRequest)getRequest(), (RenderResponse)getResponse());
      }
      catch (final PortletException e)
      {
        if (e.getMessage() != null)
        {
          throw new FacesException(e.getMessage(), e);
        }
        else
        {
          throw new FacesException(e);
        }
      }
    }

    @Override
    protected ExternalContext getExternalContext()
    {
      return _ec;
    }

    private PortletContext _getPortletContext()
    {
      if(_context == null)
      {
        _context = new PortletContextWrapper((PortletContext)_ec.getContext());
      }
      return _context;
    }

    private RenderResponse _getRenderResponse()
    {
      if(_response == null)
      {
        _response = new DispatchRenderResponse(_ec);
      }
      return _response;
    }

    private ExternalContext _ec;
    private RenderResponse _response;
    private PortletContext _context;
  }

  /**
   * Returns <code>true</code> if the request wrapper has been applied.
   *
   * @param context
   * @return
   */
  static public boolean isApplied(ExternalContext context)
  {
    return (context.getRequestMap().get(_APPLIED)!=null);
  }

  /**
   *
   */
  @SuppressWarnings("unchecked")
  static public void apply(ExternalContext context)
  {
    context.getRequestMap().put(_APPLIED, Boolean.TRUE);
  }

  static private final String _APPLIED =
    DispatchResponseConfiguratorImpl.class.getName()+".APPLIED";
  static final String __CONTENT_TYPE_KEY =
    DispatchResponseConfiguratorImpl.class.getName() + ".CONTENT_TYPE";
}
