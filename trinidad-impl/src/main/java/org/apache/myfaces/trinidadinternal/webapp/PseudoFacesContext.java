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
package org.apache.myfaces.trinidadinternal.webapp;

import java.io.InputStream;
import java.net.URL;

import java.util.AbstractMap;
import java.util.Collections;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import java.security.Principal;

import javax.faces.application.Application;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseStream;
import javax.faces.context.ResponseWriter;

import javax.faces.render.RenderKit;

//import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

/**
 * Pseudo FacesContext, vended by the filter for code that 
 * needs to run before (or after) the FacesServlet, but needs
 * access to servlet objects.  This object is only available
 * inside the filter.
 * 
 * @author The Oracle ADF Faces Team
 */
class PseudoFacesContext extends FacesContext
{
  public PseudoFacesContext(
    ServletRequest  request,
    ServletResponse response)
  {
    _external = new External((HttpServletRequest) request,
                             (HttpServletResponse) response);
  }

  @Override
  public ResponseWriter getResponseWriter()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void setResponseWriter(ResponseWriter responseWriter)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public Iterator<FacesMessage> getMessages()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public Iterator<FacesMessage> getMessages(String id)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void addMessage(String id, FacesMessage message)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public FacesMessage.Severity getMaximumSeverity()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public Iterator<String> getClientIdsWithMessages()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public Application getApplication()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public UIViewRoot getViewRoot()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void setViewRoot(UIViewRoot viewRoot)
  {
     throw new UnsupportedOperationException();
  }

  @Override
  public ExternalContext getExternalContext()
  {
    return _external;
  }

  @Override
  public RenderKit getRenderKit()
  {
     throw new UnsupportedOperationException();
  }

  @Override
  public boolean getRenderResponse()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean getResponseComplete()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public ResponseStream getResponseStream()
  {
    throw new UnsupportedOperationException();
  }
  
  @Override
  public void setResponseStream(ResponseStream responseStream)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void release()
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void renderResponse()
  {
    throw new UnsupportedOperationException();
  }
  
  @Override
  public void responseComplete()
  {
    throw new UnsupportedOperationException();
  }


  private final ExternalContext _external;

  private static final class External extends ExternalContext
  {
    public External(
      HttpServletRequest  request,
      HttpServletResponse response)
    {
      _request  = request;
      _response = response;
    }
    
    // Can't very well have a ServletContext before a servlet!!!
    @Override
    public Object getContext() { throw new UnsupportedOperationException(); }
    @Override
    public Object getRequest() { return _request; }
    @Override
    public Object getResponse() { return _response; }
    @Override
    public Object getSession(boolean create)
    {
      return _request.getSession(create);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> getSessionMap()
    {
      Object o = getSession(true);
      if (o == null)
        return Collections.EMPTY_MAP;

      return new SessionMap((HttpSession) o);
    }

    @Override
    public String getRequestContextPath()
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public String getRequestServletPath()
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public String getInitParameter(String name)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public String encodeResourceURL(String url)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public String encodeActionURL(String url)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public Map<String, Object> getRequestMap()
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public Map<String, Object> getApplicationMap()
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public void dispatch(String path)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public String encodeNamespace(String name)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public String getAuthType()
    {
      throw new UnsupportedOperationException();
    }

    // -= Simon Lessard =- 
    // FIXME: Odd... JSF 1.2 does not give generics type for this map
    @SuppressWarnings("unchecked")
    @Override
    public Map getInitParameterMap()
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public String getRemoteUser()
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public Map<String, Object> getRequestCookieMap()
    {
      throw new UnsupportedOperationException();
    }
    
    @Override
    public Map<String, String> getRequestHeaderMap()
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public Map<String, String[]> getRequestHeaderValuesMap()
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public Locale getRequestLocale()
    {
      throw new UnsupportedOperationException();
    }
    
    @Override
    public Iterator<Locale> getRequestLocales()
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public Map<String, String> getRequestParameterMap()
    {
      return new ParameterMap(_request);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Iterator<String> getRequestParameterNames()
    {
      return _request.getParameterMap().keySet().iterator();
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, String[]> getRequestParameterValuesMap()
    {
      return _request.getParameterMap();
    }

    @Override
    public String getRequestPathInfo()
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public URL getResource(String path)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public InputStream getResourceAsStream(String path)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public Set<String> getResourcePaths(String path)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public Principal getUserPrincipal()
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean isUserInRole(String role)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public void log(String message)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public void log(String message, Throwable exception)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public void redirect(String url)
    {
      throw new UnsupportedOperationException();
    }

    private HttpServletRequest  _request;
    private HttpServletResponse _response;
  }

  private static final class ParameterMap extends AbstractMap<String, String>
  {
    public ParameterMap(ServletRequest request)
    {
      _request = request;
    }
    
    @Override
    public String get(Object key)
    {
      return _request.getParameter((String) key);
    }
        
    @Override
    public Set<Map.Entry<String, String>> entrySet()
    {
      throw new UnsupportedOperationException();
    }
    
    private ServletRequest _request;
  }

  private static final class SessionMap extends AbstractMap<String, Object>
  {
    public SessionMap(HttpSession session)
    {
      _session = session;
    }
    
    @Override
    public Object get(Object key)
    {
      return _session.getAttribute((String) key);
    }
    
    @Override
    public Object put(String key, Object value)
    {
      Object old = _session.getAttribute(key);
      _session.setAttribute(key, value);
      return old;
    }
    
    @Override
    public Object remove(Object key)
    {
      Object old = _session.getAttribute((String) key);
      _session.removeAttribute((String) key);
      return old;
    }
    
    @Override
    public Set<Map.Entry<String, Object>> entrySet()
    {
      throw new UnsupportedOperationException();
    }
    
    private HttpSession _session;
  }
}
