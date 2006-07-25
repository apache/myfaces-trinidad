/*
 * Copyright  2004-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.adfinternal.webapp;

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

  public ResponseWriter getResponseWriter()
  {
    throw new UnsupportedOperationException();
  }

  public void setResponseWriter(ResponseWriter responseWriter)
  {
    throw new UnsupportedOperationException();
  }

  public Iterator getMessages()
  {
    throw new UnsupportedOperationException();
  }

  public Iterator getMessages(String id)
  {
    throw new UnsupportedOperationException();
  }

  public void addMessage(String id, FacesMessage message)
  {
    throw new UnsupportedOperationException();
  }

  public FacesMessage.Severity getMaximumSeverity()
  {
    throw new UnsupportedOperationException();
  }

  public Iterator getClientIdsWithMessages()
  {
    throw new UnsupportedOperationException();
  }

  public Application getApplication()
  {
    throw new UnsupportedOperationException();
  }

  public UIViewRoot getViewRoot()
  {
    throw new UnsupportedOperationException();
  }

  public void setViewRoot(UIViewRoot viewRoot)
  {
     throw new UnsupportedOperationException();
  }

  public ExternalContext getExternalContext()
  {
    return _external;
  }

  public RenderKit getRenderKit()
  {
     throw new UnsupportedOperationException();
  }

  public boolean getRenderResponse()
  {
    throw new UnsupportedOperationException();
  }

  public boolean getResponseComplete()
  {
    throw new UnsupportedOperationException();
  }

  public ResponseStream getResponseStream()
  {
    throw new UnsupportedOperationException();
  }
  
  public void setResponseStream(ResponseStream responseStream)
  {
    throw new UnsupportedOperationException();
  }

  public void release()
  {
    throw new UnsupportedOperationException();
  }

  public void renderResponse()
  {
    throw new UnsupportedOperationException();
  }
  
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
    public Object getContext() { throw new UnsupportedOperationException(); }
    public Object getRequest() { return _request; }
    public Object getResponse() { return _response; }
    public Object getSession(boolean create)
    {
      return _request.getSession(create);
    }

    public Map getSessionMap()
    {
      Object o = getSession(true);
      if (o == null)
        return Collections.EMPTY_MAP;

      return new SessionMap((HttpSession) o);
    }


    public String getRequestContextPath()
    {
      throw new UnsupportedOperationException();
    }

    public String getRequestServletPath()
    {
      throw new UnsupportedOperationException();
    }

    public String getInitParameter(String name)
    {
      throw new UnsupportedOperationException();
    }

    public String encodeResourceURL(String url)
    {
      throw new UnsupportedOperationException();
    }

    public String encodeActionURL(String url)
    {
      throw new UnsupportedOperationException();
    }

    public Map getRequestMap()
    {
      throw new UnsupportedOperationException();
    }

    public Map getApplicationMap()
    {
      throw new UnsupportedOperationException();
    }

    public void dispatch(String path)
    {
      throw new UnsupportedOperationException();
    }

    public String encodeNamespace(String name)
    {
      throw new UnsupportedOperationException();
    }

    public String getAuthType()
    {
      throw new UnsupportedOperationException();
    }

    public Map getInitParameterMap()
    {
      throw new UnsupportedOperationException();
    }

    public String getRemoteUser()
    {
      throw new UnsupportedOperationException();
    }

    public Map getRequestCookieMap()
    {
      throw new UnsupportedOperationException();
    }
    
    public Map getRequestHeaderMap()
    {
      throw new UnsupportedOperationException();
    }

    public Map getRequestHeaderValuesMap()
    {
      throw new UnsupportedOperationException();
    }

    public Locale getRequestLocale()
    {
      throw new UnsupportedOperationException();
    }
    
    public Iterator getRequestLocales()
    {
      throw new UnsupportedOperationException();
    }

    public Map getRequestParameterMap()
    {
      return new ParameterMap(_request);
    }

    public Iterator getRequestParameterNames()
    {
      return _request.getParameterMap().keySet().iterator();
    }

    public Map getRequestParameterValuesMap()
    {
      return _request.getParameterMap();
    }

    public String getRequestPathInfo()
    {
      throw new UnsupportedOperationException();
    }

    public URL getResource(String path)
    {
      throw new UnsupportedOperationException();
    }

    public InputStream getResourceAsStream(String path)
    {
      throw new UnsupportedOperationException();
    }

    public Set getResourcePaths(String path)
    {
      throw new UnsupportedOperationException();
    }

    public Principal getUserPrincipal()
    {
      throw new UnsupportedOperationException();
    }

    public boolean isUserInRole(String role)
    {
      throw new UnsupportedOperationException();
    }

    public void log(String message)
    {
      throw new UnsupportedOperationException();
    }

    public void log(String message, Throwable exception)
    {
      throw new UnsupportedOperationException();
    }

    public void redirect(String url)
    {
      throw new UnsupportedOperationException();
    }

    private HttpServletRequest  _request;
    private HttpServletResponse _response;
  }

  private static final class ParameterMap extends AbstractMap
  {
    public ParameterMap(ServletRequest request)
    {
      _request = request;
    }
    
    public Object get(Object key)
    {
      return _request.getParameter((String) key);
    }
        
    public Set entrySet()
    {
      throw new UnsupportedOperationException();
    }
    
    private ServletRequest _request;
  }

  private static final class SessionMap extends AbstractMap
  {
    public SessionMap(HttpSession session)
    {
      _session = session;
    }
    
    public Object get(Object key)
    {
      return _session.getAttribute((String) key);
    }
    
    public Object put(Object key, Object value)
    {
      Object old = _session.getAttribute((String) key);
      _session.setAttribute((String) key, value);
      return old;
    }
    
    public Object remove(Object key)
    {
      Object old = _session.getAttribute((String) key);
      _session.removeAttribute((String) key);
      return old;
    }
    
    public Set entrySet()
    {
      throw new UnsupportedOperationException();
    }
    
    private HttpSession _session;
  }
}
