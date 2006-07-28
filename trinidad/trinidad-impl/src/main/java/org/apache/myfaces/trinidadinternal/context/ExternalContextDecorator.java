/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.trinidadinternal.context;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.Principal;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;
import java.util.Map;

import javax.faces.context.ExternalContext;


/**
 * Base class for decorating ExternalContexts.
 *
 * @author The Oracle ADF Faces Team
 */
abstract public class ExternalContextDecorator extends ExternalContext
{
  public void dispatch(String path) throws IOException
  {
    getExternalContext().dispatch(path);
  }


  public Object getContext()
  { 
    return getExternalContext().getContext();
  }
  
  public Object getRequest()
  {
    return getExternalContext().getRequest(); 
  }
  
  public Object getResponse()
  {
    return getExternalContext().getResponse();
  }
  
  public Object getSession(boolean create)
  {
    return getExternalContext().getSession(create);
  }
  
  public String getRequestContextPath()
  {
    return getExternalContext().getRequestContextPath();
  }

  public String getRequestPathInfo()
  {
    return getExternalContext().getRequestPathInfo();
  }

  public String getRequestServletPath()
  {
    return getExternalContext().getRequestServletPath();
  }

  public String getInitParameter(String name)
  {
    return getExternalContext().getInitParameter(name);
  }
  
  public String encodeResourceURL(String url)
  {
    return getExternalContext().encodeResourceURL(url);
  }

  public String encodeActionURL(String url)
  {
    return getExternalContext().encodeActionURL(url);
  }

  public String encodeNamespace(String ns)
  {
    return getExternalContext().encodeNamespace(ns);
  }

  public String getAuthType()
  {
    return getExternalContext().getAuthType();
  }

  public String getRemoteUser()
  {
    return getExternalContext().getRemoteUser();
  }

  public Principal getUserPrincipal()
  {
    return getExternalContext().getUserPrincipal();
  }

  public boolean isUserInRole(String role)
  {
    return getExternalContext().isUserInRole(role);
  }

  public URL getResource(String path) throws MalformedURLException
  {
    return getExternalContext().getResource(path);
  }

  public InputStream getResourceAsStream(String path)
  {
    return getExternalContext().getResourceAsStream(path);
  }

  public Set getResourcePaths(String path)
  {
    return getExternalContext().getResourcePaths(path);
  }

  public Map getRequestParameterMap()
  {
    return getExternalContext().getRequestParameterMap();
  }

  public Map getRequestParameterValuesMap()
  {
    return getExternalContext().getRequestParameterValuesMap();
  }

  public Iterator getRequestParameterNames()
  {
    return getExternalContext().getRequestParameterNames();
  }

  public Map getRequestCookieMap()
  {
    return getExternalContext().getRequestCookieMap();
  }

  public Map getRequestHeaderMap()
  {
    return getExternalContext().getRequestHeaderMap();
  }

  public Map getRequestHeaderValuesMap()
  {
    return getExternalContext().getRequestHeaderValuesMap();
  }


  public Map getInitParameterMap()
  {
    return getExternalContext().getInitParameterMap();
  }

  public Map getApplicationMap()
  {
    return getExternalContext().getApplicationMap();
  }

  public Map getSessionMap()
  {
    return getExternalContext().getSessionMap();
  }

  public Map getRequestMap()
  {
    return getExternalContext().getRequestMap();
  }


  public Locale getRequestLocale()
  {
    return getExternalContext().getRequestLocale();
  }

  public Iterator getRequestLocales()
  {
    return getExternalContext().getRequestLocales();
  }


  public void log(String message)
  {
    getExternalContext().log(message);
  }

  public void log(String message, Throwable t)
  {
    getExternalContext().log(message, t);
  }

  public void redirect(String url) throws IOException
  {
    getExternalContext().redirect(url);
  }

  abstract protected ExternalContext getExternalContext();
}
