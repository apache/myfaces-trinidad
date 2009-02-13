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
package org.apache.myfaces.trinidad.context;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
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
 */
abstract public class ExternalContextDecorator extends ExternalContext
{
  @Override
  public void dispatch(String path) throws IOException
  {
    getExternalContext().dispatch(path);
  }

  @Override
  public Object getContext()
  { 
    return getExternalContext().getContext();
  }
  
  @Override
  public Object getRequest()
  {
    return getExternalContext().getRequest(); 
  }
  
  @Override
  public String getRequestCharacterEncoding()
  {
    return getExternalContext().getRequestCharacterEncoding(); 
  }
  
  @Override
  public String getRequestContentType()
  {
    return getExternalContext().getRequestContentType(); 
  }
  
  @Override
  public Object getResponse()
  {
    return getExternalContext().getResponse();
  }
  
  @Override
  public String getResponseCharacterEncoding()
  {
    return getExternalContext().getResponseCharacterEncoding(); 
  }
  
  @Override
  public String getResponseContentType()
  {
    return getExternalContext().getResponseContentType(); 
  }
  
  @Override
  public Object getSession(boolean create)
  {
    return getExternalContext().getSession(create);
  }
  
  @Override
  public String getRequestContextPath()
  {
    return getExternalContext().getRequestContextPath();
  }

  @Override
  public String getRequestPathInfo()
  {
    return getExternalContext().getRequestPathInfo();
  }

  @Override
  public String getRequestServletPath()
  {
    return getExternalContext().getRequestServletPath();
  }

  @Override
  public String getInitParameter(String name)
  {
    return getExternalContext().getInitParameter(name);
  }
  
  @Override
  public String encodeResourceURL(String url)
  {
    return getExternalContext().encodeResourceURL(url);
  }

  @Override
  public String encodeActionURL(String url)
  {
    return getExternalContext().encodeActionURL(url);
  }

  @Override
  public String encodeNamespace(String ns)
  {
    return getExternalContext().encodeNamespace(ns);
  }

  @Override
  public String getAuthType()
  {
    return getExternalContext().getAuthType();
  }

  @Override
  public String getRemoteUser()
  {
    return getExternalContext().getRemoteUser();
  }

  @Override
  public Principal getUserPrincipal()
  {
    return getExternalContext().getUserPrincipal();
  }

  @Override
  public boolean isUserInRole(String role)
  {
    return getExternalContext().isUserInRole(role);
  }

  @Override
  public URL getResource(String path) throws MalformedURLException
  {
    return getExternalContext().getResource(path);
  }

  @Override
  public InputStream getResourceAsStream(String path)
  {
    return getExternalContext().getResourceAsStream(path);
  }

  @SuppressWarnings("unchecked")
  @Override
  public Set getResourcePaths(String path)
  {
    return getExternalContext().getResourcePaths(path);
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map getRequestParameterMap()
  {
    return getExternalContext().getRequestParameterMap();
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map getRequestParameterValuesMap()
  {
    return getExternalContext().getRequestParameterValuesMap();
  }

  @SuppressWarnings("unchecked")
  @Override
  public Iterator getRequestParameterNames()
  {
    return getExternalContext().getRequestParameterNames();
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map getRequestCookieMap()
  {
    return getExternalContext().getRequestCookieMap();
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map getRequestHeaderMap()
  {
    return getExternalContext().getRequestHeaderMap();
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map getRequestHeaderValuesMap()
  {
    return getExternalContext().getRequestHeaderValuesMap();
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map getInitParameterMap()
  {
    return getExternalContext().getInitParameterMap();
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map getApplicationMap()
  {
    return getExternalContext().getApplicationMap();
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map getSessionMap()
  {
    return getExternalContext().getSessionMap();
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map getRequestMap()
  {
    return getExternalContext().getRequestMap();
  }

  @Override
  public Locale getRequestLocale()
  {
    return getExternalContext().getRequestLocale();
  }

  @Override
  public void setRequest(Object request)
  {
    getExternalContext().setRequest(request);
  }

  @SuppressWarnings("unchecked")
  @Override
  public void setRequestCharacterEncoding(String encoding) throws UnsupportedEncodingException 
  {
    getExternalContext().setRequestCharacterEncoding(encoding);
  }

  @Override
  public void setResponse(Object response)
  {
    getExternalContext().setResponse(response);
  }

  @Override
  public void setResponseCharacterEncoding(String encoding)
  {
    getExternalContext().setResponseCharacterEncoding(encoding);
  }

  @SuppressWarnings("unchecked")
  @Override
  public Iterator getRequestLocales()
  {
    return getExternalContext().getRequestLocales();
  }

  @Override
  public void log(String message)
  {
    getExternalContext().log(message);
  }

  @Override
  public void log(String message, Throwable t)
  {
    getExternalContext().log(message, t);
  }

  @Override
  public void redirect(String url) throws IOException
  {
    getExternalContext().redirect(url);
  }

  abstract protected ExternalContext getExternalContext();
}
