/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadinternal.context.external;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;

import java.security.Principal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.faces.FacesException;
import javax.faces.application.Application;
import javax.faces.application.ViewHandler;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * This class will implement the ExternalContext for use with Trinidad Configurations using the
 * Trinidad {@link org.apache.myfaces.Trinidad.config.Configurator} object. <br>
 * This code was origionally taken from MyFaces.
 *
 * @version $Revision$ $Date$
 */
public class ServletExternalContext extends ExternalContext
{
  public ServletExternalContext(final ServletContext servletContext,
      final ServletRequest servletRequest, final ServletResponse servletResponse)
  {
    assert servletContext != null;

    _servletContext = servletContext;
    _servletRequest = servletRequest;
    _servletResponse = servletResponse;

    if (servletRequest != null && servletRequest instanceof HttpServletRequest)
    {
      _httpServletRequest = (HttpServletRequest) servletRequest;

      if (_servletResponse != null)
      {
        _httpServletResponse = (HttpServletResponse) servletResponse;
      }
    }

    if (_httpServletRequest != null)
    {
      _initHttpServletRequest();
    }
  }

  @Override
  public void dispatch(final String path) throws IOException, FacesException
  {
    _checkRequest();
    _checkResponse();
    final RequestDispatcher requestDispatcher = _servletRequest.getRequestDispatcher(path);

    // If there is no dispatcher, send NOT_FOUND
    if (requestDispatcher == null)
    {
      if (_httpServletResponse != null)
      {
        _httpServletResponse.sendError(HttpServletResponse.SC_NOT_FOUND);
      }

      return;
    }

    try
    {
      requestDispatcher.forward(_servletRequest, _servletResponse);
    }
    catch (final ServletException e)
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
  public String encodeActionURL(final String url)
  {
    _checkRequest();
    _checkNull(url, "url");

    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException("Only HttpServletRequest supported");
    }

    return _httpServletResponse.encodeURL(url);
  }

  @Override
  public String encodeNamespace(final String s)
  {
    _checkRequest();
    return s;
  }

  @Override
  public String encodeResourceURL(final String s)
  {
    _checkRequest();
    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
    }
    return _httpServletResponse.encodeURL(s);
  }

  @Override
  public Map<String, Object> getApplicationMap()
  {
    if (_applicationMap == null)
    {
      _applicationMap = new ServletApplicationMap(_servletContext);
    }
    return _applicationMap;
  }

  @Override
  public String getAuthType()
  {
    _checkRequest();

    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
    }
    return _httpServletRequest.getAuthType();
  }

  @Override
  public Object getContext()
  {
    return _servletContext;
  }

  @Override
  public String getInitParameter(final String s)
  {
    return _servletContext.getInitParameter(s);
  }

  @Override
  @SuppressWarnings("unchecked")
  public Map<String, String> getInitParameterMap()
  {
    if (_initParameterMap == null)
    {
      // We cache it as an attribute in ServletContext itself (is this circular reference a
      // problem?)
      if ((_initParameterMap = (Map<String, String>)_servletContext.getAttribute(_INIT_PARAMETER_MAP_ATTRIBUTE)) == null)
      {
        _initParameterMap = new ServletInitParameterMap(_servletContext);
        _servletContext.setAttribute(_INIT_PARAMETER_MAP_ATTRIBUTE, _initParameterMap);
      }
    }
    return _initParameterMap;
  }

  @Override
  public String getRemoteUser()
  {
    _checkRequest();
    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
    }
    return _httpServletRequest.getRemoteUser();
  }

  @Override
  public Object getRequest()
  {
    return _servletRequest;
  }

  @Override
  public String getRequestContextPath()
  {
    _checkRequest();
    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
    }
    return _httpServletRequest.getContextPath();
  }

  @Override
  public Map<String, Object> getRequestCookieMap()
  {
    _checkRequest();
    if (_requestCookieMap == null)
    {
      if (_httpServletRequest == null)
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
      }
      _requestCookieMap = new ServletCookieMap(_httpServletRequest);
    }
    return _requestCookieMap;
  }

  @Override
  public Map<String, String> getRequestHeaderMap()
  {
    _checkRequest();
    if (_requestHeaderMap == null)
    {
      if (_httpServletRequest == null)
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
      }
      _requestHeaderMap = new ServletRequestHeaderMap(_httpServletRequest);
    }
    return _requestHeaderMap;
  }

  @Override
  public Map<String, String[]> getRequestHeaderValuesMap()
  {
    _checkRequest();
    if (_requestHeaderValuesMap == null)
    {
      if (_httpServletRequest == null)
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
      }
      _requestHeaderValuesMap = new ServletRequestHeaderValuesMap(_httpServletRequest);
    }
    return _requestHeaderValuesMap;
  }

  @Override
  public Locale getRequestLocale()
  {
    _checkRequest();
    return _servletRequest.getLocale();
  }

  @Override
  @SuppressWarnings("unchecked")
  public Iterator<Locale> getRequestLocales()
  {
    _checkRequest();
    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
    }
    return new EnumerationIterator<Locale>(_httpServletRequest.getLocales());
  }

  @Override
  public Map<String, Object> getRequestMap()
  {
    _checkRequest();
    if (_requestMap == null)
    {
      _requestMap = new ServletRequestMap(_servletRequest);
    }
    return _requestMap;
  }

  @Override
  public Map<String, String> getRequestParameterMap()
  {
    _checkRequest();
    if (_requestParameterMap == null)
    {
      _requestParameterMap = new ServletRequestParameterMap(_servletRequest);
    }
    return _requestParameterMap;
  }

  @Override
  public Iterator<String> getRequestParameterNames()
  {
    _checkRequest();
    @SuppressWarnings("unchecked")
    final Iterator<String> it = new EnumerationIterator<String>(_servletRequest.getParameterNames());
    return it;
  }

  @Override
  public Map<String, String[]> getRequestParameterValuesMap()
  {
    _checkRequest();
    if (_requestParameterValuesMap == null)
    {
      _requestParameterValuesMap = new ServletRequestParameterValuesMap(_servletRequest);
    }
    return _requestParameterValuesMap;
  }

  @Override
  public String getRequestPathInfo()
  {
    _checkRequest();
    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
    }
    return _requestPathInfo;
  }

  @Override
  public String getRequestServletPath()
  {
    _checkRequest();
    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
    }
    // return ((HttpServletRequest)_servletRequest).getServletPath();
    // HACK: see constructor
    return _requestServletPath;
  }

  @Override
  public URL getResource(final String s) throws MalformedURLException
  {
    return _servletContext.getResource(s);
  }

  @Override
  public InputStream getResourceAsStream(final String s)
  {
    return _servletContext.getResourceAsStream(s);
  }

  @Override
  @SuppressWarnings("unchecked")
  public Set<String> getResourcePaths(final String s)
  {
    return _servletContext.getResourcePaths(s);
  }

  @Override
  public Object getResponse()
  {
    return _servletResponse;
  }

  @Override
  public Writer getResponseOutputWriter() throws IOException
  {
    return _servletResponse.getWriter();
  }

  @Override
  public OutputStream getResponseOutputStream() throws IOException
  {
    return _servletResponse.getOutputStream();
  }


  @Override
  public Object getSession(final boolean create)
  {
    //If we don't have a request object, just return null
    if (_httpServletRequest == null)
    {
      return null;
    }
    return _httpServletRequest.getSession(create);
  }

  @Override
  public Map<String, Object> getSessionMap()
  {
    _checkRequest();
    if (_sessionMap == null)
    {
      if (_httpServletRequest == null)
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
      }
      _sessionMap = new ServletSessionMap(_httpServletRequest);
    }
    return _sessionMap;
  }

  @Override
  public Principal getUserPrincipal()
  {
    _checkRequest();
    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
    }
    return _httpServletRequest.getUserPrincipal();
  }

  @Override
  public boolean isUserInRole(final String role)
  {
    _checkRequest();
    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ONLY_HTTPSERVLETREQUEST_SUPPORTED"));
    }
    return _httpServletRequest.isUserInRole(role);
  }

  @Override
  public void log(final String message)
  {
    _servletContext.log(message);
  }

  @Override
  public void log(final String message, final Throwable t)
  {
    _servletContext.log(message, t);
  }

  @Override
  public void redirect(final String url) throws IOException
  {
    _checkResponse();

    if (_servletResponse instanceof HttpServletResponse)
    {
      _httpServletResponse.sendRedirect(url);

      FacesContext fc = FacesContext.getCurrentInstance();
      if (fc != null)
      {
        fc.responseComplete();
      }
    }
    else
    {
      throw new IllegalArgumentException(_LOG.getMessage(
        "ONLY_HTTPSERVLETRESPONSE_SUPPORTED"));
    }
  }

  @Override
  public String getRequestCharacterEncoding()
  {
    _checkRequest();
    return _servletRequest.getCharacterEncoding();
  }

  @Override
  public String getRequestContentType()
  {
    _checkRequest();
    return _servletRequest.getContentType();
  }

  @Override
  public String getResponseCharacterEncoding()
  {
    _checkResponse();
    return _servletResponse.getCharacterEncoding();
  }

  @Override
  public String getResponseContentType()
  {
    _checkResponse();
    return _servletResponse.getContentType();
  }

  @Override
  public void setRequest(Object object)
  {
    _servletRequest = (ServletRequest) object;
    if (object instanceof HttpServletRequest)
    {
      _httpServletRequest = (HttpServletRequest) object;
      _initHttpServletRequest();
    }
    else
    {
      _httpServletRequest = null;
    }

    // And clear out any of the cached maps, since we should
    // go back and look in the map
    _requestCookieMap = null;
    _requestHeaderMap = null;
    _requestHeaderValuesMap = null;
    _requestMap = null;
    _requestParameterMap = null;
    _requestParameterValuesMap = null;
  }

  @Override
  public void setRequestCharacterEncoding(String string) throws UnsupportedEncodingException
  {
    _checkRequest();
    _servletRequest.setCharacterEncoding(string);
  }

  @Override
  public void setResponse(Object object)
  {
    _servletResponse = (ServletResponse) object;
    if (_servletResponse instanceof HttpServletResponse)
      _httpServletResponse = (HttpServletResponse) object;
    else
      _httpServletResponse = null;
  }

  @Override
  public void setResponseCharacterEncoding(String string)
  {
    _checkResponse();
    _servletResponse.setCharacterEncoding(string);
  }


  public void release()
  {
    _servletContext = null;
    _servletRequest = null;
    _servletResponse = null;
    _applicationMap = null;
    _sessionMap = null;
    _requestMap = null;
    _requestParameterMap = null;
    _requestParameterValuesMap = null;
    _requestHeaderMap = null;
    _requestHeaderValuesMap = null;
    _requestCookieMap = null;
    _initParameterMap = null;
  }

  @Override
  public String encodePartialActionURL(String url)
  {
    _checkNull(url, "url");
    _checkResponse();

    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException("Only HttpServletRequest supported");
    }
    
    return _httpServletResponse.encodeURL(url);
  }

  @Override
  public String encodeBookmarkableURL(String url, Map<String, List<String>> params)
  {
    _checkNull(url, "url");

    return _encodeURL(url, params);
  }

  @Override
  public String encodeRedirectURL(String url, Map<String, List<String>> params)
  {
    _checkResponse();
    _checkNull(url, "url");

    if (_httpServletRequest == null)
    {
      throw new IllegalArgumentException("Only HttpServletRequest supported");
    }

    if (params == null)
      return _httpServletResponse.encodeRedirectURL(url);
    else    
      return _httpServletResponse.encodeRedirectURL(_encodeURL(url, params));
  }

  private void _checkNull(final Object o, final String param)
  {
    if (o == null)
    {
      throw new NullPointerException(_LOG.getMessage(
        "CANNOT_BE_NULL", param));
    }
  }

  private void _checkRequest()
  {
    if (_servletRequest == null)
    {
      throw new UnsupportedOperationException(_LOG.getMessage(
        "NULL_REQUEST_ON_THIS_CONTEXT"));
    }
  }

  private void _checkResponse()
  {
    if (_servletResponse == null)
    {
      throw new UnsupportedOperationException(_LOG.getMessage(
        "NULL_RESPONSE_ON_THIS_CONTEXT"));
    }
  }

  private String _lookupCharacterEncoding(final String contentType)
  {
    String characterEncoding = null;

    if (contentType != null)
    {
      final int charsetFind = contentType.indexOf("charset=");
      if (charsetFind != -1)
      {
        if (charsetFind == 0)
        {
          // charset at beginning of Content-Type, curious
          characterEncoding = contentType.substring(8);
        }
        else
        {
          final char charBefore = contentType.charAt(charsetFind - 1);
          if (charBefore == ';' || Character.isWhitespace(charBefore))
          {
            // Correct charset after mime type
            characterEncoding = contentType.substring(charsetFind + 8);
          }
        }
        if (_LOG.isFine())
        {
          _LOG.fine("Incoming request has Content-Type header with character encoding "
              + characterEncoding);
        }
      }
      else
      {
        if (_LOG.isFine())
        {
          _LOG.fine("Incoming request has Content-Type header without character encoding: "
              + contentType);
        }
      }
    }
    return characterEncoding;
  }

  private void _initHttpServletRequest()
  {
    // TODO: is this necessary still?
    // HACK: MultipartWrapper scrambles the servletPath for some reason in Tomcat 4.1.29 embedded
    // in JBoss 3.2.3!?
    // (this was reported by frederic.auge [frederic.auge@laposte.net])
    _requestServletPath = _httpServletRequest.getServletPath();
    _requestPathInfo = _httpServletRequest.getPathInfo();

    final String contentType = _httpServletRequest.getHeader("Content-Type");

    String characterEncoding = _lookupCharacterEncoding(contentType);

    if (characterEncoding == null)
    {
      final HttpSession session = _httpServletRequest.getSession(false);
      if (session != null)
      {
        characterEncoding = (String) session.getAttribute(ViewHandler.CHARACTER_ENCODING_KEY);
      }

      if (characterEncoding != null)
      {
        try
        {
          _servletRequest.setCharacterEncoding(characterEncoding);
        }
        catch (UnsupportedEncodingException uee)
        {
          _LOG.warning(uee);
        }
      }
    }
  }

  /**
   * Cut-and-paste reuse from org/apache/myfaces/context/servlet/ServletExternalContextImpl.
   *
   * @param baseUrl
   * @param parameters
   * @return
   */
  private String _encodeURL(String baseUrl, Map<String, List<String>> parameters)
  {

    String fragment = null;
    String queryString = null;
    Map<String, List<String>> paramMap = new HashMap<String, List<String>>();

    //extract any URL fragment
    int index = baseUrl.indexOf(_URL_FRAGMENT_SEPERATOR);
    if (index != -1)
    {
      fragment = baseUrl.substring(index + 1);
      baseUrl = baseUrl.substring(0, index);
    }

    //extract the current query string and add the params to the paramMap
    index = baseUrl.indexOf(_URL_QUERY_SEPERATOR);
    if (index != -1)
    {
      queryString = baseUrl.substring(index + 1);
      baseUrl = baseUrl.substring(0, index);
      String[] nameValuePairs = queryString.split(_URL_PARAM_SEPERATOR);
      for (int i = 0; i < nameValuePairs.length; i++)
      {
        String[] currentPair = nameValuePairs[i].split(_URL_NAME_VALUE_PAIR_SEPERATOR);

        ArrayList<String> value = new ArrayList<String>(1);
        try
        {
          value.add(currentPair.length > 1? URLDecoder.decode(currentPair[1], getResponseCharacterEncoding()): "");
        }
        catch (UnsupportedEncodingException e)
        {
          //shouldn't ever get here
          throw new UnsupportedOperationException("Encoding type=" + getResponseCharacterEncoding() + " not supported",
                                                  e);
        }
        paramMap.put(currentPair[0], value);
      }
    }

    FacesContext context = FacesContext.getCurrentInstance();
    //add/update with new params on the paramMap
    if (parameters != null && parameters.size() > 0)
    {
      for (Map.Entry<String, List<String>> pair: parameters.entrySet())
      {
        if (pair.getKey() != null && pair.getKey().trim().length() != 0)
        {
          paramMap.put(pair.getKey(), _evaluateValueExpressions(context, pair.getValue()));
        }
      }
    }

    // start building the new URL
    StringBuilder newUrl = new StringBuilder(baseUrl);

    //now add the updated param list onto the url
    if (paramMap.size() > 0)
    {
      boolean isFirstPair = true;
      for (Map.Entry<String, List<String>> pair: paramMap.entrySet())
      {
        for (String value: pair.getValue())
        {
          if (!isFirstPair)
          {
            newUrl.append(_URL_PARAM_SEPERATOR);
          }
          else
          {
            newUrl.append(_URL_QUERY_SEPERATOR);
            isFirstPair = false;
          }

          newUrl.append(pair.getKey());
          newUrl.append(_URL_NAME_VALUE_PAIR_SEPERATOR);
          try
          {
            newUrl.append(URLEncoder.encode(value, getResponseCharacterEncoding()));
          }
          catch (UnsupportedEncodingException e)
          {
            //shouldn't ever get here
            throw new UnsupportedOperationException("Encoding type=" + getResponseCharacterEncoding() +
                                                    " not supported", e);
          }
        }
      }
    }

    //add the fragment back on (if any)
    if (fragment != null)
    {
      newUrl.append(_URL_FRAGMENT_SEPERATOR + fragment);
    }

    return newUrl.toString();
  }

  /**
   * Cut-and-paste reuse from org/apache/myfaces/context/servlet/ServletExternalContextImpl.
   *
   * Checks the Strings in the List for EL expressions and evaluates them.
   * Note that the returned List will be a copy of the given List, because
   * otherwise it will have unwanted side-effects.
   * @param context faces context
   * @param values from a query parameter that might have EL
   * @return resultant list will have any embedded EL evaluated
   */
  private List<String> _evaluateValueExpressions(FacesContext context, List<String> values)
  {
    // note that we have to create a new List here, because if we
    // change any value on the given List, it will be changed in the
    // NavigationCase too and the EL expression won't be evaluated again
    List<String> target = new ArrayList<String>(values.size());
  
    for (String value: values)
    {
      if (_isExpression(value))
      {
        if (context == null)
          throw new UnsupportedOperationException("FacesContext not established yet. Unable to resolve EL bound query" + 
                                                  "parameter value: \"" + value + "\"");
        
        Application app = context.getApplication();
        String dynamicValue = app.evaluateExpressionGet(context, value, String.class);
        target.add(dynamicValue);
      }
      else
        target.add(value);
    }
    return target;
  }

  /**
   * Cut-and-paste reuse from org/apache/myfaces/context/servlet/ServletExternalContextImpl.
   *
   * @param text of query parameter that might contain EL
   * @return <code>true</code> if the value of <code>text</code> contains an EL expression.
   */
  private boolean _isExpression(String text)
  {
    return text.indexOf("#{") != -1;
  }

  private Map<String, Object> _applicationMap;
  private HttpServletRequest _httpServletRequest;
  private HttpServletResponse _httpServletResponse;
  private Map<String, String> _initParameterMap;
  private Map<String, Object> _requestCookieMap;
  private Map<String, String> _requestHeaderMap;
  private Map<String, String[]> _requestHeaderValuesMap;
  private Map<String, Object> _requestMap;
  private Map<String, String> _requestParameterMap;
  private Map<String, String[]> _requestParameterValuesMap;
  private String _requestPathInfo;
  private String _requestServletPath;
  private ServletContext _servletContext;
  private ServletRequest _servletRequest;
  private ServletResponse _servletResponse;
  private Map<String, Object> _sessionMap;


  private static final String _URL_PARAM_SEPERATOR = "&";
  private static final String _URL_QUERY_SEPERATOR = "?";
  private static final String _URL_FRAGMENT_SEPERATOR = "#";
  private static final String _URL_NAME_VALUE_PAIR_SEPERATOR = "=";


  private static final String _INIT_PARAMETER_MAP_ATTRIBUTE = ServletInitParameterMap.class.getName();
  private static final String _CHAR_ENCODING_CALLED = ServletExternalContext.class.getName() + ".CHAR_ENCODING_CALLED";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ServletExternalContext.class);

}