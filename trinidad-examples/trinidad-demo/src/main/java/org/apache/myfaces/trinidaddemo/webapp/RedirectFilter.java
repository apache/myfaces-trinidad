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
package org.apache.myfaces.trinidaddemo.webapp;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet filter that ensures the FacesServlet is called before rendering
 * the page.  Most useful for getting JSP Faces pages to work correctly.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-demo/src/main/java/oracle/adfdemo/view/faces/webapp/RedirectFilter.java#1 $) $Date: 16-aug-2005.15:12:33 $
 */
public class RedirectFilter implements Filter 
{
  public static final String URL_PATTERN_PARAM = "faces-servlet-url-pattern";
  public static final String DEFAULT_URL_PATTERN = "/faces/*";

  public void init(
    FilterConfig filterConfig) throws ServletException
  {
    String pattern = filterConfig.getInitParameter(URL_PATTERN_PARAM);
    
    if (pattern == null)
      pattern = DEFAULT_URL_PATTERN;
      
    int offset = pattern.indexOf('*');
    if (offset > 1 && pattern.charAt(offset - 1) == '/')
      offset--;
      
    _servletPath = pattern.substring(0, offset);
  }

  public void destroy()
  {
    // Technically, we should dump _servletPath.  However,
    // OC4J is calling destroy(), then not calling init() before
    // using the Filter again!  So ignore destroy().
    //    _servletPath = null;
  }

  public void doFilter(
    ServletRequest  request, 
    ServletResponse response, 
    FilterChain     chain) throws IOException, ServletException
  {
    if (request instanceof HttpServletRequest)
    {
      HttpServletRequest httpRequest = (HttpServletRequest) request;
      String servletPath = httpRequest.getServletPath(); 
      if (!servletPath.startsWith(_servletPath))
      {
        servletPath = _servletPath + servletPath;
        String pathInfo = httpRequest.getPathInfo();
        String queryString = httpRequest.getQueryString();
        // Use a client-side redirect
        String url =
          (httpRequest.getContextPath() +
           servletPath + 
           (pathInfo == null ? "": pathInfo) +
           (queryString == null ? "": queryString));
        
        // Use a client-side redirect so that filters will run
        ((HttpServletResponse) response).sendRedirect(url);
        /*
          request.getRequestDispatcher(servletPath).
          forward(request, response);
        */
        return;
      }
    }

    chain.doFilter(request, response);
  }

  private String _servletPath;
}
