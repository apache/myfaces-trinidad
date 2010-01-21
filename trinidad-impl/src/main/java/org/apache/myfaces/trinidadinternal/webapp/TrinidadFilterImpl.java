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

import java.io.IOException;
import java.util.List;
import java.util.Map;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidadinternal.config.GlobalConfiguratorImpl;
import org.apache.myfaces.trinidadinternal.config.dispatch.DispatchResponseConfiguratorImpl;
import org.apache.myfaces.trinidadinternal.config.dispatch.DispatchServletResponse;
import org.apache.myfaces.trinidadinternal.config.upload.FileUploadConfiguratorImpl;
import org.apache.myfaces.trinidadinternal.config.upload.UploadRequestWrapper;
import org.apache.myfaces.trinidadinternal.config.xmlHttp.XmlHttpConfigurator;
import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;
import org.apache.myfaces.trinidadinternal.context.external.ServletExternalContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.webapp.wrappers.BasicHTMLBrowserRequestWrapper;

/**
 * Actual implementation of the Trinidad servlet filter.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/webapp/AdfFacesFilterImpl.java#0 $) $Date: 10-nov-2005.18:48:59 $
 * @todo Allow configuration of the maximum allowed number of bytes in
 *   an entire request
 */
public class TrinidadFilterImpl implements Filter
{
  static public void verifyFilterIsInstalled(FacesContext context)
  {
    Object isInstalled =
      context.getExternalContext().getRequestMap().get(_FILTER_EXECUTED_KEY);
    if (!Boolean.TRUE.equals(isInstalled))
    {
      _LOG.warning("REQUIRED_TRINIDADFILTER_NOT_INSTALLED");

    }
  }

  static public FacesContext getPseudoFacesContext()
  {
    return _PSEUDO_FACES_CONTEXT.get();
  }

  /**
   * Returns true if the filter is in the middle of executing the
   * "return from dialog"
   */
  static public boolean isExecutingDialogReturn(FacesContext context)
  {
    return Boolean.TRUE.equals(
      context.getExternalContext().getRequestMap().get(_IS_RETURNING_KEY));
  }

  public void init(FilterConfig filterConfig) throws ServletException
  {
    _servletContext = filterConfig.getServletContext();
            
    //There is some functionality that still might require servlet-only filter services.
    _filters = ClassLoaderUtils.getServices(TrinidadFilterImpl.class.getName());
    for(Filter f:_filters)
    {
      f.init(filterConfig);
    }
  }

  public void destroy()
  {
    //Destroy filter services
    for(Filter f:_filters)
    {
      f.destroy();
    }

    _filters = null;
  }

  @SuppressWarnings("unchecked")
  public void doFilter(
    ServletRequest  request,
    ServletResponse response,
    FilterChain     chain) throws IOException, ServletException
  {
     //Execute the filter services
    if (!_filters.isEmpty())
      chain = new FilterListChain(_filters, chain);

    // Set a flag so that we can detect if the filter has been
    // properly installed.
    request.setAttribute(_FILTER_EXECUTED_KEY, Boolean.TRUE);

    ExternalContext externalContext = new ServletExternalContext(_servletContext, request, response);    
    GlobalConfiguratorImpl config = GlobalConfiguratorImpl.getInstance();
    config.beginRequest(externalContext);
    
    String noJavaScript = request.getParameter(XhtmlConstants.NON_JS_BROWSER);
        
    //Wrap the request only for Non-javaScript browsers
    if(noJavaScript != null &&
                 XhtmlConstants.NON_JS_BROWSER_TRUE.equals(noJavaScript))
    {
      request = new BasicHTMLBrowserRequestWrapper((HttpServletRequest)request);
    } 
    
    //To maintain backward compatibilty, wrap the request at the filter level
    Map<String, String[]> addedParams = FileUploadConfiguratorImpl.getAddedParameters(externalContext);
    
    boolean isPartialRequest;
    if(addedParams != null)
    {
      FileUploadConfiguratorImpl.apply(externalContext);
      request = new UploadRequestWrapper((HttpServletRequest)request, addedParams);
      isPartialRequest = CoreRenderKit.isPartialRequest(addedParams);
    }
    else
    {
      // Only test for AJAX request, since file uploads *should* be
      // handled by the above test.  NOTE: this will not necessarily
      // work if someone is using Trinidad PPR with non-Trinidad 
      // file upload!  I've no idea currently how to cleanly handle
      // that combination in JSF 1.1.  We don't want to ask if
      // its a partial request here, as this requires getting a
      // query parameter, but that could block setting
      // the character set later in the request in some app servers!
      isPartialRequest = CoreRenderKit.isAjaxRequest(externalContext);
      if (isPartialRequest)
      {
        request = XmlHttpConfigurator.getAjaxServletRequest(request);
      }
    }

    if (isPartialRequest)
    {
      XmlHttpConfigurator.beginRequest(externalContext);
      response = XmlHttpConfigurator.getWrappedServletResponse(response);
    }

    try
    {
      
      _doFilterImpl(request, response, chain);
    }
    catch (Throwable t)
    {
      if (isPartialRequest)
      {
        XmlHttpConfigurator.handleError(externalContext, t);
      }
      else
      {
        // For non-partial requests, just re-throw.  It is not
        // our responsibility to catch these
        if (t instanceof RuntimeException)
          throw ((RuntimeException) t);
        if (t instanceof Error)
          throw ((Error) t);
        if (t instanceof IOException)
          throw ((IOException) t);
        if (t instanceof ServletException)
          throw ((ServletException) t);

        // Should always be one of those four types to have
        // gotten here.
        _LOG.severe(t);
      }

    }
    finally
    {
      config.endRequest(externalContext);
    }
  }


  @SuppressWarnings("unchecked")
  private void _doFilterImpl(
    ServletRequest  request,
    ServletResponse response,
    FilterChain     chain) throws IOException, ServletException
  {
    // -= Scott O'Bryan =-
    // Added for backward compatibility
    ExternalContext ec = new ServletExternalContext(_servletContext, request, response);
    HttpServletResponse dispatch = new DispatchServletResponse(ec);
    DispatchResponseConfiguratorImpl.apply(ec);

    _invokeDoFilter(request, dispatch, chain);

    // If there are existing "launchParameters", then that means
    // we've returned from a "launch", and we need to re-execute the
    // faces lifecycle.  ViewHandlerImpl will be responsible for ensuring
    // that we re-execute the lifecycle on the correct page.
    // -= Simon Lessard =-
    // FIXME: Using <String, String[]> for now to accomodate ReplaceParametersRequestWrapper.
    //        However, the Servlet specification suggest <String, Object> so this 
    //        could lead to some nasty problems one day. Especially if JEE spec includes 
    //        generics for its Servlet API soon.
    //
    // -= Scott O'Bryan =- 
    // TODO: The following should be made available to the Portal.  This is not trivial 
    //       because this just re-invokes the filter chain with a new set of parameters.
    //       In the portal environment, this must rerun the portlet without the use of 
    //       filters until Portlet 2.0.
    Map<String, String[]> launchParameters = (Map<String, String[]>)
      request.getAttribute(RequestContextImpl.LAUNCH_PARAMETERS);
    if (launchParameters != null)
    {
      request.removeAttribute(RequestContextImpl.LAUNCH_PARAMETERS);
      request.setAttribute(_IS_RETURNING_KEY, Boolean.TRUE);
      request = new ReplaceParametersRequestWrapper(
               (HttpServletRequest) request, launchParameters);
      _invokeDoFilter(request, dispatch, chain);
      request.removeAttribute(_IS_RETURNING_KEY);
    }
  }

  private void _invokeDoFilter(
    ServletRequest  request,
    ServletResponse response,
    FilterChain     chain) throws IOException, ServletException
  {
    // Set up a PseudoFacesContext with the actual request and response
    // so that RequestContext can be more functional in the interval
    // between now and when the FacesServlet starts.
    PseudoFacesContext pfc = new PseudoFacesContext(
      new ServletExternalContext(_servletContext, request, response));
    _PSEUDO_FACES_CONTEXT.set(pfc);
    try
    {
      chain.doFilter(request, response);
    }
    finally
    {
      _PSEUDO_FACES_CONTEXT.remove();
    }
  }


  private static final class FilterListChain implements FilterChain
  {
    private final List<Filter> _filters;
    private final FilterChain _last;
    private final int _index;
    
    public FilterListChain(List<Filter> filters, FilterChain last)
    {
      this(filters, last, 0);
    }

    private FilterListChain(List<Filter> filters, FilterChain last, int index)
    {
      assert index < filters.size();
      _filters = filters;
      _last = last;
      _index = index;
    }
    
    public void doFilter(ServletRequest request, ServletResponse response)
      throws IOException, ServletException
    {
      int nextIndex = _index+1;
      final FilterChain next;
      // if there are more filters to chain, then keep using
      // FilterListChain; otherwise, just use the last chain:
      if (nextIndex < _filters.size())
        next = new FilterListChain(_filters, _last, nextIndex);
      else
        next = _last;

      _filters.get(_index).doFilter(request, response, next);
    }
  }

  private ServletContext _servletContext;
  private List<Filter> _filters = null;

  private static final String _IS_RETURNING_KEY =
    "org.apache.myfaces.trinidadinternal.webapp.AdfacesFilterImpl.IS_RETURNING";
  private static final String _FILTER_EXECUTED_KEY =
    "org.apache.myfaces.trinidadinternal.webapp.AdfacesFilterImpl.EXECUTED";

  private static ThreadLocal<PseudoFacesContext> _PSEUDO_FACES_CONTEXT = 
    new ThreadLocal<PseudoFacesContext>()
  {
    @Override
    protected PseudoFacesContext initialValue() { return null; }
  };

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(TrinidadFilterImpl.class);
}
