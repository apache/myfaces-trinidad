/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.webapp;

import java.io.IOException;
import java.io.InputStream;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.context.FacesContext;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.RequestContextFactory;
import org.apache.myfaces.trinidad.model.UploadedFile;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

import org.apache.myfaces.trinidadinternal.context.RequestContextFactoryImpl;
import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;
import org.apache.myfaces.trinidadinternal.context.TrinidadPhaseListener;
import org.apache.myfaces.trinidadinternal.share.util.MultipartFormHandler;
import org.apache.myfaces.trinidadinternal.share.util.MultipartFormItem;
import org.apache.myfaces.trinidadinternal.skin.SkinFactory;
import org.apache.myfaces.trinidadinternal.skin.SkinFactoryImpl;
import org.apache.myfaces.trinidadinternal.skin.SkinUtils;

/**
 * Actual implementation of the ADF Faces servlet filter.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/webapp/AdfFacesFilterImpl.java#0 $) $Date: 10-nov-2005.18:48:59 $
 * @author The Oracle ADF Faces Team
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
      _LOG.warning("The AdfFacesFilter has not been installed.  ADF Faces " +
                   "requires this filter for proper execution.");

    }
  }

  static public FacesContext getPseudoFacesContext()
  {
    return (FacesContext) _PSEUDO_FACES_CONTEXT.get();
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



  /**
   * Sets the maximum number of bytes that MultipartFormItem.writeFile()
   * will be allowed to write.  This value may be set immediately
   * before or between calls to MultipartFormItem.writeFile().  If
   * any call to writeFile() exceeds this value, an EOFException
   * will be thrown.
   * <p>
   * @param maxAllowedBytes the maximum number of bytes that
   * MultipartFormItem.writeFile() will be allowed to write.  Defaults
   * to 128MB.
   * @see org.apache.myfaces.trinidadinternal.share.util.MultipartFormItem#writeFile
   */
  public void setMaximumAllowedBytes(long maxAllowedBytes)
  {
    _maxAllowedBytes = Math.max(0L, maxAllowedBytes);
  }

  /**
   * Gets the maximum number of bytes that MultipartFormItem.writeFile()
   * will be allowed to write.
   */
  public long getMaximumAllowedBytes()
  {
    return _maxAllowedBytes;
  }

  public void init(
    FilterConfig filterConfig) throws ServletException
  {
    if (RequestContextFactory.getFactory() == null)
      RequestContextFactory.setFactory(new RequestContextFactoryImpl());

    _servletContext = filterConfig.getServletContext();

    // Create a new SkinFactory if needed.
    if (SkinFactory.getFactory() == null)
      SkinFactory.setFactory(new SkinFactoryImpl());

    // register the base skins
    SkinUtils.registerBaseSkins();
    
    _filters = ClassLoaderUtils.getServices(TrinidadFilterImpl.class.getName());
    for(Filter f:_filters)
    {
      f.init(filterConfig);
    }
    // after the 'services' filters are initialized, then register
    // the skin extensions found in adf-faces-skins.xml. This
    // gives a chance to the 'services' filters to create more base
    // skins that the skins in adf-faces-skins.xml can extend.
    SkinUtils.registerSkinExtensions(_servletContext);

    
  }

  public void destroy()
  {
    for(Filter f:_filters)
    {
      f.destroy();
    }
    _filters = null;
  }

  public void doFilter(
    ServletRequest  request,
    ServletResponse response,
    FilterChain     chain) throws IOException, ServletException
  {
    if (!_filters.isEmpty())
      chain = new FilterListChain(_filters, chain);

    // Set a flag so that we can detect if the filter has been
    // properly installed.
    request.setAttribute(_FILTER_EXECUTED_KEY, Boolean.TRUE);


    // If someone didn't release the RequestContext on an earlier request,
    // then it'd still be around, and trying to create a new one
    // would trigger an exception.  We don't want to take down
    // this thread for all eternity, so clean up after poorly-behaved code.
    RequestContext context = RequestContext.getCurrentInstance();
    if (context != null)
    {
      if (_LOG.isWarning())
        _LOG.warning("RequestContext had not been properly released on earlier " +
                     "request.");
      context.release();
    }

    RequestContextFactory factory = RequestContextFactory.getFactory();
    assert(factory != null);

    // See if we've got a cached RequestContext instance;  if so,
    // reattach it
    Object cachedRequestContext = 
      request.getAttribute(TrinidadPhaseListener.CACHED_ADF_FACES_CONTEXT);

    // Catch both the null scenario and the 
    // RequestContext-from-a-different-classloader scenario
    if (cachedRequestContext instanceof RequestContext)
    {
      context = (RequestContext) cachedRequestContext;
      context.attach();
    }
    else
    {
      context = factory.createContext(_servletContext, request);
      request.setAttribute(TrinidadPhaseListener.CACHED_ADF_FACES_CONTEXT,
                           context);
    }

    assert(RequestContext.getCurrentInstance() == context);

    try
    {
      // Only handle multipart and HTTP requests
      if (!MultipartFormHandler.isMultipartRequest(request) ||
          !(request instanceof HttpServletRequest))
      {
        _doFilterImpl(request, response, chain);
      }
      else
      {
        MultipartFormHandler mfh = new MultipartFormHandler(request);
        mfh.setMaximumAllowedBytes(getMaximumAllowedBytes());
        mfh.setCharacterEncoding(request.getCharacterEncoding());

        HashMap parameters = new HashMap();

        // Copy over all parameters that were already present (for example,
        // query parameters)
        parameters.putAll(request.getParameterMap());

        MultipartFormItem item;
        UploadedFiles files = new UploadedFiles(request);
        while ((item = mfh.getNextPart()) != null)
        {
          String name = item.getName();
          String value = null;
          // No filename - it's not a file uploaded field
          if (item.getFilename() == null)
          {
            value = item.getValue();
            Object oldValue = parameters.get(name);
            if (oldValue == null)
            {
              parameters.put(name, new String[]{value});
            }
            else
            {
              String[] oldArray = (String[]) oldValue;
              String[] newArray = new String[oldArray.length + 1];
              System.arraycopy(oldArray, 0, newArray, 1, oldArray.length);
              newArray[0] = value;
              parameters.put(name, newArray);
            }
          }
          // Upload a file
          else if (item.getFilename().length() > 0)
          {
            _doUploadFile(context, request, files, item);
          }
        }

        request = new UploadRequestWrapper((HttpServletRequest) request,
                                           parameters);

        _doFilterImpl(request, response, chain);

        files.dispose();
      }
    }
    finally
    {
      if (context != null)
      {
        context.release();
        assert(RequestContext.getCurrentInstance() == null);
      }
    }
  }


  private void _doFilterImpl(
    ServletRequest  request,
    ServletResponse response,
    FilterChain     chain) throws IOException, ServletException
  {
    HttpServletResponse monitor
      = new MonitorRedirectServletResponse((HttpServletResponse) response,
                                           request);

    ServletResponse dispatch
      = new DispatchServletResponse(monitor, (HttpServletRequest)request);

    _invokeDoFilter(request, dispatch, chain);

    // If there are existing "launchParameters", then that means
    // we've returned from a "launch", and we need to re-execute the
    // faces lifecycle.  ViewHandlerImpl will be responsible for ensuring
    // that we re-execute the lifecycle on the correct page.
    Map launchParameters = (Map)
      request.getAttribute(RequestContextImpl.LAUNCH_PARAMETERS);
    if (launchParameters != null)
    {
      request.removeAttribute(RequestContextImpl.LAUNCH_PARAMETERS);
      request.setAttribute(_IS_RETURNING_KEY, Boolean.TRUE);
      request = new ReplaceParametersRequestWrapper(
               (HttpServletRequest) request, launchParameters);
      _invokeDoFilter(request, monitor, chain);
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
    PseudoFacesContext pfc = new PseudoFacesContext(request, response);
    _PSEUDO_FACES_CONTEXT.set(pfc);
    try
    {
      chain.doFilter(request, response);
    }
    finally
    {
      _PSEUDO_FACES_CONTEXT.set(null);
    }
  }

  private void _doUploadFile(
    RequestContext   context,
    ServletRequest    request,
    UploadedFiles     files,
    MultipartFormItem item) throws IOException
  {
    UploadedFile temp = new TempUploadedFile(item);

    UploadedFile file =
      context.getUploadedFileProcessor().processFile(request, temp);

    if (file != null)
    {
      // Store the file.
      files.__put(item.getName(), file);

      if (_LOG.isFine())
        _LOG.fine("Uploaded file " + file.getFilename() + "(" +
                  file.getLength() + " bytes) for ID " + item.getName());
    }
  }

  /**
   * A ServletResponseWrapper that will catch partial page redirects
   * and handle them correctly.
   */
  private class MonitorRedirectServletResponse
             extends HttpServletResponseWrapper
  {
    public MonitorRedirectServletResponse(HttpServletResponse response,
                                          ServletRequest request)
    {
      super(response);
      _request = request;
    }

    public void sendRedirect(String location)
      throws java.io.IOException
    {
      // We're only interested in redirects from partial events
      Object partial = _request.getParameter("partial");
      if ((partial != null) && ("true".equals(partial)))
      {
        ServletOutputStream out = getOutputStream();
        // set up to take the script
        setContentType("text/html");
        // send down a redirect script to the IFrame
        out.print(_IFRAME_REDIRECT_START + location + _IFRAME_REDIRECT_FINISH);
        // done
        out.flush();
      }
      else
        super.sendRedirect(location);
    }
    private ServletRequest _request;
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

  static private class TempUploadedFile implements UploadedFile
  {
    public TempUploadedFile(MultipartFormItem item)
    {
      _item = item;
      assert(item.getValue() == null);
    }

    public String getFilename()
    {
      return _item.getFilename();
    }

    public String getContentType()
    {
      return _item.getContentType();
    }

    public long getLength()
    {
      // The length is not known yet.
      return -1L;
    }

    public Object getOpaqueData()
    {
      return null;
    }

    public InputStream getInputStream() throws IOException
    {
      return _item.getInputStream();
    }

    public void dispose()
    {
      throw new UnsupportedOperationException();
    }

    private MultipartFormItem _item;
  }

  private long _maxAllowedBytes = 1L << 27;

  private ServletContext _servletContext;
  private List<Filter> _filters = null;

  private static final String _IS_RETURNING_KEY =
    "org.apache.myfaces.trinidadinternal.webapp.AdfacesFilterImpl.IS_RETURNING";
  private static final String _FILTER_EXECUTED_KEY =
    "org.apache.myfaces.trinidadinternal.webapp.AdfacesFilterImpl.EXECUTED";

  // two parts of a script that will redirect the main page from the IFrame.
  private static final String _IFRAME_REDIRECT_START
    = "<html><script>if(parent._pprUpdateMode)parent.location.href='";
  private static final String _IFRAME_REDIRECT_FINISH
    = "';</script></html>";

  private static ThreadLocal _PSEUDO_FACES_CONTEXT = new ThreadLocal()
  {
    protected Object initialValue() { return (null); }
  };

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TrinidadFilterImpl.class);
}
