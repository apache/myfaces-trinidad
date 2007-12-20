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
package org.apache.myfaces.trinidadinternal.application;

import java.io.InputStream;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import javax.faces.FacesException;
import javax.faces.application.ViewHandler;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.ExtendedRenderKitService;
import org.apache.myfaces.trinidad.render.InternalView;
import org.apache.myfaces.trinidad.util.Service;
import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;
import org.apache.myfaces.trinidadinternal.context.TrinidadPhaseListener;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.util.URLUtils;

/**
 * ViewHandler that adds modification detection to the existing ViewHandler,
 * assuming that the viewId is a valid resource path.
 * <p>
 * And now also supports inserting URLs tokens to preserve PageFlowScope.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/application/ViewHandlerImpl.java#0 $) $Date: 05-jan-2006.13:19:09 $
 * @todo Rename something less generic
 * @todo Support extension mapping (*.faces)
 * @todo The modification detection only works for a single user.  That's
 *   OK for now, because it's intended for use while developing, not while
 *   deployed - yet it's on all the time.  Hrm.
 */
public class ViewHandlerImpl extends ViewHandler
{
  static public final String ALTERNATE_VIEW_HANDLER =
    "org.apache.myfaces.trinidad.ALTERNATE_VIEW_HANDLER";

  public ViewHandlerImpl(
    ViewHandler delegate)
  {
    _delegate = delegate;
    _timestamps = new HashMap<String, Long>();
    _loadInternalViews();
  }

  @Override
  public Locale calculateLocale(FacesContext context)
  {
    return _delegate.calculateLocale(context);
  }

  @Override
  public String calculateRenderKitId(FacesContext context)
  {
    return _delegate.calculateRenderKitId(context);
  }

  @Override
  public UIViewRoot createView(FacesContext context, String viewId)
  {
    _initIfNeeded(context);

    InternalView internal = _getInternalView(context, viewId);
    if (internal != null)
    {
      UIViewRoot root = internal.createView(context, viewId);
      if (root != null)
        return root;
      // Otherwise, fall through to default processing
    }
    else if (_checkTimestamp(context))
    {
      try
      {
        // Check the timestamp on the physical path
        String path = _getPath(viewId);
        synchronized (_timestamps)
        {
          Long ts = _timestamps.get(path);
          if (ts != _NOT_FOUND)
          {
            URL url = context.getExternalContext().getResource(path);
            Long modified = _getLastModified(url);
            _timestamps.put(path, modified);
          }
        }
      }
      catch (IOException e)
      {
        _LOG.severe(e);
      }
    }

    return _delegate.createView(context, viewId);
  }

  @Override
  public String getActionURL(FacesContext context, String viewId)
  {
    String actionURL = _delegate.getActionURL(context, viewId);
    RequestContext afContext = RequestContext.getCurrentInstance();
    if (afContext != null)
    {
      actionURL = afContext.getPageResolver().encodeActionURI(actionURL);
      actionURL = afContext.getPageFlowScopeProvider().
                     encodeCurrentPageFlowScopeURL(context, actionURL);
    }

    return actionURL;
  }

  @Override
  public String getResourceURL(
    FacesContext context,
    String       path)
  {
    return _delegate.getResourceURL(context, path);
  }


  @Override
  public void renderView(
    FacesContext context,
    UIViewRoot   viewToRender) throws IOException, FacesException
  {
    _initIfNeeded(context);

    // See if there is a possiblity of short-circuiting the current
    // Render Response
    ExtendedRenderKitService service = _getExtendedRenderKitService(context);
    if ((service != null) &&
        service.shortCircuitRenderView(context))
    {
      // Yup, we don't need to do anything
      ;
    }
    else
    {
      try
      {
        if (service != null)
          service.encodeBegin(context);

        InternalView internal = _getInternalView(context,
                                                 viewToRender.getViewId());
        if (internal != null)
        {
          internal.renderView(context, viewToRender);
        }
        else
        {
          _delegate.renderView(context, viewToRender);
        }

        if (service != null)
          service.encodeEnd(context);
      }
      finally
      {
        if (service != null)
          service.encodeFinally(context);
      }
    }
  }

  @Override
  public UIViewRoot restoreView(
    FacesContext context,
    String       viewId)
  {
    // If we're being asked to re-run the lifecycle for a "return"
    // event, always restore the "launch view", which was set
    // over in RequestContextImpl
    UIViewRoot launchView = (UIViewRoot)
      context.getExternalContext().getRequestMap().get(
        RequestContextImpl.LAUNCH_VIEW);

    if (launchView != null)
    {
      context.getExternalContext().getRequestMap().remove(
        RequestContextImpl.LAUNCH_VIEW);
      TrinidadPhaseListener.markPostback(context);
      return launchView;
    }

    InternalView internal = _getInternalView(context, viewId);
    if (internal != null)
    {
      return internal.restoreView(context, viewId);
    }

    boolean uptodate = true;

    if (_checkTimestamp(context))
    {
      try
      {
        // Check the timestamp on the physical path
        String path = _getPath(viewId);
        synchronized (_timestamps)
        {
          Long ts = _timestamps.get(path);
          if (ts != _NOT_FOUND)
          {
            URL url = context.getExternalContext().getResource(path);
            Long modified = _getLastModified(url);
            if (modified == _NOT_FOUND)
            {
              _timestamps.put(path, _NOT_FOUND);
            }
            else if ((ts == null) ||
                     (modified.longValue() > ts.longValue()))
            {
              _timestamps.put(path, modified);
              if (ts != null)
              {
                _LOG.fine("View document \"" + path + "\" has been modified, " +
                          "ignoring postback for view \"" + viewId +"\"");
              }
              uptodate = false;
            }
          }
        }
      }
      catch (IOException e)
      {
        _LOG.severe(e);
      }
    }

    if (!uptodate)
    {
      return null;
    }

    UIViewRoot result = _delegate.restoreView(context, viewId);
    // If we've successfully restored a view, then assume that
    // this is a postback request.
    if (result != null)
    {
      TrinidadPhaseListener.markPostback(context);
    }

    return result;
  }

  @Override
  public void writeState(
    FacesContext context) throws IOException
  {
    String viewId = context.getViewRoot().getViewId();
    InternalView internal =
       _getInternalView(context, viewId);

    // As internal views whether they're stateless.  If they are, don't
    // bother writing anything out.
    if ((internal != null) && internal.isStateless(context, viewId))
      return;

    ExtendedRenderKitService service = _getExtendedRenderKitService(context);
    if ((service != null) &&
        service.isStateless(context))
      return;

    _delegate.writeState(context);
  }

  synchronized private void _initIfNeeded(FacesContext context)
  {
    if (!_inited)
    {
      _inited = true;
      String alternateViewHandler =
        context.getExternalContext().getInitParameter(ALTERNATE_VIEW_HANDLER);
      if (alternateViewHandler != null)
      {
        ViewHandler viewHandlerInstance = null;
        try
        {
          ClassLoader loader = Thread.currentThread().getContextClassLoader();
          Class<?> c = loader.loadClass(alternateViewHandler);
          try
          {
            Constructor<?> constructor = c.getConstructor(
               new Class[]{ViewHandler.class});
            viewHandlerInstance =
               (ViewHandler) constructor.newInstance(new Object[]{_delegate});
          }
          catch (NoSuchMethodException nsme)
          {
            viewHandlerInstance = (ViewHandler) c.newInstance();
          }
        }
        catch (Exception e)
        {
          _LOG.warning("CANNOT_LOAD_VIEWHANDLER", alternateViewHandler);
          _LOG.warning(e);
        }

        if (viewHandlerInstance != null)
          _delegate = viewHandlerInstance;
      }
    }
  }

  private ExtendedRenderKitService _getExtendedRenderKitService(
    FacesContext context)
  {
    return Service.getService(context.getRenderKit(),
                              ExtendedRenderKitService.class);
  }

  private boolean _checkTimestamp(FacesContext context)
  {
    if (_checkTimestamp == null)
    {
      String checkTimestamp =
        context.getExternalContext().getInitParameter(Configuration.CHECK_TIMESTAMP_PARAM);
      // Detect when we're running inside of the JDeveloper embedded OC4J
      // environment - and there, always use timestamp checking
      // TODO: come up with a non-proprietary way of checking this?
      boolean performCheck = "true".equals(checkTimestamp) ||
        "development".equals(System.getProperty("oracle.application.environment"));
      _checkTimestamp = Boolean.valueOf(performCheck);
      if ("true".equals(checkTimestamp))
      {
        _LOG.info("TIMESTAMP_CHECKING_ENABLED_SHOULDNOT_IN_PRODUCTION",
                  Configuration.CHECK_TIMESTAMP_PARAM);
      }
    }

    return _checkTimestamp.booleanValue();
  }


  /**
   * Return the physical path of a particular URI
   */
  static private String _getPath(String uri)
  {
    RequestContext afc = RequestContext.getCurrentInstance();
    if (afc != null)
    {
      return afc.getPageResolver().getPhysicalURI(uri);
    }

    // No RequestContext?  Just return the URI
    return uri;
  }


  private Long _getLastModified(URL url) throws IOException
  {
    if (url == null)
      return _NOT_FOUND;

    return Long.valueOf(URLUtils.getLastModified(url));
  }


  private InternalView _getInternalView(
    FacesContext context, 
    String       viewId)
  {
    InternalView internal = _internalViews.get(viewId);
    if (internal == null)
    {
      // If we're using suffix-mapping, then any internal viewId will
      // get affixed with ".jsp" or ".jspx";  try trimming that off
      // if present
      ExternalContext external = context.getExternalContext();
      
      // Only bother when using suffix-mapping (path info will always
      // be non-null for prefix-mapping)
      if (external.getRequestPathInfo() == null)
      {
        String suffix = external.getInitParameter("javax.faces.DEFAULT_SUFFIX");
        if (suffix == null)
          suffix = ".jspx";
        
        if (viewId.endsWith(suffix))
        {
          String viewIdWithoutSuffix = viewId.substring(
             0, viewId.length() - suffix.length());
          internal = _internalViews.get(viewIdWithoutSuffix);
        }
      }
    }

    return internal;
  }

  //
  // Load the META-INF/org.apache.myfaces.trinidad.render.InternalView.properties
  // files.
  //
  private void _loadInternalViews()
  {
    _internalViews = new HashMap<String, InternalView>();
    List<URL> list = new ArrayList<URL>();
    ClassLoader loader = _getClassLoader();
    try
    {
      Enumeration<URL> en = loader.getResources(
               "META-INF/org.apache.myfaces.trinidad.render.InternalView.properties");
      while (en.hasMoreElements())
      {
        list.add(en.nextElement());
      }

      // And, for some temporary backwards compatibility, also load
      // the incorrect properties without "render"
      en = loader.getResources(
               "META-INF/org.apache.myfaces.trinidad.InternalView.properties");
      while (en.hasMoreElements())
      {
        list.add(en.nextElement());
      }


      // Reverse the list so it is in the proper order (most local
      // entry "wins")
      Collections.reverse(list);
    }
    catch (IOException ioe)
    {
      _LOG.severe(ioe);
    }

    for (URL url : list)
    {
      try
      {
        Properties properties = new Properties();
        _LOG.fine("Loading internal views from {0}",  url);
        InputStream is = url.openStream();
        try
        {
          properties.load(is);
        }
        finally
        {
          is.close();
        }

        for (Map.Entry<Object, Object> entry : properties.entrySet())
        {
          String name = (String) entry.getKey();
          String className = (String) entry.getValue();
          Class<?> clazz = loader.loadClass(className);
          InternalView view = (InternalView) clazz.newInstance();
          _internalViews.put(name, view);
        }
      }
      catch (IllegalAccessException iae)
      {
        _LOG.severe("CANNOT_LOAD_URL", url);
        _LOG.severe(iae);
      }
      catch (InstantiationException ie)
      {
        _LOG.severe("CANNOT_LOAD_URL", url);
        _LOG.severe(ie);
      }
      catch (ClassNotFoundException cnfe)
      {
        _LOG.severe("CANNOT_LOAD_URL", url);
        _LOG.severe(cnfe);
      }
      catch (IOException ioe)
      {
        _LOG.severe("CANNOT_LOAD_URL", url);
        _LOG.severe(ioe);
      }
    }
  }


  static private ClassLoader _getClassLoader()
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      loader = ViewHandlerImpl.class.getClassLoader();
    return loader;
  }

  private Boolean           _checkTimestamp;
  // Mostly final, but see _initIfNeeded()
  private ViewHandler       _delegate;
  private final Map<String, Long> _timestamps;
  private boolean           _inited;
  private Map<String, InternalView> _internalViews;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ViewHandlerImpl.class);
  private static final Long   _NOT_FOUND = Long.valueOf(0);
}
