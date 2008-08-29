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
package org.apache.myfaces.trinidad.util;

import java.io.IOException;
import java.io.InputStream;

import javax.faces.context.ExternalContext;
import javax.portlet.ActionRequest;
import javax.portlet.PortletContext;
import javax.portlet.PortletRequest;
import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * This provides some functionality for determining some things about the
 * native request object that is not provided by the base utils.
 *
 * @version $Revision$ $Date$
 */
public class ExternalContextUtils
{

  /**
   * Returns the session ID for the client, or null if there is none.
   *
   * @param externalContext
   * @return
   */
  public static String getRequestedSessionId(final ExternalContext externalContext)
  {
    if (isPortlet(externalContext))
    {
      return ((PortletRequest) externalContext.getRequest()).getRequestedSessionId();
    }
    else
    {
      return ((HttpServletRequest) externalContext.getRequest()).getRequestedSessionId();
    }
  }

  /**
   * Checks if the requested session ID is still valid
   *
   * @param externalContext
   * @return
   */
  public static boolean isRequestedSessionIdValid(final ExternalContext externalContext)
  {
    if (isPortlet(externalContext))
    {
      return ((PortletRequest) externalContext.getRequest()).isRequestedSessionIdValid();
    }
    else
    {
      return ((HttpServletRequest) externalContext.getRequest()).isRequestedSessionIdValid();
    }
  }

  /**
   * Returns the character encoding or null if there isn't any
   *
   * @param externalContext
   * @return
   */
  public static String getCharacterEncoding(final ExternalContext externalContext)
  {
    if (isAction(externalContext))
    {
      try
      {
        if (isPortlet(externalContext))
        {
          // Allows us to not have the portal api's in the classpath
          return _getPortletCharacterEncoding(externalContext.getRequest());
        }
        else
        {
          return ((ServletRequest) externalContext.getRequest()).getCharacterEncoding();
        }
      }
      catch (final ClassCastException e)
      {
        _LOG.severe(e);
      }
    }

    return null;
  }
  
  /**
   * Returns the name of the underlying context
   * @param externalContext the ExternalContex
   * @return the name or null
   */
  public static String getContextName(final ExternalContext externalContext)
  {
    try
    {
      if (isPortlet(externalContext))
      {
        return ((PortletContext) externalContext.getContext()).getPortletContextName();
      }
      else
      {
        return ((ServletContext) externalContext.getContext()).getServletContextName();
      }
    }
    catch (final ClassCastException e)
    {
      _LOG.severe(e);
    }
    return null;
  }

  /**
   * Returns the content length or -1 if the unknown.
   *
   * @param externalContext
   *          the ExternalContext
   * @return the length or -1
   */
  public static int getContentLength(final ExternalContext externalContext)
  {
    if (isAction(externalContext))
    {
      try
      {
        if (isPortlet(externalContext))
        {
          // Allows us to not have the portal api's in the classpath
          _getPortletContentLength(externalContext.getRequest());
        }
        else
        {
          return ((ServletRequest) externalContext.getRequest()).getContentLength();
        }
      }
      catch (final ClassCastException e)
      {
        _LOG.severe(e);
      }
    }
    return -1;
  }

  /**
   * Returns the content type from the current externalContext or <code>null</code> if unknown.
   *
   * @param externalContext
   *          the ExternalContext
   * @return the content type or <code>null</code>
   */
  public static String getContentType(final ExternalContext externalContext)
  {
    if (isAction(externalContext))
    {
      try
      {
        if (isPortlet(externalContext))
        {
          // Allows us to not have the portal api's in the classpath
          return _getPortletContentType(externalContext.getRequest());
        }
        else
        {
          return ((ServletRequest) externalContext.getRequest()).getContentType();
        }
      }
      catch (final ClassCastException e)
      {
        // probably won't happen, but it could if we don't have a portlet OR a servlet container.
        _LOG.severe(e);
      }
    }
    return null;
  }

  /**
   * Returns the request input stream if one is available
   *
   * @param externalContext
   * @return
   * @throws IOException
   */
  public static InputStream getRequestInputStream(final ExternalContext externalContext)
      throws IOException
  {
    if (isAction(externalContext))
    {
      try
      {
        if (isPortlet(externalContext))
        {
          // Allows us to not have the portal api's in the classpath
          return _getPortletInputStream(externalContext.getRequest());
        }
        else
        {
          return ((ServletRequest) externalContext.getRequest()).getInputStream();
        }
      }
      catch (final ClassCastException e)
      {
        _LOG.severe(e);
      }
    }
    return null;
  }

  /**
   * Returns <code>true</code> if this externalContext represents an "action". An action request
   * is any ServletRequest or a portlet ActionRequest. It is assumed that the ExternalContext
   *
   * @return a boolean of <code>true</code> if this is a Portlet ActionRequest or an non-portlet
   *         request.
   */
  public static boolean isAction(final ExternalContext externalContext)
  {
    final Object request = externalContext.getRequest();

    if (_PORTLET_ACTION_REQUEST_CLASS == null)
    {
      _LOG
          .fine("Portlet API's are not on the classpath so isAction will only check for servlet request.");
      return request instanceof ServletRequest;
    }

    return request instanceof ServletRequest || _PORTLET_ACTION_REQUEST_CLASS.isInstance(request);
  }

  /**
   * Returns whether or not this external context is from a Portlet or a Servlet.
   *
   * @param externalContext
   *          the ExternalContext to check
   *
   * @return <code>true</code> if this is a portlet RenderRequest or ActionRequest and
   *         <code>false<code> if it is not.
   */
  public static boolean isPortlet(final ExternalContext externalContext)
  {
    if (_PORTLET_CONTEXT_CLASS == null)
    {
      _LOG.fine("Portlet API's are not on the classpath therefore isPortlet is false.");
      return false;
    }

    return _PORTLET_CONTEXT_CLASS.isInstance(externalContext.getContext());
  }

  private static final String _getPortletCharacterEncoding(final Object request)
  {
    if (!(request instanceof ActionRequest))
      return null;

    return ((ActionRequest) request).getCharacterEncoding();
  }

  private static final int _getPortletContentLength(final Object request)
  {
    if (!(request instanceof ActionRequest))
      return -1;

    return ((ActionRequest) request).getContentLength();
  }

  private static final String _getPortletContentType(final Object request)
  {
    if (!(request instanceof ActionRequest))
      return null;

    return ((ActionRequest) request).getContentType();
  }

  private static final InputStream _getPortletInputStream(final Object request) throws IOException
  {
    return ((ActionRequest) request).getPortletInputStream();
  }

  // prevent this from being instantiated
  private ExternalContextUtils()
  {}

  private static final TrinidadLogger _LOG = TrinidadLogger
                                               .createTrinidadLogger(ExternalContextUtils.class);

  // =-= Scott O'Bryan =-=
  // Performance enhancement. These will be needed anyway, let's not get them every time.
  private static final Class<?>       _PORTLET_ACTION_REQUEST_CLASS;
  private static final Class<?>       _PORTLET_CONTEXT_CLASS;

  static
  {
    Class<?> context;
    Class<?> actionRequest;
    try
    {
      context = ClassLoaderUtils.loadClass("javax.portlet.PortletContext");
      actionRequest = ClassLoaderUtils.loadClass("javax.portlet.ActionRequest");
    }
    catch (final ClassNotFoundException e)
    {
      _LOG
          .fine("Portlet API is not available on the classpath.  Portlet configurations are disabled.");
      context = null;
      actionRequest = null;
    }

    _PORTLET_CONTEXT_CLASS = context;
    _PORTLET_ACTION_REQUEST_CLASS = actionRequest;
  }
}
