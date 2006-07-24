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
package org.apache.myfaces.adfinternal.renderkit;

import java.io.File;
import java.io.InputStream;

import java.net.MalformedURLException;
import java.net.URL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.faces.application.Application;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.render.RenderKit;

import org.apache.myfaces.adfinternal.share.config.Configuration;
import org.apache.shale.test.mock.MockExternalContext;
import org.apache.shale.test.mock.MockFacesContext;

/**
 * Mock faces context for use with unit tests
 * @author Arjuna Wijeyekoon
 */
public class MFacesContext extends MockFacesContext
{
  public MFacesContext(boolean testMode)
  {
    setCurrentInstance(this);
    _external = new External(testMode);
  }

  public ResponseWriter getResponseWriter()
  {
    return _responseWriter;
  }

  public void setResponseWriter(ResponseWriter responseWriter)
  {
    _responseWriter = responseWriter;
  }

  public Iterator getMessages()
  {
    return getMessages(_GLOBAL_MESSAGE);
  }

  public Iterator getMessages(String id)
  {
    if (id == null)
      id = _GLOBAL_MESSAGE;

    List messages = (List) _messages.get(id);
    if (messages == null)
      messages = Collections.EMPTY_LIST;
    return messages.iterator();
  }

  public void addMessage(String id, FacesMessage message)
  {
    if (id == null)
      id = _GLOBAL_MESSAGE;

    List messages = (List) _messages.get(id);
    if (messages == null)
    {
      messages = new ArrayList();
      _messages.put(id, messages);
    }
    messages.add(message);
  }

  public FacesMessage.Severity getMaximumSeverity()
  {
    FacesMessage.Severity max = FacesMessage.SEVERITY_INFO;

    Iterator clients = getClientIdsWithMessages();
    while (clients.hasNext())
    {
      String messagesKey = (String) clients.next();
      List messages = (List) _messages.get(messagesKey);
      int len = _messages.size();
      for (int i = 0; i < len; i++)
      {
        FacesMessage fm = (FacesMessage) messages.get(i);
        FacesMessage.Severity nextSev = fm.getSeverity();

        if (max.compareTo(nextSev) < 0)
        {
          max = nextSev;
          if (max.compareTo(FacesMessage.SEVERITY_FATAL) >= 0)
          {
            return max;
          }
        }
      }
    }
    return max;
  }

  public Iterator getClientIdsWithMessages()
  {
    return _messages.keySet().iterator();
  }

  public Application getApplication()
  {
    return MApplication.sharedInstance();
  }

  public UIViewRoot getViewRoot()
  {
    return _viewRoot;
  }

  public void setViewRoot(UIViewRoot viewRoot)
  {
    _viewRoot = viewRoot;
    if (_viewRoot != null)
    {
      _kit = RenderKitBootstrap.getRenderKit(this);
    }
    else
      _kit = null;
  }

  public static void clearContext()
  {
    FacesContext.setCurrentInstance(null);
  }

  public Locale getLocale()
  {
    return Locale.ENGLISH;
  }


  public ExternalContext getExternalContext()
  {
    // this method is called a lot, so we don't want to use the "mock"
    // implementations as those expect a specific number of calls:
    return _external;
  }

  public RenderKit getRenderKit()
  {
    if (_viewRoot == null)
      throw new IllegalStateException("Trying to get a RenderKit without a UIViewRoot");
    // this method is called a lot, so we don't want to use the "mock"
    // implementations as those expect a specific number of calls:
    return _kit;
  }

  private RenderKit       _kit;
  private ExternalContext _external;
  private ResponseWriter  _responseWriter;
  private UIViewRoot      _viewRoot;
  private Map             _messages = new HashMap();

  private static final class External extends MockExternalContext
  {
    public External(boolean testMode)
    {
      super(null, null, null);
      
      _testMode = testMode;
      File file = null;
      try
      {
        String tmpdir = System.getProperty("java.io.tmpdir");
        file = new File(tmpdir,
                        "adftest/view/faces/cache".replace('/',
                                           File.separatorChar));
        file.mkdirs();
        _applicationMap.put("javax.servlet.context.tempdir", file);
      }
      catch (Exception e)
      {
        System.err.println("Could not create temp directory " + file + ": " + e);
      }
    }

    public Object getContext() { return null; }
    public Object getRequest() { return null; }
    public Object getResponse() { return null; }
    public Object getSession(boolean create) { return null; }
    public String getRequestContextPath() { return "/test-context-path"; }
    public String getRequestServletPath() { return "/test-faces"; }
    public String getInitParameter(String name)
    {
      if (_testMode && Configuration.DISABLE_CONTENT_COMPRESSION.equals(name))
        return "true";
      // A hack to disable image generation
      if ("org.apache.myfaces.adfinternal.BLOCK_IMAGE_GENERATION".equals(name))
        return "true";
      return null;
    }

    public String encodeNamespace(String in) { return in; }

    public String encodeResourceURL(String url) { return "encoded-resource-url:" + url; }
    public String encodeActionURL(String url) { return "encoded-action-url:" + url; }

    public Map getRequestHeaderMap()
    {
      return Collections.EMPTY_MAP;
    }

    public Map getRequestParameterMap()
    {
      return Collections.EMPTY_MAP;
    }


    public InputStream getResourceAsStream(String path)
    {
      return MFacesContext.class.getResourceAsStream(path);
    }

    public URL getResource(String path) throws MalformedURLException
    {
      return MFacesContext.class.getResource(path);
    }

    public Map getApplicationMap()
    {
      // Return an unmodifiable map - noone should be putting
      // things into application scope during rendering.
      return Collections.unmodifiableMap(_applicationMap);
    }

    public Map getRequestMap()
    {
      // this method is called a lot, so we don't want to use the "mock"
      // implementations as those expect a specific number of calls:
      return _requestMap;
    }

    private final Map _requestMap = new HashMap(2);
    private final Map _applicationMap = new HashMap(2);
    private final boolean _testMode;
  }
  private static final String _GLOBAL_MESSAGE = "org.apache.myfaces.adfinternal.renderkit.MFacesContext.GLOBAL_MESSAGE";
}