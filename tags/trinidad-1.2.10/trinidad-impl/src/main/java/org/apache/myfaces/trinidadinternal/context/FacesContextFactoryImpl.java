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
package org.apache.myfaces.trinidadinternal.context;

import java.io.IOException;
import java.util.Iterator;
import java.util.Map;

import javax.el.ELContext;

import javax.faces.application.Application;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.FacesContextFactory;
import javax.faces.context.ResponseStream;
import javax.faces.context.ResponseWriter;
import javax.faces.lifecycle.Lifecycle;
import javax.faces.render.RenderKit;

import org.apache.myfaces.trinidad.context.ExternalContextDecorator;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadinternal.config.GlobalConfiguratorImpl;

/**
 * Internal class that optimizes retrieval of the RenderKit by caching it
 * on the FacesContext, and hooks ExternalContext.dispatch()
 * to use the PageResolver.
 * <p>
 */
public class FacesContextFactoryImpl
  extends FacesContextFactory
{
  public FacesContextFactoryImpl(FacesContextFactory factory)
  {
    _factory = factory;
  }

  @Override
  @SuppressWarnings ("unchecked")
  public FacesContext getFacesContext(
      Object context,
      Object request,
      Object response, 
      Lifecycle lifecycle)
  {
    return new CacheRenderKit(_factory.getFacesContext(context, request, response, lifecycle));
  }
  
  static public class CacheRenderKit extends FacesContext
  {
    @SuppressWarnings("unchecked")
    public CacheRenderKit(FacesContext base)
    {
      _base = base;
      ExternalContext baseExternal = base.getExternalContext();
      GlobalConfiguratorImpl config = GlobalConfiguratorImpl.getInstance();

      //This should be done only if beginRequest was not called on the configurator
      //before we retrieve the FacesContext.  If this is the case then we'll need to handle
      //cleanup on the release of the FacesContext.  Otherwise the endRequest should be
      //called by whatever did he origional beginRequest.
      if(!GlobalConfiguratorImpl.isRequestStarted(baseExternal))
      {
        Map<String, Object> requestMap = baseExternal.getRequestMap();
        requestMap.put(_CONFIG_IN_CONTEXT, Boolean.TRUE);
      }

      _external = new OverrideDispatch(config.getExternalContext(baseExternal));
      setCurrentInstance(this);
    }

    @Override
    public Application getApplication()
    {
      return _base.getApplication();
    }

    @SuppressWarnings("unchecked")
    @Override
    public Iterator getClientIdsWithMessages()
    {
      return _base.getClientIdsWithMessages();
    }

    @Override
    public ExternalContext getExternalContext()
    {
      return _external;
    }

    @Override
    public FacesMessage.Severity getMaximumSeverity()
    {
      return _base.getMaximumSeverity();
    }

    @SuppressWarnings("unchecked")
    @Override
    public Iterator getMessages()
    {
      return _base.getMessages();
    }

    @SuppressWarnings("unchecked")
    @Override
    public Iterator getMessages(String clientId)
    {
      return _base.getMessages(clientId);
    }

    @Override
    public RenderKit getRenderKit()
    {
      if (_kit == null)
      {
        _kit = _base.getRenderKit();
      }
      else
      {
        UIViewRoot root = getViewRoot();
        if (root != null)
        {
          String renderKitId = root.getRenderKitId();
          // Yes, instance equality, not .equals();  within a single
          // request and single thread, instance equality should always
          // be sufficient, and behavior will still be correct even
          // if it was somehow not (we'll just spend more time re-getting the
          // RenderKit)
          if (renderKitId != _cachedRenderKitId)
          {
            _cachedRenderKitId = renderKitId;
            _kit = _base.getRenderKit();
          }
        }
      }

      return _kit;
    }

    @Override
    public boolean getRenderResponse()
    {
      return _base.getRenderResponse();
    }

    @Override
    public boolean getResponseComplete()
    {
      return _base.getResponseComplete();
    }

    @Override
    public ResponseStream getResponseStream()
    {
      return _base.getResponseStream();
    }

    @Override
    public void setResponseStream(ResponseStream responseStream)
    {
      _base.setResponseStream(responseStream);
    }

    @Override
    public ResponseWriter getResponseWriter()
    {
      return _base.getResponseWriter();
    }

    @Override
    public void setResponseWriter(ResponseWriter responseWriter)
    {
      _base.setResponseWriter(responseWriter);
    }

    @Override
    public UIViewRoot getViewRoot()
    {
      return _base.getViewRoot();
    }

    @Override
    public void setViewRoot(UIViewRoot viewRoot)
    {
      _base.setViewRoot(viewRoot);
    }

    @Override
    public void addMessage(String clientId, FacesMessage facesMessage)
    {
      _base.addMessage(clientId, facesMessage);
    }

    @Override
    public void renderResponse()
    {
      _base.renderResponse();
    }

    @Override
    public void responseComplete()
    {
      _base.responseComplete();
    }

    @Override
    public void release()
    {
      ExternalContext ec = getExternalContext();
      if(Boolean.TRUE.equals(ec.getRequestMap().get(_CONFIG_IN_CONTEXT)))
      {
        GlobalConfiguratorImpl.getInstance().endRequest(ec);
      }
      
      _base.release();
    }
    
    public ELContext getELContext()
    {
      return _base.getELContext();
    }

    private final FacesContext    _base;
    private final ExternalContext _external;
    // An Object, not a String, so that FindBugs won't complain
    // about this usage of instance equality
    private Object    _cachedRenderKitId;
    private RenderKit _kit;
    
    static private final String _CONFIG_IN_CONTEXT = FacesContextFactoryImpl.class.getName()+".CONFIG_IN_CONTEXT";
  }

  static public class OverrideDispatch extends ExternalContextDecorator
  {
    public OverrideDispatch(ExternalContext decorated)
    {
      _decorated = decorated;
    }

    @Override
    public void dispatch(String path) throws IOException
    {
      RequestContext afc = RequestContext.getCurrentInstance();
      if (afc != null)
      {
        path = afc.getPageResolver().getPhysicalURI(path);
      }

      super.dispatch(path);
    }


    @Override
    protected ExternalContext getExternalContext()
    {
      return _decorated;
    }

    private final ExternalContext _decorated;
  }

  private final FacesContextFactory _factory;
}
