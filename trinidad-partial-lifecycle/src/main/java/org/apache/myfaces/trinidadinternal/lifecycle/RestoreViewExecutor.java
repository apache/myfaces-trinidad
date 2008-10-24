package org.apache.myfaces.trinidadinternal.lifecycle;

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import javax.el.ValueExpression;
import javax.faces.FacesException;
import javax.faces.FactoryFinder;
import javax.faces.application.Application;
import javax.faces.application.ViewExpiredException;
import javax.faces.application.ViewHandler;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;
import java.lang.reflect.Method;
import java.util.Iterator;
import java.util.Map;

/**
 * Implements the lifecycle as described in Spec. 1.0 PFD Chapter 2
 * <p/>
 * Restore view phase (JSF Spec 2.2.1)
 */
class RestoreViewExecutor implements PhaseExecutor
{

  private static final TrinidadLogger LOG = TrinidadLogger.createTrinidadLogger(RestoreViewExecutor.class);
  private static final String JAVAX_SERVLET_INCLUDE_SERVLET_PATH = "javax.servlet.include.servlet_path";

  private static final String JAVAX_SERVLET_INCLUDE_PATH_INFO = "javax.servlet.include.path_info";

  public boolean execute(FacesContext facesContext)
  {
    if (facesContext == null)
    {
      throw new FacesException("FacesContext is null");
    }

    // init the View
    Application application = facesContext.getApplication();
    ViewHandler viewHandler = application.getViewHandler();
    viewHandler.initView(facesContext);

    UIViewRoot viewRoot = facesContext.getViewRoot();


    if (viewRoot != null)
    {
      if (LOG.isFine())
      {
        LOG.fine("View already exists in the FacesContext");
      }
      viewRoot.setLocale(facesContext.getExternalContext().getRequestLocale());
      processComponentBinding(facesContext, viewRoot);
      return false;
    }

    String viewId = calculateViewId(facesContext);
    // Determine if this request is a postback or initial request
    if (isPostback(facesContext))
    {
      if (LOG.isFine())
      {
        LOG.fine("Request is a postback");
      }

      viewRoot = viewHandler.restoreView(facesContext, viewId);
      if (viewRoot == null)
      {
        throw new ViewExpiredException(
            "No saved view state could be found for the view identifier: "
                + viewId, viewId);
      }
      processComponentBinding(facesContext, viewRoot);
    } else
    {
      if (LOG.isFine())
      {
        LOG.fine("Request is not a postback. New UIViewRoot will be created");
      }
      viewRoot = viewHandler.createView(facesContext, viewId);
      facesContext.renderResponse();
    }

    facesContext.setViewRoot(viewRoot);

    return false;
  }

  public PhaseId getPhase()
  {
    return PhaseId.RESTORE_VIEW;
  }

  /**
   * Walk the component tree, executing any component-bindings to reattach
   * components to their backing beans.
   * <p/>
   * Note that this method effectively breaks encapsulation; instead of
   * asking each component to update itself and its children, this
   * method just reaches into each component. That makes it impossible
   * for any component to customise its behaviour at this point.
   * <p/>
   * This has been filed as an issue against the spec. Until this
   * issue is resolved, we'll add a new marker-interface for components
   * to allow them to define their interest in handling children bindings themselves.
   *
   * @param facesContext
   * @param parent
   */
  public static void recursivelyHandleComponentReferencesAndSetValid(FacesContext facesContext,
      UIComponent parent)
  {
    recursivelyHandleComponentReferencesAndSetValid(facesContext, parent, false);
  }

  public static void recursivelyHandleComponentReferencesAndSetValid(FacesContext facesContext,
      UIComponent parent, boolean forceHandle)
  {
    Method handleBindingsMethod = getBindingMethod(parent);

    if (handleBindingsMethod != null && !forceHandle)
    {
      try
      {
        handleBindingsMethod.invoke(parent, new Object[]{});
      }
      catch (Throwable th)
      {
        LOG.severe("Exception while invoking handleBindings on component with client-id:"
            + parent.getClientId(facesContext), th);
      }
    } else
    {
      for (Iterator it = parent.getFacetsAndChildren(); it.hasNext();)
      {
        UIComponent component = (UIComponent) it.next();

        ValueExpression binding = component.getValueExpression("binding");    //TODO: constant
        if (binding != null)
        {
          binding.setValue(facesContext.getELContext(), component);
        }

        recursivelyHandleComponentReferencesAndSetValid(facesContext, component);
      }
    }
  }

  /**
   * This is all a hack to work around a spec-bug which will be fixed in JSF2.0
   *
   * @param parent
   * @return true if this component is bindingAware (e.g. aliasBean)
   */
  private static Method getBindingMethod(UIComponent parent)
  {
    for (Class clazz : parent.getClass().getInterfaces())
    {
      if (clazz.getName().indexOf("BindingAware") != -1)
      {
        try
        {
          return parent.getClass().getMethod("handleBindings", new Class[]{});
        }
        catch (NoSuchMethodException e)
        {
          // return
        }
      }
    }

    return null;
  }


  public void processComponentBinding(FacesContext facesContext, UIComponent component)
  {
    ValueExpression binding = component.getValueExpression("binding");
    if (binding != null)
    {
      binding.setValue(facesContext.getELContext(), component);
    }

    //This part is for make compatibility with t:aliasBean, because
    //this components has its own code before and after binding is
    //set for child components.
    recursivelyHandleComponentReferencesAndSetValid(facesContext, component);

    //The required behavior for the spec is call recursively this method
    //for walk the component tree.
    //for (Iterator<UIComponent> iter = component.getFacetsAndChildren(); iter.hasNext();)
    //{
    //    processComponentBinding(facesContext, iter.next());
    //}
  }

  private String calculateViewId(FacesContext facesContext)
  {
    ExternalContext externalContext = facesContext.getExternalContext();
    Map requestMap = externalContext.getRequestMap();

    String viewId = (String) requestMap.get(JAVAX_SERVLET_INCLUDE_PATH_INFO);
    boolean traceEnabled = LOG.isFine();
    if (viewId != null)
    {
      if (traceEnabled)
      {
        LOG.fine("Calculated viewId '" + viewId + "' from request param '" + JAVAX_SERVLET_INCLUDE_PATH_INFO + "'");
      }
    } else
    {
      viewId = externalContext.getRequestPathInfo();
      if (viewId != null && traceEnabled)
      {
        LOG.fine("Calculated viewId '" + viewId + "' from request path info");
      }
    }

    if (viewId == null)
    {
      viewId = (String) requestMap.get(JAVAX_SERVLET_INCLUDE_SERVLET_PATH);
      if (viewId != null && traceEnabled)
      {
        LOG.fine("Calculated viewId '" + viewId + "' from request param '" + JAVAX_SERVLET_INCLUDE_SERVLET_PATH + "'");
      }
    }

    if (viewId == null)
    {
      viewId = externalContext.getRequestServletPath();
      if (viewId != null && traceEnabled)
      {
        LOG.fine("Calculated viewId '" + viewId + "' from request servlet path");
      }
    }

    if (viewId == null)
    {
      throw new FacesException("Could not determine view id.");
    }

    return viewId;
  }

  public boolean isPostback(FacesContext facesContext)
  {
    ViewHandler viewHandler = facesContext.getApplication().getViewHandler();
    String renderkitId = viewHandler.calculateRenderKitId(facesContext);
    RenderKitFactory renderFactory = (RenderKitFactory) FactoryFinder.getFactory(FactoryFinder.RENDER_KIT_FACTORY);
    RenderKit renderKit = renderFactory.getRenderKit(facesContext, renderkitId);
    return renderKit.getResponseStateManager().isPostback(facesContext);
  }
}
