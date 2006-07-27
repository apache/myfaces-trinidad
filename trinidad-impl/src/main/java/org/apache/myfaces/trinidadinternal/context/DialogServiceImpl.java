/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.context;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.faces.render.RenderKit;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.DialogService;
import org.apache.myfaces.trinidad.event.LaunchEvent;
import org.apache.myfaces.trinidad.event.ReturnEvent;
import org.apache.myfaces.trinidad.render.DialogRenderKitService;
import org.apache.myfaces.trinidad.util.Service;

import org.apache.myfaces.trinidad.logging.ADFLogger;

import org.apache.myfaces.trinidadinternal.application.StateManagerImpl;
import org.apache.myfaces.trinidadinternal.webapp.TrinidadFilterImpl;

public class DialogServiceImpl extends DialogService
{
  public DialogServiceImpl(RequestContextImpl context)
  {
    _context = context;
  }

  //
  // OBSOLETE METHODS THAT WILL BE DELETED
  //
  public void pushPageFlowScope(boolean copyParent)
  {
    FacesContext context = _getFacesContext();
    _context.getPageFlowScopeProvider().pushPageFlowScope(context,
                                                          copyParent);
  }

  public Map popPageFlowScope(boolean discardScope)
  {
    FacesContext context = _getFacesContext();
    return _context.getPageFlowScopeProvider().popPageFlowScope(context,
                                                                discardScope);
  }

  public void restorePageFlowScope(Map pageFlowScope)
  {
    FacesContext context = _getFacesContext();
    PageFlowScopeProviderImpl.__setPageFlowScope(context,
                                                 (PageFlowScopeMap) pageFlowScope);
  }

  public boolean isPageFlowScopeInvalid()
  {
    PageFlowScopeMap pageFlowScope =
      (PageFlowScopeMap) _context.getPageFlowScope();
    if (pageFlowScope == null)
      return false;
    return pageFlowScope.__invalid;
  }

  public String getPageFlowScopeToken()
  {
    PageFlowScopeMap pageFlowScope =
      (PageFlowScopeMap) _context.getPageFlowScope();
    if (pageFlowScope == null)
      return null;

    return pageFlowScope.getToken(_getFacesContext());
  }

  public Map getPageFlowScope(String token)
  {
    if (token != null)
    {
      int lifetime = _context.__getPageFlowScopeLifetime();
      FacesContext context = _getFacesContext();
      return PageFlowScopeMap.getPageFlowScopeMap(context,
                                                  token,
                                                  lifetime);
    }

    return null;
  }
  /// END OF OBSOLETE METHODS

  public void pushView(UIViewRoot viewRoot)
  {
    FacesContext context = _getFacesContext();
    Object savedOld = StateManagerImpl.saveViewRoot(context, viewRoot);

    List list = (List) _getPageFlowScope().get(_PUSHED_VIEWS_KEY);
    if (list == null)
      list = new ArrayList(1);
    else
    {
      List tmp = new ArrayList(list.size() + 1);
      tmp.addAll(list);
      list = tmp;
    }

    list.add(savedOld);
    _getPageFlowScope().put(_PUSHED_VIEWS_KEY, list);

    _LOG.fine("Pushed view {0}", viewRoot.getViewId());
  }

  public void popView(boolean navigateToPoppedView)
  {
    FacesContext context = _getFacesContext();
    // Pop the view, and navigate back
    if (navigateToPoppedView)
    {
      UIViewRoot poppedView = peekView();
      if (poppedView == null)
        throw new IllegalStateException("popView(): No view has been pushed.");

      // Set the view root
      context.setViewRoot(poppedView);

      // Re-execute any "binding" attributes
      _executeBindings(context, poppedView);
      _LOG.fine("Popped view {0}", poppedView.getViewId());
    }

    // Make a copy of the List;  never mutate the original list
    List list = (List) _getPageFlowScope().get(_PUSHED_VIEWS_KEY);
    if (list == null)
    {
      // For starters, this should only happen if we weren't navigating
      assert(!navigateToPoppedView);
      // But even then, it's an illegal state
      throw new IllegalStateException("popView(): No view has been pushed.");
    }
    else if (list.size() == 1)
    {
      _getPageFlowScope().remove(_PUSHED_VIEWS_KEY);
    }
    else
    {
      list = new ArrayList(list);
      list.remove(list.size() - 1);
      _getPageFlowScope().put(_PUSHED_VIEWS_KEY, list);
    }
  }

  public UIViewRoot peekView()
  {
    FacesContext context = _getFacesContext();
    Object savedRoot = null;

    List list = (List) _getPageFlowScope().get(_PUSHED_VIEWS_KEY);
    if (list != null)
      savedRoot = list.get(list.size() - 1);

    if (savedRoot == null)
      return null;

    try
    {
      return StateManagerImpl.restoreViewRoot(context, savedRoot);
    }
    // Exceptions will be exceptional (if a view could be saved,
    // it should be restorable too)
    catch (ClassNotFoundException cnfe)
    {
      _LOG.severe(cnfe);
    }
    catch (InstantiationException ie)
    {
      _LOG.severe(ie);
    }
    catch (IllegalAccessException iae)
    {
      _LOG.severe(iae);
    }

    return null;
  }

  public boolean returnFromDialog(Object returnValue,
                                  Map returnParameters)
  {
    FacesContext context = _getFacesContext();
    context.getExternalContext().getSessionMap().
      put(_DIALOG_RETURN_KEY, new Object[]{returnValue, returnParameters});

    Object usedRenderKit = _getPageFlowScope().get(_USED_RENDER_KIT_KEY);
    if (usedRenderKit == null)
    {
      _LOG.warning("No 'DialogUsedRK' key available for returnFromDialog to " +
                   "do the right thing!");
    }


    if (Boolean.TRUE.equals(usedRenderKit))
    {
      DialogRenderKitService service = _getDialogRenderKitService(context);
      if ((service != null) &&
          service.returnFromDialog(context, returnValue))
        return true;
    }

    // If the render kit didn't handle it, then pop the view ourselves
    popView(true);

    UIViewRoot poppedView = context.getViewRoot();

    // And, if there's parameters that need to be passed to the popped page,
    // do that;  we'll mark the response as complete, because we'll need
    // the AdfFacesFilter to re-execute the faces lifecycle with the
    // new parameters
    Map launchParameters = (Map)
      poppedView.getAttributes().get(RequestContextImpl.LAUNCH_PARAMETERS);

    if (launchParameters != null)
    {
      // Store the parameters and the UIViewRoot for (respectively)
      // AdfFacesFilterImpl and ViewHandlerImpl
      poppedView.getAttributes().remove(RequestContextImpl.LAUNCH_PARAMETERS);

      Map requestMap = context.getExternalContext().getRequestMap();
      requestMap.put(RequestContextImpl.LAUNCH_PARAMETERS, launchParameters);
      requestMap.put(RequestContextImpl.LAUNCH_VIEW, poppedView);

      context.responseComplete();
      _LOG.fine("Returned from dialog and re-executing lifecycle for {0}",
                poppedView.getViewId());
    }

    return false;

  }

  public ReturnEvent getReturnEvent(UIComponent source)
  {
    FacesContext context = _getFacesContext();
    if (TrinidadFilterImpl.isExecutingDialogReturn(context))
    {
      Map parameterMap = context.getExternalContext().getRequestParameterMap();
      Object returnParam = parameterMap.get(_RETURN_PARAM);
      if (returnParam == null)
        return null;

      String clientId = source.getClientId(context);
      if (!returnParam.equals(clientId))
        return null;
    }
    else
    {
      DialogRenderKitService service = _getDialogRenderKitService(context);
      if (service == null)
        return null;
      if (!service.isReturning(context, source))
        return null;
    }

    // OK, we think we are returning to this component.  Create
    // the ReturnEvent.
    // =-=AEW Should we automatically add the component as a partial
    // target???
    Object o = context.getExternalContext().getSessionMap().
             remove(_DIALOG_RETURN_KEY);

    Object returnValue = null;
    Map    returnParams = null;
    if (o != null)
    {
      returnValue = ((Object[]) o)[0];
      returnParams = (Map) ((Object[]) o)[1];
    }

    ReturnEvent returnEvent =
       new ReturnEvent(source, returnValue, returnParams);

    if (_LOG.isFine())
      _LOG.fine("Obtained ReturnEvent {0}", returnEvent);

    return returnEvent;
  }

  public void queueLaunchEvent(UIViewRoot viewRoot)
  {
    UIComponent source = getCurrentLaunchSource();
    if (source == null)
    {
      launchDialog(viewRoot, null, null, false, null);
    }
    else
    {
      (new InternalLaunch(source, viewRoot)).queue();
    }
  }

  public void queueReturnEvent(
    Object returnValue,
    Map    returnParams)
  {
    UIComponent source = getCurrentLaunchSource();
    if (source == null)
    {
      _LOG.warning("Could not queue return event: no launch source");
    }
    else
    {
      (new ReturnEvent(source, returnValue, returnParams)).queue();
    }
  }


  /**
   * Launch a dialog.
   * @todo Don't save parameters for state-saving, page-flow scope, etc.
   */
  public void launchDialog(
    UIViewRoot  dialogRoot,
    Map         dialogParameters,
    UIComponent source,
    boolean     useWindow,
    Map         windowProperties)
  {
    if (dialogParameters == null)
      dialogParameters = new HashMap();

    FacesContext context = _getFacesContext();

    UIViewRoot sourceRoot = context.getViewRoot();

    // Mark down that we're going to (at least try) to use
    // the renderkit to launch the dialog;  which means
    // we'll need to use the renderkit to close the dialog
    dialogParameters.put(_USED_RENDER_KIT_KEY, Boolean.TRUE);

    // Try to launch a window using the render kit. If that
    // fails (or isn't needed), fall through to the same-window
    // dialog code
    DialogRenderKitService service = _getDialogRenderKitService(context);
    if ((service != null) &&
        service.launchDialog(context,
                             dialogRoot,
                             source,
                             dialogParameters,
                             useWindow,
                             windowProperties))
    {
      // The dialog was successfully launched
      _LOG.fine("Launched {0} dialog using RenderKit",
                dialogRoot.getViewId());
      // And we must pop the pageFlow scope immediately;  it'll
      // be restored later.
      _context.getPageFlowScopeProvider().popPageFlowScope(context, false);
    }
    else
    {
      _LOG.fine("Launching {0} dialog via fallback mechanism",
                dialogRoot.getViewId());

      // Nope, we're launching it ourselves
      dialogParameters.put(_USED_RENDER_KIT_KEY, Boolean.FALSE);

      // Save the parameters used to launch the dialog so we can
      // simulate a postback when coming back to the dialog;  and
      // write in a "returnId" with the "id" that will be used.
      Map savedRequestParameters = new HashMap();
      savedRequestParameters.putAll(
            context.getExternalContext().getRequestParameterValuesMap());
      if (source != null)
      {
        savedRequestParameters.put(_RETURN_PARAM,
                                   new String[]{source.getClientId(context)});
      }
      // And make sure to remove the _RETURN_PARAM for dialogs
      // launched in response to other dialogs.
      else
      {
        savedRequestParameters.remove(_RETURN_PARAM);
      }

      // =-=AEW HACK: attempt to block events from getting retriggered
      // by simply removing the "source" and "event" parameters altogether!
      savedRequestParameters.remove("source");
      savedRequestParameters.remove("event");

      sourceRoot.getAttributes().put(RequestContextImpl.LAUNCH_PARAMETERS,
                                     savedRequestParameters);
      pushView(sourceRoot);

      // Push parameters into the new pageFlow scope
      _getPageFlowScope().putAll(dialogParameters);

      // And navigate to the dialog root
      context.setViewRoot(dialogRoot);
      context.renderResponse();
    }
  }


  //
  // Get the FacesContext.
  //
  private FacesContext _getFacesContext()
  {
    return _context.__getFacesContext();
  }

  private Map _getPageFlowScope()
  {
    return _context.getPageFlowScope();
  }

  static private DialogRenderKitService _getDialogRenderKitService(
    FacesContext context)
  {
    RenderKit rk = context.getRenderKit();
    DialogRenderKitService service = (DialogRenderKitService)
       Service.getService(rk, DialogRenderKitService.class);

    if (service == null)
    {
      _LOG.info("RenderKit {0} does not support DialogRenderKitService, and " +
                "cannot be used to launch dialogs;  using a single window " +
                "instead.", rk);
    }

    return service;
  }

  /**
   * Execute any "binding" attributes so that a popped view
   * is properly set up
   */
  private void _executeBindings(FacesContext context, UIComponent component)
  {
    ValueBinding binding = component.getValueBinding("binding");
    if (binding != null)
      binding.setValue(context, component);

    Iterator kids = component.getFacetsAndChildren();
    while (kids.hasNext())
      _executeBindings(context, (UIComponent) kids.next());
  }

  private RequestContextImpl _context;


  /**
   * Inner class that provides LaunchEvent hooking into
   * internalLaunchPageFlow()
   */
  static private class InternalLaunch extends LaunchEvent
  {
    public InternalLaunch(UIComponent source,
                          UIViewRoot viewRoot)
    {
      super(source, viewRoot);
    }

    public void launchDialog(boolean useWindow)
    {
      RequestContext afContext = RequestContext.getCurrentInstance();
      afContext.getDialogService().launchDialog(
        getViewRoot(),
        getDialogParameters(),
        getComponent(),
        useWindow,
        getWindowProperties());
    }
  }

  static private final String _PUSHED_VIEWS_KEY =
    "org.apache.myfaces.trinidadinternal.PushedViews";
  static private final String _DIALOG_RETURN_KEY =
    "org.apache.myfaces.trinidadinternal.DialogReturnValue";
  static private final String _USED_RENDER_KIT_KEY =
    "org.apache.myfaces.trinidadinternal.DialogUsedRK";
  static private final String _RETURN_PARAM =
    "org.apache.myfaces.trinidadinternal.ReturnParam";

  static private final ADFLogger _LOG =
    ADFLogger.createADFLogger(DialogServiceImpl.class);
}
