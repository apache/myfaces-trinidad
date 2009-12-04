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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.faces.FactoryFinder;
import javax.faces.application.StateManager;
import javax.faces.application.StateManagerWrapper;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;
import javax.faces.render.ResponseStateManager;
import javax.faces.view.StateManagementStrategy;
import javax.faces.view.ViewDeclarationLanguage;

import org.apache.myfaces.trinidad.bean.util.StateUtils;
import org.apache.myfaces.trinidad.component.UIXComponentBase;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.Window;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;
import org.apache.myfaces.trinidadinternal.context.TrinidadPhaseListener;
import org.apache.myfaces.trinidadinternal.util.SubKeyMap;
import org.apache.myfaces.trinidadinternal.util.TokenCache;


/**
 * StateManager that handles a hybrid client/server strategy:  a
 * SerializedView is stored on the server, and only a small token
 * is stored on the client.
 * <p>
 * <h3>Application View cache</h3>
 * <p>
 * In addition, an optional Application view cache is supported.
 * This view cache will, when enabled, perform special caching
 * of all state for non-postback requests (that is, the initial
 * state of all pages).  For all pages, their SerializedView state
 * is stored in a Map at application scope, and reused across
 * all users.  This simultaneously eliminates the expense of saving
 * the state at all (except for the first request for any page),
 * and significantly reduces memory usage as long as users are
 * largely viewing initial pages only.
 * <p>
 * In addition, because the viewId is sufficient to identify the
 * page state out of the cache, the token can be completely
 * constant across requests and users.  This makes it possible
 * to cache the page content (which is not possible otherwise).
 * <p>
 * Since application scope objects do not support failover,
 * a mirror of the cache is saved at session scope.  The mirror
 * is an LRU map of the last 16 application-scoped entries, but
 * since it stores precisely the same SerializedView instances
 * as the application scope, the additional memory requirements
 * are minimal.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/application/StateManagerImpl.java#2 $) $Date: 18-nov-2005.16:12:04 $
 */
public class StateManagerImpl extends StateManagerWrapper
{
  static public final String CACHE_VIEW_ROOT_INIT_PARAM =
    "org.apache.myfaces.trinidad.CACHE_VIEW_ROOT";


  /**
   * Servlet context initialization parameter used by
   * StateManagerImpl to decide what sort of state should be saved
   * on the client.  Valid values are "token" and "all";  the
   * default is "token".
   */
  static public final String CLIENT_STATE_METHOD_PARAM_NAME =
    "org.apache.myfaces.trinidad.CLIENT_STATE_METHOD";

  /**
   * Servlet context initialization parameter used by
   * StateManagerImpl to decide how many tokens can be stored
   * per user.  The default is 15.
   */
  static public final String CLIENT_STATE_MAX_TOKENS_PARAM_NAME =
    "org.apache.myfaces.trinidad.CLIENT_STATE_MAX_TOKENS";

  /**
   * Value indicating that only a simple token will be stored
   * on the client.
   */
  static public final String CLIENT_STATE_METHOD_TOKEN = "token";

  /**
   * Value indicating that the entire component state will be stored
   * on the client.
   */
  static public final String CLIENT_STATE_METHOD_ALL = "all";

  public StateManagerImpl(
    StateManager delegate)
  {
    _delegate = delegate;
  }
  
  @Override
  public StateManager getWrapped()
  {
    return _delegate;
  }

  @SuppressWarnings("deprecation")
  @Override
  public Object saveView(FacesContext context)
  {
    assert(context != null);
    
    // if the root is transient don't state save
    UIViewRoot viewRoot = context.getViewRoot();
    
    if (viewRoot.isTransient()) 
    {
        return null;
    }
    
    String viewId = context.getViewRoot().getViewId();
    ViewDeclarationLanguage vdl =  context.getApplication().getViewHandler().
                                                    getViewDeclarationLanguage(context, viewId);
    StateManagementStrategy sms = null;
    
    if (vdl != null) 
    {
      sms = vdl.getStateManagementStrategy(context, viewId);
    }
    
    if (sms != null) 
    {
      return sms.saveView(context);
    }
    else
    {
      SerializedView view = _saveSerializedView(context);
      return new Object[]{view.getStructure(), view.getState()};
    }
  }

  @Override @SuppressWarnings("deprecation")
  public SerializedView saveSerializedView(FacesContext context)
  {
    assert(context != null);
    
    return _saveSerializedView(context);
  }

  /**
   * Save a component tree as an Object.
   */
  static public Object saveComponentTree(
    FacesContext context,
    UIComponent  component)
  {
    // Don't remove transient components...
    Object structure = new Structure(component);
    Object state = component.processSaveState(context);
    return new PageState(context, structure, state, null);
  }
  
  /**
   * Take an object created by saveComponentTree()
   * and instantiate it as a UIComponent.
   */
  static public UIComponent restoreComponentTree(
    FacesContext context,
    Object       savedState) throws ClassNotFoundException,
                                    InstantiationException,
                                    IllegalAccessException
  {
    if (savedState == null)
      throw new NullPointerException();

    if (!(savedState instanceof PageState))
      throw new IllegalArgumentException(_LOG.getMessage(
        "INVALID_SAVED_STATE_OBJECT"));

    PageState viewState = (PageState) savedState;

    Object structure = viewState.getStructure();
    Object state = viewState.getState();

    UIComponent component =
      ((Structure) structure).createComponent();

    if (state != null)
      component.processRestoreState(context, state);

    return component;
  }


  /**
   * Save a view root.  Doesn't return a SerializedView because
   * SerializedView is a non-static inner class, and this needs
   * to be a static method.
   */
  static public Object saveViewRoot(
    FacesContext context,
    UIViewRoot   root)
  {
    _removeTransientComponents(root);

    Object structure = new Structure(root);
    Object state = root.processSaveState(context);
    return new PageState(context, structure, state, root);
  }

  static public UIViewRoot restoreViewRoot(
    FacesContext    context,
    Object          saved) throws ClassNotFoundException, InstantiationException,
                                  IllegalAccessException

  {
    if (saved == null)
      throw new NullPointerException();

    PageState viewState = (PageState) saved;

    UIViewRoot root = viewState.popRoot(context);
    if (root != null)
    {
      return root; // bug 4712492
    }

    Object structure = viewState.getStructure();
    Object state = viewState.getState();

    root = (UIViewRoot)
      ((Structure) structure).createComponent();

    if (state != null)
      root.processRestoreState(context, state);

    return root;
  }  

  @SuppressWarnings({"unchecked", "deprecation"})
  private SerializedView _saveSerializedView(FacesContext context)
  {
    // see if a serialized view has been saved on the request
    SerializedView view = _getCachedSerializedView(context);
    if (view != null)
      return view;

    UIViewRoot root = context.getViewRoot();

    _removeTransientComponents(root);

    Object structure = !_needStructure(context) ? null : new Structure(root);
    Object state = root.processSaveState(context);

    if (_saveAsToken(context))
    {
      String token;
      ExternalContext extContext = context.getExternalContext();


      TokenCache cache = _getViewCache(context);
      assert(cache != null);

      Map<String, Object> sessionMap = extContext.getSessionMap();

      RequestContext trinContext = RequestContext.getCurrentInstance();
      
      // get per window view cache key with "." separator suffix to separate the SubKeyMap keys
      String subkey = _getViewCacheKey(extContext, trinContext, _SUBKEY_SEPARATOR);
      
      Map<String, PageState> stateMap = new SubKeyMap<PageState>(sessionMap, subkey);

      // Sadly, we can't save just a SerializedView, because we should
      // save a serialized object, and SerializedView is a *non*-static
      // inner class of StateManager
      PageState pageState = new PageState(
          context,
          structure,
          state,
          // Save the view root into the page state as a transient
          // if this feature has not been disabled
          _useViewRootCache(context) ? root : null);

      // clear out all of the previous PageStates' UIViewRoots and add this page
      // state as an active page state.  This is necessary to avoid UIViewRoots
      // laying around if the user navigates off of a page using a GET
      synchronized(extContext.getSession(true))
      {
        // get the per-window key for the active page state
        String activePageStateKey = _getActivePageStateKey(extContext, trinContext);
        PageState activePageState = (PageState)sessionMap.get(activePageStateKey);

        if (activePageState != null)
          activePageState.clearViewRootState();

        sessionMap.put(activePageStateKey, pageState);
      }
      
      String requestToken = _getRequestTokenForResponse(context);
      // If we have a cached token that we want to reuse,
      // and that token hasn't disappeared from the cache already
      // (unlikely, but not impossible), use the stateMap directly
      // without asking the cache for a new token
      if ((requestToken != null) && cache.isAvailable(requestToken))
      {
        // NOTE: under *really* high pressure, the cache might
        // have been emptied between the isAvailable() call and
        // this put().  This seems sufficiently implausible to
        // be worth punting on
        stateMap.put(requestToken, pageState);
        token = requestToken;
        // NOTE 2: we have not pinned this reused state to any old state
        // This is OK for current uses of pinning and state reuse,
        // as pinning stays constant within a window, and we're not
        // erasing pinning at all.
      }
      else
      {
        // See if we should pin this new state to any old state
        String pinnedToken = (String)extContext.getRequestMap().get(_PINNED_STATE_TOKEN_KEY);
        token = cache.addNewEntry(pageState,
                                  stateMap,
                                  pinnedToken);
      }
      

      assert(token != null);

      // Create a "tokenView" which abuses SerializedView to store
      // our token only
      view = new SerializedView(token, null);
      
      // And store the token for this request
      extContext.getRequestMap().put(_REQUEST_STATE_TOKEN_KEY, token);
    }
    else
    {
      view = new SerializedView(structure, state);
    }

    _saveCachedSerializedView(context, view);

    return view;
  }

  /**
   * Requests that an old state token be "pinned" to the state of
   * the current request.  This means that the view state corresponding
   * to the token will not be released before the state for this request
   * is released.
   */
  @SuppressWarnings("unchecked")
  static public void pinStateToRequest(FacesContext context, String stateToken)
  {
    context.getExternalContext().getRequestMap().put(
            _PINNED_STATE_TOKEN_KEY, stateToken);
    
  }
  
  /**
   * @return the state token for the current request
   */
  static public String getStateToken(FacesContext context)
  {
    return (String) context.getExternalContext().getRequestMap().get(
            _REQUEST_STATE_TOKEN_KEY);
  }
  
  
  /**
   * Mark the the incoming request token should be used for the response
   */
  @SuppressWarnings("unchecked")
  static public void reuseRequestTokenForResponse(ExternalContext ec)
  {
    ec.getRequestMap().put(_REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY, Boolean.TRUE);    
  }
  
  /**
   * Clears the flag indicating that the old request token should be used for the response.
   */
  @SuppressWarnings("unchecked")
  static public void clearReuseRequestTokenForResponse(ExternalContext ec)
  {
    ec.getRequestMap().remove(_REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY);    
  }

  /**
   * If we've been asked to reuse the request token for the response,
   * store it off.
   */
  @SuppressWarnings("unchecked")
  static private void _updateRequestTokenForResponse(
    FacesContext context, String token)
  {
    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();
    // Go from TRUE -> the saved token
    if (Boolean.TRUE.equals(
          requestMap.get(_REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY)))
    {
      requestMap.put(_REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY, token);
    }
  }


  /**
   * Get any cached token for the response.
   */
  @SuppressWarnings("unchecked")
  static private String _getRequestTokenForResponse(
    FacesContext context)
  {
    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();
    Object token = requestMap.get(_REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY);
    // We wanted to, but didn't have anything saved
    if (Boolean.TRUE.equals(token))
      return null;

    return (String) token;
  }
    
  
  @Override @SuppressWarnings("deprecation")
  public void writeState(FacesContext context,
                         SerializedView state) throws IOException
  {
    _delegate.writeState(context, state);
  }

  @SuppressWarnings({"unchecked", "deprecation"})
  @Override
  public UIViewRoot restoreView(FacesContext context, String viewId,
                                String renderKitId)
  {
    final ExternalContext extContext = context.getExternalContext();
    
    // If we're being asked to execute a "return" event from, say, a dialog, always 
    // restore the "launch view", which was set over in the TrinidadFilter.
    UIViewRoot launchView = (UIViewRoot)
                            extContext.getRequestMap().remove(RequestContextImpl.LAUNCH_VIEW);
    if (launchView != null)
    {
      TrinidadPhaseListener.markPostback(context);
      return launchView;
    }
          
    ViewDeclarationLanguage vdl = context.getApplication().getViewHandler().
                                                       getViewDeclarationLanguage(context, viewId);    
    StateManagementStrategy sms = null;
    
    if (vdl != null) 
    {
      sms = vdl.getStateManagementStrategy(context, viewId);
    }

    if (sms!= null) 
    {
      return sms.restoreView(context, viewId, renderKitId);
    } 
    else
    {
      final Object structure;
      final Object state;
  
      ResponseStateManager rsm = _getResponseStateManager(context, renderKitId);
      if (_saveAsToken(context))
      {
        Object token = rsm.getTreeStructureToRestore(context, viewId);
        if (token == null)
        {
          _LOG.finest("No token in the request for view \"{0}\";  probably a first view.", viewId);
          return null;
        }
  
        assert(token instanceof String);
        _LOG.finer("Restoring saved view state for token {0}", token);
  

        // get view cache key with "." separator suffix to separate the SubKeyMap keys
        String subkey = _getViewCacheKey(extContext,
                                         RequestContext.getCurrentInstance(),
                                         _SUBKEY_SEPARATOR);
        
        Map<String, PageState> stateMap = new SubKeyMap<PageState>(
                         extContext.getSessionMap(),
                         subkey);
        PageState viewState = stateMap.get(token);

        if (viewState != null)
          _updateRequestTokenForResponse(context, (String) token);

        // Make sure that if the view state is present, the cache still
        // has the token, and vice versa

        // NOTE: it's very important that we call through to the
        // token cache here, not just inside the assert.  If we don't,
        // then we don't actually access the token, so it doesn't
        // get bumped up to the front in the LRU Cache!
        boolean isAvailable =
          _getViewCache(context).isAvailable((String) token);
        assert ((viewState != null) == isAvailable);
  
        if (viewState == null)
        {
          _LOG.severe("CANNOT_FIND_SAVED_VIEW_STATE", token);
          return null;
        }
  
        _LOG.fine("Successfully found view state for token {0}", token);
  
        UIViewRoot root = viewState.popRoot(context); // bug 4712492
        if (root != null)
        {
          _LOG.finer("UIViewRoot for token {0} already exists. Bypassing restoreState", token);
          return root;
        }
  
        structure = viewState.getStructure();
        state = viewState.getState();
      }
      else
      {
        structure = rsm.getTreeStructureToRestore(context, viewId);
        state = rsm.getComponentStateToRestore(context);
      }
  
      if (structure == null)
      {
  
        UIViewRoot root = context.getViewRoot();
        if (root == null && _needStructure(context))
        {
          _LOG.severe("NO_STRUCTURE_ROOT_AVAILABLE");
          return null;
        }
  
        if (state != null)
          root.processRestoreState(context, state);
  
        return root;
      }
      else
      {
        if (!(structure instanceof Structure))
        {
          _LOG.severe("NO_STRUCTURE_AVAILABLE");
          return null;
        }
  
        // OK, we've structure and state; let's see what we can do!
        try
        {
          UIViewRoot root = (UIViewRoot)
          ((Structure) structure).createComponent();
  
          if (state != null)
            root.processRestoreState(context, state);          
  
          _LOG.finer("Restored state for view \"{0}\"", viewId);
          return root;
        }
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
      }
    }
    return null;
  }

  @Override
  public boolean isSavingStateInClient(FacesContext context)
  {
    return _delegate.isSavingStateInClient(context);
  }

  //
  // Protected APIs: we don't want
  //

  @Override
  protected Object getTreeStructureToSave(FacesContext context)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  protected Object getComponentStateToSave(FacesContext context)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  protected UIViewRoot restoreTreeStructure
    (FacesContext context, String viewId, String renderKitId)
  {
    throw new UnsupportedOperationException();
  }

  @Override
  protected void restoreComponentState
    (FacesContext context, UIViewRoot viewRoot, String renderKitId)
  {
    throw new UnsupportedOperationException();
  }


  private TokenCache _getViewCache(FacesContext context)
  {
    ExternalContext extContext = context.getExternalContext();
    
    return TokenCache.getTokenCacheFromSession(context,
                                               _getViewCacheKey(extContext,
                                                                RequestContext.getCurrentInstance(),
                                                                null),
                                               true,
                                               _getCacheSize(extContext));
  }

  

  /**
   * Returns a key suitable for finding the per-window active page state key
   * @param extContext
   * @param trinContext
   * @return
   */
  static private String _getActivePageStateKey(
    ExternalContext extContext,
    RequestContext trinContext)
  {
    return _getPerWindowCacheKey(extContext, trinContext, _ACTIVE_PAGE_STATE_SESSION_KEY, null);
  }
  
  /**
   * Returns a key suitable for finding the per-window cache key
   * @param extContext
   * @param trinContext
   * @param suffix
   * @return
   */
  static private String _getViewCacheKey(
    ExternalContext extContext,
    RequestContext trinContext,
    Character suffix)
  {
    return _getPerWindowCacheKey(extContext, trinContext, _VIEW_CACHE_KEY, suffix);
  }
  
  /**
   * Returns a key of the form <prefix>.<windowid><suffix> if a window and a suffix are available
   *                           <prefix>.<window> if just a window is available
   *                           <prefix> if neither a window or a suffix is available
   * @param eContext
   * @param trinContext
   * @param prefix
   * @param suffix
   * @return
   */
  static private String _getPerWindowCacheKey(
    ExternalContext eContext,
    RequestContext trinContext,
    String    prefix,
    Character suffix)
  {
    Window currWindow = trinContext.getWindowManager().getCurrentWindow(eContext);
    
    // if we have a current window or a suffix, we need a StringBuilder to calculate the cache key
    if ((currWindow != null) || (suffix != null))
    {
      // get the window id and the extra size neeeded to store it and its separator
      String windowId;
      int windowPartSize;
    
      if (currWindow != null)
      {
        windowId = currWindow.getId();
        
        // add 1 for separator
        windowPartSize = windowId.length() + 1;
      }
      else
      {
        windowId = null;
        windowPartSize = 0;
      }
      
      int builderSize =  prefix.length() + windowPartSize;
      
      // add extra space for the suffix Character
      if (suffix != null)
        builderSize += 1;
      
      // add the constant part to the StringBuilder
      StringBuilder keyBuilder = new StringBuilder(builderSize);
      keyBuilder.append(prefix);
      
      // add the windowId and its separator
      if (currWindow != null)
      {
        keyBuilder.append('.');
        keyBuilder.append(windowId);
      }
      
      // add the suffix if any
      if (suffix != null)
        keyBuilder.append(suffix);
      
      return keyBuilder.toString();
    }
    else
    {
      return prefix;
    }
  }

  /**
   * Tests whether to send a small string token, or the entire
   * serialized component state to the client-side.
   * @return true, if the small string token is to be sent to the client-side.
   */
  private boolean _saveAsToken(FacesContext context)
  {
    ExternalContext external = context.getExternalContext();
    Object stateSavingMethod =
      external.getInitParameterMap().get(StateManager.STATE_SAVING_METHOD_PARAM_NAME);    
    
    if ((stateSavingMethod == null) ||
        StateManager.STATE_SAVING_METHOD_SERVER.equalsIgnoreCase((String) stateSavingMethod))
    {
      return true;
    }
    
    Object clientMethod =
      external.getInitParameterMap().get(CLIENT_STATE_METHOD_PARAM_NAME);
    if ((clientMethod != null) &&
        CLIENT_STATE_METHOD_ALL.equalsIgnoreCase((String) clientMethod))
      return false;

    return true;
  }

  private int _getCacheSize(ExternalContext extContext)
  {
    Object maxTokens =
      extContext.getInitParameterMap().get(CLIENT_STATE_MAX_TOKENS_PARAM_NAME);
    if (maxTokens != null)
    {
      try
      {
        return Math.max(1, Integer.parseInt((String) maxTokens));
      }
      catch (NumberFormatException nfe)
      {
        _LOG.warning("Ignoring servlet init parameter:"+CLIENT_STATE_MAX_TOKENS_PARAM_NAME+
          "\n unable to parse:"+maxTokens, nfe);
        _LOG.warning(nfe);
      }
    }

    return _DEFAULT_CACHE_SIZE;
  }
  

  private boolean _useViewRootCache(FacesContext context)
  {
    if (_useViewRootCache == null)
    {
      String s = context.getExternalContext().getInitParameter(
                        CACHE_VIEW_ROOT_INIT_PARAM);
      _useViewRootCache =
      (!"false".equalsIgnoreCase(s)) ? Boolean.TRUE : Boolean.FALSE;
    }

    return _useViewRootCache.booleanValue();
  }



  private boolean _needStructure(FacesContext context)
  {
    // TODO - partial state saving is handled by facelets in JSF 2.0, 
    //        remove this method and anything that depends on it?
    return true;
  }

  static private ResponseStateManager _getResponseStateManager(
    FacesContext context,
    String       renderKitId)
  {
    RenderKitFactory factory = (RenderKitFactory)
      FactoryFinder.getFactory(FactoryFinder.RENDER_KIT_FACTORY);
    RenderKit kit = factory.getRenderKit(context, renderKitId);
    return kit.getResponseStateManager();
  }

  @SuppressWarnings("unchecked")
  static private void _removeTransientComponents(
    UIComponent root)
  {
    List<UIComponent> components = new ArrayList<UIComponent>();
    _gatherTransientComponents(root, components);
    Iterator<UIComponent> iter = components.iterator();
    while (iter.hasNext())
    {
      UIComponent kid = iter.next();
      UIComponent parent = kid.getParent();
      // First, see if its a child
      if (parent.getChildCount() > 0)
      {
        List<UIComponent> children = parent.getChildren();
        if (children.remove(kid))
        {
          continue;
        }
      }

      // Nope, guess it's a facet
      // 2006-08-02: -= Simon Lessard
      //             Not 1.5 structure and inefficient loop
      //             values() is more efficient as you don't have 
      //             to do a second lookup for the value.
      Map<String, UIComponent> facets = parent.getFacets();
      for(Iterator<UIComponent> facetIter = facets.values().iterator(); 
          facetIter.hasNext();)
      {
        if(facetIter.next() == kid)
        {
          facetIter.remove();
          // FIXME: -= Simon Lessard
          //        Is that continue need to labeled to go all the way up to 
          //        the first while? Currently it won't cause any problem, but 
          //        it's a performance loss.
          continue;
        }
      }

      // Couldn't find the component at all in its parent:  that's
      // not good.
      assert false;
    }
  }

  @SuppressWarnings("unchecked")
  static private void _gatherTransientComponents(
    UIComponent component, List<UIComponent> componentsToRemove)
  {
    Iterator<UIComponent> kids = component.getFacetsAndChildren();
    while (kids.hasNext())
    {
      UIComponent kid = kids.next();
      // UIXComponentBase doesn't mind transient components
      // in its saved state, so don't bother with this.
      if (!(component instanceof UIXComponentBase) &&
          kid.isTransient())
      {
        componentsToRemove.add(kid);
      }
      else
      {
        _gatherTransientComponents(kid, componentsToRemove);
      }
    }
  }

  @SuppressWarnings("deprecation")
  private SerializedView _getCachedSerializedView(
    FacesContext context)
  {
    return (SerializedView) context.getExternalContext().
                 getRequestMap().get(_CACHED_SERIALIZED_VIEW);
  }

  @SuppressWarnings({"unchecked","deprecation"})
  private void _saveCachedSerializedView(
    FacesContext context, SerializedView state)
  {
    context.getExternalContext().getRequestMap().put(_CACHED_SERIALIZED_VIEW,
                                                     state);
  }

  private static final class ViewRootState
  {
    public ViewRootState(FacesContext context, UIViewRoot viewRoot)
    {
      if (viewRoot == null)
        throw new NullPointerException();
      
      _viewRoot = viewRoot;
      _viewRootState = viewRoot.saveState(context);
    }
    
    public UIViewRoot getViewRoot()
    {
      return _viewRoot;
    }

    public Object getViewRootState()
    {
      return _viewRootState;
    }
   
    private final UIViewRoot _viewRoot;
    private final Object _viewRootState;
  }

  private static final class PageState implements Serializable
  {
    private static final long serialVersionUID = 1L;

    private final Object _structure, _state;
    
    // use transient since UIViewRoots are not Serializable.
    private transient ViewRootState _cachedState;

    public PageState(FacesContext fc, Object structure, Object state, UIViewRoot root)
    {
      _structure = structure;
      _state = state;
      
      // if component tree serialization checking is on (in order to validate
      // fail over support, attempt to Serialize all of the component state
      //  immediately
      if (StateUtils.checkComponentTreeStateSerialization(fc))
      {
        try
        {
          new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(state);
        }
        catch (IOException e)
        {          
          throw new RuntimeException(_LOG.getMessage("COMPONENT_TREE_SERIALIZATION_FAILED"), e);
        }
      }
                  
      // we need this state, as we are going to recreate the UIViewRoot later. see
      // the popRoot() method:
      _cachedState = (root != null)
                       ? new ViewRootState(fc, root)
                       : null;
    }

    public Object getStructure()
    {
      return _structure;
    }

    public Object getState()
    {
      return _state;
    }

    public void clearViewRootState()
    {
      synchronized(this)
      {
        _cachedState = null;
      }
    }
    
    @SuppressWarnings("unchecked")
    public UIViewRoot popRoot(FacesContext fc)
    {
      UIViewRoot root = null;
      Object viewRootState = null;
      // we need to synchronize because we are mutating _root
      // which is shared between simultaneous requests from the same user:
      synchronized(this)
      {
        if (_cachedState != null)
        {
          root = _cachedState.getViewRoot();
          viewRootState = _cachedState.getViewRootState();
          // we must clear the cached viewRoot. This is because UIComponent trees
          // are mutable and if the back button
          // is used to come back to an old PageState, then it would be
          // really bad if we reused that component tree:
          _cachedState = null;
        }
      }
      
      if (root != null)
      {
        // If an error happens during updateModel, JSF 1.1 does not
        // clear FacesEvents (or FacesMessages, IIRC), so any pending
        // events will still be present, on the subsequent request.
        // so to clear the events, we create a new UIViewRoot.
        // must get the UIViewRoot from the application so that
        // we pick up any custom ViewRoot defined in faces-config.xml:
        UIViewRoot newRoot = (UIViewRoot) 
          fc.getApplication().createComponent(UIViewRoot.COMPONENT_TYPE);
        
        //This code handles automatic namespacing in a JSR-301 environment
        if(ExternalContextUtils.isPortlet(fc.getExternalContext())) 
        {
          //IMPORTANT: To avoid introducing a runtime dependency on the bridge,
          //this method should only be executed when we have a portlet
          //request.  If we do have a portlet request then the bridge
          //should be available anyway. Trinidad has a compile-time dependency
          //on the PortletBridge API but DOES NOT have a runtime dependency on
          //those clases.  It is very important to keep this seperation.
          newRoot = PortletUtils.getPortletViewRoot(newRoot);
        }

        
        // must call restoreState so that we setup attributes, listeners,
        // uniqueIds, etc ...
        newRoot.restoreState(fc, viewRootState);

        // we need to use a temp list because as a side effect of
        // adding a child to a UIComponent, that child is removed from
        // the parent UIComponent. So the following will break:
        // newRoot.getChildren().addAll(root.getChildren());
        // because "root"'s child List is being mutated as the List
        // is traversed.
        List<UIComponent> temp = new ArrayList<UIComponent>(root.getChildCount());
        temp.addAll(root.getChildren());
        newRoot.getChildren().addAll(temp);
        return newRoot;
      }
      
      return null;
    }    
  }

  // TODO - we used to delegate to the RI when the stateManagement method was server,
  // but we no longer do that, do we really need _delegate any more?
  private final StateManager _delegate;
  private       Boolean      _useViewRootCache;

  private static final Character _SUBKEY_SEPARATOR = new Character('.');
  
  private static final int _DEFAULT_CACHE_SIZE = 15;
  
  // base key used to identify the view cache.  The window name, if any, is appended to this
  private static final String _VIEW_CACHE_KEY =
    "org.apache.myfaces.trinidadinternal.application.VIEW_CACHE";

  private static final String _CACHED_SERIALIZED_VIEW =
    "org.apache.myfaces.trinidadinternal.application.CachedSerializedView";

  private static final String _REQUEST_STATE_TOKEN_KEY =
    "org.apache.myfaces.trinidadinternal.application.REQUEST_STATE_TOKEN";

  private static final String _PINNED_STATE_TOKEN_KEY =
    "org.apache.myfaces.trinidadinternal.application.PINNED_STATE_TOKEN";

  private static final String _REUSE_REQUEST_TOKEN_FOR_RESPONSE_KEY =
    "org.apache.myfaces.trinidadinternal.application.REUSE_REQUEST_TOKEN_FOR_RESPONSE";

  // key for saving the PageState for the last accessed view in this Session
  private static final String _ACTIVE_PAGE_STATE_SESSION_KEY =
              "org.apache.myfaces.trinidadinternal.application.StateManagerImp.ACTIVE_PAGE_STATE";

  private static final long serialVersionUID = 1L;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StateManagerImpl.class);
}
