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
package org.apache.myfaces.trinidadinternal.application;

import java.io.IOException;

import java.io.Serializable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.FactoryFinder;
import javax.faces.application.StateManager;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;
import javax.faces.render.ResponseStateManager;

import org.apache.myfaces.trinidad.logging.ADFLogger;

import org.apache.myfaces.trinidad.component.UIXComponentBase;

import org.apache.myfaces.trinidadinternal.util.LRUCache;
import org.apache.myfaces.trinidadinternal.util.SubKeyMap;
import org.apache.myfaces.trinidadinternal.util.TokenCache;
import org.apache.myfaces.trinidadinternal.context.TrinidadPhaseListener;

// Imported only for a String constant - so no runtime dependency
import com.sun.facelets.FaceletViewHandler;

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
 * @author The Oracle ADF Faces Team
 */
public class StateManagerImpl extends StateManager
{
  static public final String USE_APPLICATION_VIEW_CACHE_INIT_PARAM =
    "org.apache.myfaces.trinidad.USE_APPLICATION_VIEW_CACHE";

  // =-=AEW Should this really be public?
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
    return new PageState(structure, state, root);
  }

  static public UIViewRoot restoreViewRoot(
    FacesContext    context,
    Object          saved) throws ClassNotFoundException, InstantiationException,
                                  IllegalAccessException

  {
    if (saved == null)
      throw new NullPointerException();

    PageState viewState = (PageState) saved;

    UIViewRoot root = viewState.popRoot();
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

  public SerializedView saveSerializedView(FacesContext context)
  {
    if (!isSavingStateInClient(context))
      return _delegate.saveSerializedView(context);

    SerializedView view = _getCachedSerializedView(context);
    if (view != null)
      return view;

    UIViewRoot root = context.getViewRoot();
    boolean dontSave = false;

    // See if we're going to use the application view cache for
    // this request
    Map applicationViewCache = null;
    Map perSessionApplicationViewCache = null;
    if (_useApplicationViewCache(context))
    {
      // OK, we are: so find the application cache and
      // the per-session mirror
      applicationViewCache = _getApplicationViewCache(context);
      perSessionApplicationViewCache =
        _getPerSessionApplicationViewCache(context);

      synchronized (applicationViewCache)
      {
        // If we've already got a copy of the state stored, then
        // we just need to make sure it's mirrored on the session
        Object applicationState = applicationViewCache.get(root.getViewId());
        if (applicationState != null)
        {
          // Note that we've got no work to do...
          dontSave = true;
          perSessionApplicationViewCache.put(root.getViewId(),
                                             applicationState);
        }
      }
    }

    _removeTransientComponents(root);

    Object structure = (dontSave || !_needStructure(context))
                         ? null
                         : new Structure(root);
    Object state = dontSave ? null : root.processSaveState(context);

    if (_saveAsToken(context))
    {
      String token;
      if (applicationViewCache == null)
      {
        assert(!dontSave);
        TokenCache cache = _getViewCache(context);
        assert(cache != null);

        // Store bits of the session as subkeys off of the session
        Map stateMap = new SubKeyMap(
                         context.getExternalContext().getSessionMap(),
                         _VIEW_CACHE_KEY + ".");
        // Sadly, we can't save just a SerializedView, because we should
        // save a serialized object, and SerializedView is a *non*-static
        // inner class of StateManager
        PageState pageState = new PageState(
            structure,
            state,
            // Save the view root into the page state as a transient
            // if this feature has not been disabled
            _useViewRootCache(context) ? root : null);

        token = cache.addNewEntry(new PageState(structure, state, root),
                                  stateMap);
      }
      // If we got the "applicationViewCache", we're using it.
      else
      {
        // use null viewRoot since this state is shared across users:
        Object applicationState = new PageState(structure, state, null);
        // If we need to, stash the state off in our cache
        if (!dontSave)
        {
          synchronized (applicationViewCache)
          {
            applicationViewCache.put(root.getViewId(),
                                     applicationState);
            perSessionApplicationViewCache.put(root.getViewId(),
                                               applicationState);
          }
        }

        token = _APPLICATION_CACHE_TOKEN;
      }

      assert(token != null);

      // Create a "tokenView" which abuses SerializedView to store
      // our token only
      view = new SerializedView(token, null);
    }
    else
    {
      assert(!dontSave);
      view = new SerializedView(structure, state);
    }

    _saveCachedSerializedView(context, view);

    return view;
  }

  public void writeState(FacesContext context,
                         SerializedView state) throws IOException
  {
    _delegate.writeState(context, state);
  }

  public UIViewRoot restoreView(FacesContext context, String viewId,
                                String renderKitId)
  {
    if (!isSavingStateInClient(context))
      return _delegate.restoreView(context, viewId, renderKitId);

    final Object structure;
    final Object state;

    ResponseStateManager rsm = _getResponseStateManager(context, renderKitId);
    if (_saveAsToken(context))
    {
      Object token = rsm.getTreeStructureToRestore(context, viewId);
      if (token == null)
      {
        _LOG.finest("No token in the request for view \"{0}\";  " +
                    "probably a first view.", viewId);
        return null;
      }

      assert(token instanceof String);
      _LOG.finer("Restoring saved view state for token {0}", token);

      PageState viewState;

      // Load from the application cache
      if (_APPLICATION_CACHE_TOKEN.equals(token))
      {
        Map cache = _getApplicationViewCache(context);
        Map perSessionCache =
          _getPerSessionApplicationViewCache(context);

        // Synchronize on the application-level cache.
        // =-=AEW This may produce excessive contention
        synchronized (cache)
        {
          // Look first in the per-session cache
          viewState = (PageState) perSessionCache.get(viewId);
          if (viewState == null)
          {
            // Nope, it's not there.  Look in the application cache
            viewState = (PageState) cache.get(viewId);
            // And if we find it there, then push it back into
            // the per-session cache (it may have expired)
            if (viewState != null)
              perSessionCache.put(viewId, viewState);
          }

        }
      }
      else
      {
        Map stateMap = new SubKeyMap(
                         context.getExternalContext().getSessionMap(),
                         _VIEW_CACHE_KEY + ".");
        viewState = (PageState) stateMap.get((String) token);

        // Make sure that if the view state is present, the cache still
        // has the token, and vice versa

        // NOTE: it's very important that we call through to the
        // token cache here, not just inside the assert.  If we don't,
        // then we don't actually access the token, so it doesn't
        // get bumped up to the front in the LRU Cache!
        boolean isAvailable =
          _getViewCache(context).isAvailable((String) token);
        assert ((viewState != null) == isAvailable);
      }

      if (viewState == null)
      {
        _LOG.severe("Could not find saved view state for token {0}", token);
        return null;
      }

      _LOG.fine("Successfully found view state for token {0}", token);

      UIViewRoot root = viewState.popRoot(); // bug 4712492
      if (root != null)
      {
        _LOG.finer("UIViewRoot for token {0} already exists. Bypassing restoreState", token);
        // we need to create a new UIViewRoot because of bug 4719021:
        UIViewRoot newRoot = new UIViewRoot();
        newRoot.setId(root.getId());
        newRoot.setLocale(root.getLocale());
        newRoot.setViewId(root.getViewId());
        newRoot.setRenderKitId(root.getRenderKitId());
        // copy any render specific attributes.
        // adfc uses some:
        newRoot.getAttributes().putAll(root.getAttributes());
        // we need to use a temp list because as a side effect of
        // adding a child to a UIComponent, that child is removed from
        // the parent UIComponent. So the following will break:
        // newRoot.getChildren().addAll(root.getChildren());
        // because "root"'s child List is being mutated as the List
        // is traversed.
        List temp = new ArrayList(root.getChildCount());
        temp.addAll(root.getChildren());
        newRoot.getChildren().addAll(temp);

        return newRoot;
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
        _LOG.severe("No structure available and no root available");
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
        _LOG.severe("No structure available");
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

    return null;
  }

  public boolean isSavingStateInClient(FacesContext context)
  {
    return _delegate.isSavingStateInClient(context);
  }

  //
  // Protected APIs: we don't want
  //

  protected Object getTreeStructureToSave(FacesContext context)
  {
    throw new UnsupportedOperationException();
  }

  protected Object getComponentStateToSave(FacesContext context)
  {
    throw new UnsupportedOperationException();
  }

  protected UIViewRoot restoreTreeStructure
    (FacesContext context, String viewId, String renderKitId)
  {
    throw new UnsupportedOperationException();
  }

  protected void restoreComponentState
    (FacesContext context, UIViewRoot viewRoot, String renderKitId)
  {
    throw new UnsupportedOperationException();
  }


  private TokenCache _getViewCache(FacesContext context)
  {
    return TokenCache.getTokenCacheFromSession(context,
                                               _VIEW_CACHE_KEY,
                                               true,
                                               _getCacheSize(context));
  }

  /**
   * Tests whether to send a small string token, or the entire
   * serialized component state to the client-side.
   * @return true, if the small string token is to be sent to the client-side.
   */
  private boolean _saveAsToken(FacesContext context)
  {
    ExternalContext external = context.getExternalContext();
    Object clientMethod =
      external.getInitParameterMap().get(CLIENT_STATE_METHOD_PARAM_NAME);
    if ((clientMethod != null) &&
        CLIENT_STATE_METHOD_ALL.equalsIgnoreCase((String) clientMethod))
      return false;

    return true;
  }

  private int _getCacheSize(FacesContext context)
  {
    ExternalContext external = context.getExternalContext();
    Object maxTokens =
      external.getInitParameterMap().get(CLIENT_STATE_MAX_TOKENS_PARAM_NAME);
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
      }
    }

    return _DEFAULT_CACHE_SIZE;
  }

  //
  // @todo Map is a bad structure
  // @todo a static size is bad
  //
  static private Map _getApplicationViewCache(FacesContext context)
  {
    synchronized (_APPLICATION_VIEW_CACHE_LOCK)
    {
      Map appMap = context.getExternalContext().getApplicationMap();
      Map cache = (Map) appMap.get(_APPLICATION_VIEW_CACHE_KEY);
      if (cache == null)
      {
        cache = new HashMap(128);
        appMap.put(_APPLICATION_VIEW_CACHE_KEY, cache);
      }

      return cache;
    }
  }

  static private Map _getPerSessionApplicationViewCache(FacesContext context)
  {
    ExternalContext external = context.getExternalContext();
    Object session = external.getSession(true);
    assert(session != null);

    Map cache;
    // Synchronize on the session object to ensure that
    // we don't ever create two different caches
    synchronized (session)
    {
      cache = (Map) external.getSessionMap().get(_APPLICATION_VIEW_CACHE_KEY);
      if (cache == null)
      {
        cache = _createPerSessionApplicationViewCache();
        external.getSessionMap().put(_APPLICATION_VIEW_CACHE_KEY, cache);
      }
    }

    return cache;
  }

  //
  // For the per-session mirror of the application view cache,
  // use an LRU LinkedHashMap to store the latest 16 pages.
  //
  static private Map _createPerSessionApplicationViewCache()
  {
    return new LRUCache(_MAX_PER_SESSION_APPLICATION_SIZE);
  }

  static private final int _MAX_PER_SESSION_APPLICATION_SIZE = 16;

  //
  // Use the application view cache if and only if:
  // (1) We're saving state tokens on the client
  // (2) This is *not* a postback request
  // (3) The feature has been explicitly enabled
  //
  private boolean _useApplicationViewCache(FacesContext context)
  {
    if (_useApplicationViewCache == Boolean.FALSE)
      return false;

    if (_saveAsToken(context) &&
        !TrinidadPhaseListener.isPostback(context))
    {
      if (_useApplicationViewCache == null)
      {
        String s = context.getExternalContext().getInitParameter(
                                USE_APPLICATION_VIEW_CACHE_INIT_PARAM);
        _useApplicationViewCache =
          "true".equalsIgnoreCase(s) ? Boolean.TRUE : Boolean.FALSE;
      }

      return _useApplicationViewCache.booleanValue();
    }

    return false;
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
    if (_structureGeneratedByTemplate == null)
    {
      ExternalContext external = context.getExternalContext();
      String restoreMode = external.getInitParameter(
        FaceletViewHandler.PARAM_BUILD_BEFORE_RESTORE);
      if ("true".equals(restoreMode))
        _structureGeneratedByTemplate = Boolean.TRUE;
      else
        _structureGeneratedByTemplate = Boolean.FALSE;
    }

    return !_structureGeneratedByTemplate.booleanValue();
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

  static private void _removeTransientComponents(
    UIComponent root)
  {
    List components = new ArrayList();
    _gatherTransientComponents(root, components);
    Iterator iter = components.iterator();
    while (iter.hasNext())
    {
      UIComponent kid = (UIComponent) iter.next();
      UIComponent parent = kid.getParent();
      // First, see if its a child
      if (parent.getChildCount() > 0)
      {
        List children = parent.getChildren();
        if (children.remove(kid))
        {
          continue;
        }
      }

      // Nope, guess it's a facet
      Iterator facetNames = parent.getFacets().keySet().iterator();
      while (facetNames.hasNext())
      {
        String name = (String) facetNames.next();
        if (parent.getFacet(name) == kid)
        {
          parent.getFacets().remove(name);
          continue;
        }
      }

      // Couldn't find the component at all in its parent:  that's
      // not good.
      assert false;
    }
  }

  static private void _gatherTransientComponents(
    UIComponent component, List componentsToRemove)
  {
    Iterator kids = component.getFacetsAndChildren();
    while (kids.hasNext())
    {
      UIComponent kid = (UIComponent) kids.next();
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


  private SerializedView _getCachedSerializedView(
    FacesContext context)
  {
    return (SerializedView) context.getExternalContext().
                 getRequestMap().get(_CACHED_SERIALIZED_VIEW);
  }

  private void _saveCachedSerializedView(
    FacesContext context, SerializedView state)
  {
    context.getExternalContext().getRequestMap().put(_CACHED_SERIALIZED_VIEW,
                                                     state);
  }

  private static final class PageState implements Serializable
  {
    private final Object _structure, _state;
    // use transient since UIViewRoots are not Serializable.
    private transient UIViewRoot _root;

    public PageState(Object structure, Object state, UIViewRoot root)
    {
      _structure = structure;
      _state = state;
      _root = root;
    }

    public Object getStructure()
    {
      return _structure;
    }

    public Object getState()
    {
      return _state;
    }

    // we need to synchronize because we are mutating _root
    // which is shared between simultaneous requests from the same user:
    public synchronized UIViewRoot popRoot()
    {
      if (_root != null)
      {
        UIViewRoot root = _root;
        // we must clear the cached viewRoot. This is because UIComponent trees
        // are mutable and if the back button
        // is used to come back to an old PageState, then it would be
        // really bad if we reused that component tree:
        _root = null;
        return root;
      }
      return null;
    }
  }


  /* =-=AEW A utility function to dump out all the state that's getting
     sent over.  To compile, you need to make most of TreeState public */
  /*
  static private void _dumpState(Object state, int depth)
  {
    String out = _SPACES.substring(0, depth * 2);
    Object objectState = state;

    if (state instanceof org.apache.myfaces.trinidad.component.TreeState)
      objectState = ((org.apache.myfaces.trinidad.component.TreeState) state)._state;

    if (objectState == null)
      out += "null";
    else if (objectState instanceof Object[])
      out += "array";
    else
      out += objectState.toString() + "(" + objectState.getClass() + ")";

    if (objectState instanceof Object[])
    {
      Object[] array = (Object[]) objectState;
      for (int i = 0; i < array.length; i++)
        _dumpState(array[i], depth + 1);
    }


    if (state instanceof org.apache.myfaces.trinidad.component.TreeState)
    {
      Object[] array = ((org.apache.myfaces.trinidad.component.TreeState)state)._children;
      if (array != null)
      {
        for (int i = 0; i < array.length; i++)
          _dumpState(array[i], depth + 1);
      }

      array = ((org.apache.myfaces.trinidad.component.TreeState)state)._facets;
      if (array != null)
      {
        for (int i = 0; i < array.length; i++)
          _dumpState(array[i], depth + 1);
      }
    }
  }

  static private final String _SPACES = "                                                                            ";
  */

  private final StateManager _delegate;
  private       Boolean      _useViewRootCache;
  private       Boolean      _useApplicationViewCache;
  private       Boolean      _structureGeneratedByTemplate;


  private static final int _DEFAULT_CACHE_SIZE = 15;

  private static final Object _APPLICATION_VIEW_CACHE_LOCK = new Object();
  private static final String _VIEW_CACHE_KEY =
    "org.apache.myfaces.trinidadinternal.application.VIEW_CACHE";

  private static final String _APPLICATION_VIEW_CACHE_KEY =
    "org.apache.myfaces.trinidadinternal.application.APPLICATION_VIEW_CACHE";

  private static final String _CACHED_SERIALIZED_VIEW =
    "org.apache.myfaces.trinidadinternal.application.CachedSerializedView";



  private static final String _APPLICATION_CACHE_TOKEN = "_a_";

  private static final ADFLogger _LOG = ADFLogger.createADFLogger(StateManagerImpl.class);
}
