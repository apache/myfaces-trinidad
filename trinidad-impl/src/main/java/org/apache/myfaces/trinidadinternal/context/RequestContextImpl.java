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

import java.awt.Color;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.change.NullChangeManager;
import org.apache.myfaces.trinidad.change.SessionChangeManager;
import org.apache.myfaces.trinidad.component.UIXCollection;
import org.apache.myfaces.trinidad.config.RegionManager;
import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.DialogService;
import org.apache.myfaces.trinidad.context.PageFlowScopeProvider;
import org.apache.myfaces.trinidad.context.PageResolver;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.util.ComponentUtils;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;
import org.apache.myfaces.trinidadinternal.agent.AgentFactory;
import org.apache.myfaces.trinidadinternal.agent.AgentFactoryImpl;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgentImpl;
import org.apache.myfaces.trinidadinternal.application.StateManagerImpl;
import org.apache.myfaces.trinidadinternal.el.FormatterMap;
import org.apache.myfaces.trinidadinternal.el.HelpProvider;
import org.apache.myfaces.trinidadinternal.el.OracleHelpProvider;
import org.apache.myfaces.trinidadinternal.metadata.RegionMetadata;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;
import org.apache.myfaces.trinidadinternal.share.config.UIXCookie;
import org.apache.myfaces.trinidadinternal.ui.expl.ColorPaletteUtils;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;
import org.apache.myfaces.trinidadinternal.webapp.TrinidadFilterImpl;

/**
 */
public class RequestContextImpl extends RequestContext
{
  static public final String LAUNCH_PARAMETERS =
    "org.apache.myfaces.trinidad.PageFlowSourceParameters";

  static public final String LAUNCH_VIEW =
    "org.apache.myfaces.trinidad.PageFlowSourceView";


  public RequestContextImpl(RequestContextBean bean)
  {
    _bean = bean;
    _dialogService = new DialogServiceImpl(this);
    _partialTargets = new HashSet<String>();
  }

  public void init(ExternalContext request)
  {
    attach();
  }


  @Override
  public DialogService getDialogService()
  {
    return _dialogService;
  }

  @Override
  public PageResolver getPageResolver()
  {
    return _pageResolver;
  }

  @Override
  public PageFlowScopeProvider getPageFlowScopeProvider()
  {
    return _pageFlowScopeProvider;
  }

  @Override
  public Map<String, Object> getPageFlowScope()
  {
    return _pageFlowScopeProvider.getPageFlowScope(__getFacesContext());
  }


  @Override
  public void returnFromDialog(Object returnValue, Map<Object, Object> returnParameters)
  {
    boolean dialogIsInaccessible =
      _dialogService.returnFromDialog(returnValue, returnParameters);
    _pageFlowScopeProvider.popPageFlowScope(__getFacesContext(),
                                            dialogIsInaccessible);
  }



  /**
   * Launch a dialog.
   * @todo Don't save parameters for state-saving, page-flow scope, etc.
   */
  @Override
  public void launchDialog(
    UIViewRoot  dialogRoot,
    Map<String, Object> dialogParameters,
    UIComponent source,
    boolean     useWindow,
    Map<String, Object> windowProperties)
  {
    _pageFlowScopeProvider.pushPageFlowScope(__getFacesContext(),
                                             true);
    _dialogService.launchDialog(dialogRoot,
                                dialogParameters,
                                source,
                                useWindow,
                                windowProperties);
  }

  @Override
  public UploadedFileProcessor getUploadedFileProcessor()
  {
    return (UploadedFileProcessor)
      _bean.getProperty(RequestContextBean.UPLOADED_FILE_PROCESSOR_KEY);
  }


  @Override
  public boolean isPostback()
  {
    FacesContext context = __getFacesContext();
    // First, see if this is definitely not a postback request
    if (!TrinidadPhaseListener.isPostback(context))
      return false;

    // Second, see if we are still on the original view root (which
    // the PhaseListener's isPostback() method doesn't look at).
    UIViewRoot originalViewRoot = (UIViewRoot)
      context.getExternalContext().getRequestMap().
         get(TrinidadPhaseListener.INITIAL_VIEW_ROOT_KEY);

    // However, we only set the "originalViewRoot" during the afterPhase()
    // of our PhaseListener;  so, if it's null, then we have to asume
    // that we have yet to even reach this code, as in, for example,
    // an afterPhase() phase listener method for Restore View that happens
    // to run before ours (which happens if it's registered *after* ours,
    // because afterPhase() runs in inverse order).  If so, there certainly
    // hasn't been any navigation, so consider it a postback request.
    if ((originalViewRoot != null) &&
        (originalViewRoot != context.getViewRoot()))
      return false;

    return true;
  }
  
  @SuppressWarnings("unchecked")
  @Override
  public boolean isPartialRequest(FacesContext context)
  {
    return CoreRenderKit.isPartialRequest(context.getExternalContext());
  }


  @Override
  public boolean isDebugOutput()
  {
    return Boolean.TRUE.equals(
       _bean.getProperty(RequestContextBean.DEBUG_OUTPUT_KEY));
  }

  @Override
  public boolean isClientValidationDisabled()
  {
    return (ClientValidation.DISABLED == getClientValidation());
  }

  @Override
  public String getOutputMode()
  {
    //=-= Scott O'Bryan =-=
    // FIXME: Not real happy with this.  We should find a way to get this into
    // the bean.  The bean is cached by the RequestContextFactory, and the 
    // Portlet mode needs to be assigned per request since it's possible to run
    // a trinidad application from a servlet container and a portlet container
    // at the same time.  For now?  Hey, it works.
    
    if(ExternalContextUtils.isPortlet(__getFacesContext().getExternalContext()))
    {
      return CoreRenderKit.OUTPUT_MODE_PORTLET;
    }
    return (String) _bean.getProperty(RequestContextBean.OUTPUT_MODE_KEY);
  }

  @Override
  public String getSkinFamily()
  {
    return (String) _bean.getProperty(RequestContextBean.SKIN_FAMILY_KEY);
  }

  @Override
  public Accessibility getAccessibilityMode()
  { 
    String name = (String) _bean.getProperty(
      RequestContextBean.ACCESSIBILITY_MODE_KEY);
    if (name == null)
    {
      UIXCookie cookie = _getUIXCookie();
      if (cookie != null)
      {
        if (cookie.getAccessibilityMode() != null)
          name = cookie.getAccessibilityMode().toString();
      }
    }

    return _ACCESSIBILITY_NAMES.get(name);
  }

  @Override
  public AccessibilityProfile getAccessibilityProfile()
  { 
    return (AccessibilityProfile) _bean.getProperty(
      RequestContextBean.ACCESSIBILITY_PROFILE_KEY);
  }

  @Override
  public ClientValidation getClientValidation()
  { 
    ClientValidation clientValidation = (ClientValidation)
      _bean.getProperty(RequestContextBean.CLIENT_VALIDATION_KEY);

    if (clientValidation == null)
      clientValidation = ClientValidation.INLINE;

    // Force use of ALERT validation (instead of INLINE) if using
    // screen reader mode
    if ((clientValidation == ClientValidation.INLINE) &&
        (getAccessibilityMode() == RequestContext.Accessibility.SCREEN_READER))
      clientValidation = ClientValidation.ALERT;

    return clientValidation;
  }

  @Override
  public boolean isAnimationEnabled()
  {
    return !Boolean.FALSE.equals(_bean.getProperty(RequestContextBean.ANIMATION_ENABLED_KEY));
  }
  
  @Override
  public char getNumberGroupingSeparator()
  {

    Object property = _bean.getProperty(
      RequestContextBean.NUMBER_GROUPING_SEPARATOR_KEY);
    char c = CoreRenderer.toChar(property);
    if (c != CoreRenderer.CHAR_UNDEFINED)
      return c;

    return (char) 0;
  }

  @Override
  public char getDecimalSeparator()
  {
    Object property = _bean.getProperty(
      RequestContextBean.DECIMAL_SEPARATOR_KEY);
    char c = CoreRenderer.toChar(property);
    if (c != CoreRenderer.CHAR_UNDEFINED)
      return c;
      
    return (char) 0;
  }


  @Override
  public TimeZone getTimeZone()
  {
    TimeZone tz = (TimeZone) _bean.getProperty(RequestContextBean.TIME_ZONE_KEY);
    if (tz != null)
      return tz;

    // see bug 4960813
    return TimeZone.getDefault();
    // see bug 4960813. we can't guess the timezone in javascript:

//    UIXCookie cookie = _getUIXCookie();
//    if (cookie != null)
//    {
//      if (cookie.getTimeZone() != null)
//        return cookie.getTimeZone();
//    }

//    return null;
  }

  /**
   * {@inheritDoc}
   */
  @SuppressWarnings("unchecked")
  @Override
  public ChangeManager getChangeManager()
  {
    FacesContext context = __getFacesContext();
    Map<String, Object> appMap = context.getExternalContext().getApplicationMap();
    ChangeManager changeManager = (ChangeManager)appMap.get(_CHANGE_MANAGER_KEY);

    if (changeManager == null)
    {
      changeManager = _createChangeManager();
      appMap.put(_CHANGE_MANAGER_KEY, changeManager);
    }
    return changeManager;
  }

  private ChangeManager _createChangeManager()
  {
    FacesContext context = __getFacesContext();
    String changeManager =
      context.getExternalContext().getInitParameter(
        _CHANGE_PERSISTENCE_INIT_PARAM);
    if (changeManager != null)
    {
      // Support the "session" token
      if ("session".equalsIgnoreCase(changeManager))
      {
        _LOG.info("HTTPSESSION_USED_FOR_CHANGE_PERSISTENCE");
        return new SessionChangeManager();
      }
      // Otherwise, just assume its a class name.
      else
      {
        return _createChangeManager(changeManager);
      }
    }
    return new NullChangeManager();
  }

  /**
   * Indirectly instantiate ChangeManager classes that have external runtime
   * dependencies, so that we don't create link dependencies.
   * @param className
   * @return The ChangeManager instance
   */
  private ChangeManager _createChangeManager(
    String className)
  {
    try
    {
      Class<?> managerClass = ClassLoaderUtils.loadClass(className);
      return (ChangeManager)managerClass.newInstance();
    }
    catch (Throwable throwable)
    {
      _LOG.warning("CHANGE_MANAGER_CREATION_FAILED", className);
      _LOG.warning(throwable);
      return new NullChangeManager();
    }
  }


  @Override
  public RegionManager getRegionManager()
  {
    FacesContext context = __getFacesContext();
    return RegionMetadata.getRegionMetadata(context);
  }

  @Override
  public String getCurrencyCode()
  {
    return (String) _bean.getProperty(RequestContextBean.CURRENCY_CODE_KEY);
  }

  @Override
  public String getOracleHelpServletUrl()
  {
    return (String) _bean.getProperty(
     RequestContextBean.ORACLE_HELP_SERVLET_URL_KEY);
  }

  @Override
  public boolean isRightToLeft()
  {
    Boolean b = (Boolean) _bean.getProperty(RequestContextBean.RIGHT_TO_LEFT_KEY);
    if (b != null)
      return b.booleanValue();

    FacesContext fContext = __getFacesContext();
    if ((fContext != null) && (fContext.getViewRoot() != null))
    {
      Locale locale = fContext.getViewRoot().getLocale();
      return (LocaleUtils.getReadingDirectionForLocale(locale) ==
              LocaleUtils.DIRECTION_RIGHTTOLEFT);
    }

    return false;
  }

  @Override
  public Locale getFormattingLocale()
  {
    Object o = _bean.getProperty(RequestContextBean.FORMATTING_LOCALE_KEY);
    if (o == null)
      return null;

    if (o instanceof Locale)
      return (Locale) o;
    
    // Don't know how this would ever get here.  ConfigParser should have set the key if
    // formatting-locale was specified, or it is null.
    if (o instanceof String)  
      o = ((String)o).replace('_', '-');
    return LocaleUtils.getLocaleForIANAString(o.toString());
  }


  @Override
  public Map<String, Object> getHelpTopic()
  {
    HelpProvider provider = _getHelpProvider();
    if (provider == null)
      return null;

    return provider.getHelpTopicMap();
  }

  @Override
  public Map<String, Object> getHelpSystem()
  {
    HelpProvider provider = _getHelpProvider();
    if (provider == null)
      return null;

    return provider.getHelpSystemMap();
  }

  //
  // Partial Page Rendering support
  //

  @Override
  public void addPartialTarget(UIComponent newTarget)
  {
    FacesContext fContext = __getFacesContext();

    RenderingContext afContext = RenderingContext.getCurrentInstance();

    PartialPageContext pContext = null;

    if (afContext != null)
      pContext = afContext.getPartialPageContext();

    // find the nearest ancestor that generates html markup:
    newTarget = _getNearestPPRTarget(newTarget);

    Object savedKey = null;
    // =-=AEW Force the rowkey of a collection back to null so that the clientId
    // will be correct.  Note that in JSF 1.2, this will be unnecessary
    if (newTarget instanceof UIXCollection)
    {
      savedKey = ((UIXCollection) newTarget).getRowKey();
      if (savedKey != null)
        ((UIXCollection) newTarget).setRowKey(null);
    }

    String clientId = newTarget.getClientId(fContext);

    // Restore the row key
    if (savedKey != null)
    {
      ((UIXCollection) newTarget).setRowKey(savedKey);
    }

    _LOG.finer("Adding partial target: {0}", newTarget);

    if (pContext != null)
    {
      pContext.addPartialTarget(clientId);
    }
    else
    {
      // If we haven't built the partial context yet, maintain a list of the
      // target IDs that have requested partial update.
      _partialTargets.add(clientId);
    }
  }
  
  /**
   * @see org.apache.myfaces.trinidad.context.RequestContext#addPartialTargets(javax.faces.component.UIComponent, java.lang.String[])
   */
  @Override
  public void addPartialTargets(UIComponent from, String... targets)
  {
    if (targets == null)
    {
      return;
    }
    for (String target : targets)
    {
      UIComponent component = ComponentUtils.findRelativeComponent(from, target);
      if (component != null)
      {
        addPartialTarget(component);
      }
    }
  }

  @Override
  public void addPartialTriggerListeners
    (UIComponent listener,
     String[] triggers)
  {
    if ((listener == null) || (triggers == null))
      return;

    Map<UIComponent, Set<UIComponent>> pl = _getPartialListeners();

    for (int i = 0; i < triggers.length; i++)
    {
      String trigger = triggers[i];

      // Look for the master component.  Note that if the listener is itself 
      // a naming container, we don't want to restrict ourselves to looking
      // inside - we want to look outside instead (at least, that was
      // the old ADF Faces rules, and now we should stick with it for
      // backwards compatibility even within Trinidad)
      
      UIComponent master = ComponentUtils.findRelativeComponent(listener, trigger);
      
      boolean deprecatedFind = false;
    
      if (master == null)
      {
        UIComponent from = listener;
        // backward compatible code
        // The old rule is "if the component is a naming container, search relative 
        // to the parent; otherwise, search relative to the component." 
      if (listener instanceof NamingContainer)
      {
        from = listener.getParent();
        master = ComponentUtils.findRelativeComponent(from, trigger);
        deprecatedFind = true;
      }
      }

      if (master == null)
      {
        _LOG.warning("CANNOT_FIND_PARTIAL_TRIGGER", new Object[] {trigger, listener});
      }
      else
      {
        // if we found this with the deprecated method, 
        // then warn the user to change their syntax.
        if (deprecatedFind)
        {
          _LOG.warning("DEPRECATED_TRIGGER_SYNTAX", 
            new Object[] {trigger, listener});
        }
      
        // Get the set of listeners on this trigger and add this component.
        Set<UIComponent> listeners = pl.get(master);
        if (listeners == null)
        {
          listeners = new HashSet<UIComponent>();
          pl.put(master, listeners);
        }
        listeners.add(listener);
      }
    }
  }

  @Override
  public void partialUpdateNotify(UIComponent updated)
  {
    if (updated != null)
    {
      // Wildcards removed for now. If there's a demonstrated need for global
      // triggers, reinstate this.

      // and always do global
      // _addTargets(_GLOBAL_TRIGGER);

      // now do all listeners
      _addTargets(updated);
    }
  }

  /**
   * Get the clientIds of all components that have requested partial update
   */
  public Iterator<String> getPartialTargets()
  {
    return _partialTargets.iterator();
  }

  /**
   * Get the clientIds of all components that have been updated (If a component
   * is listening on this component, it will add itself to the partialTargets
   * list).
   */
  public Set<String> getPartialUpdates()
  {
    return _partialTargets;
  }

  @Override
  public Map<String, List<Color>> getColorPalette()
  {
    return ColorPaletteUtils.getColorPaletteMap();
  }

  @Override
  public Map<Object, Map<Object,String>> getFormatter()
  {
    return FormatterMap.sharedInstance();
  }

  @Override
  public int getTwoDigitYearStart()
  {
    Integer twoDigitYearStart  = (Integer) _bean.getProperty(
      RequestContextBean.TWO_DIGIT_YEAR_START);

    if (twoDigitYearStart != null)
      return twoDigitYearStart.intValue();

    return 1950;
  }

  @Override
  public Agent getAgent()
  {
    if (_agent == null)
    {
      Agent agent = _agentFactory.createAgent(__getFacesContext());
      // =-=AEW In theory, this does not need to be a TrinidadAgent
      // That should only be necessary once we get to rendering...
      // However, we're gonna have to turn it into one when it comes
      // to rendering time, and our RenderingContext isn't doing this
      // today
      TrinidadAgentImpl fAgent = new TrinidadAgentImpl(__getFacesContext(),agent);
      _agent = fAgent;
    }

    return _agent;
  }

  @Override
  public Object saveComponent(UIComponent component)
  {
    return StateManagerImpl.saveComponentTree(__getFacesContext(),
                                              component);
  }

  @Override
  public UIComponent restoreComponent(Object state)
                            throws ClassNotFoundException,
                                   InstantiationException,
                                   IllegalAccessException
  {
    return StateManagerImpl.restoreComponentTree(__getFacesContext(),
                                                 state);
  }

  void __setPageResolver(PageResolver pageResolver)
  {
    _pageResolver = pageResolver;
  }

  void __setPageFlowScopeProvider(PageFlowScopeProvider pageFlowScopeProvider)
  {
    _pageFlowScopeProvider = pageFlowScopeProvider;
  }

  int __getPageFlowScopeLifetime()
  {
    Integer lifetimeObj = (Integer) _bean.getProperty(
                           RequestContextBean.PAGE_FLOW_SCOPE_LIFETIME_KEY);
    if (lifetimeObj == null)
      return _DEFAULT_PAGE_FLOW_SCOPE_LIFETIME;
    return lifetimeObj.intValue();
  }


  private void _addTargets(Object key)
  {
    Map<UIComponent, Set<UIComponent>> pl = _getPartialListeners();
    Set<UIComponent> listeners = pl.get(key);
    if (listeners != null)
    {
      // protect from infinite recursion
      pl.remove(key);

      for(UIComponent listener : listeners)
      {
        addPartialTarget(listener);
        // This target will be re-rendered, re-render anything that's
        // listening on it also.
        partialUpdateNotify(listener);
      }
    }
  }

  private HelpProvider _getHelpProvider()
  {
    if (_provider == null)
    {
      String url = getOracleHelpServletUrl();
      if (url != null)
        _provider = new OracleHelpProvider(url);
    }

    return _provider;
  }

  //
  // Get the FacesContext.  We used to cache the instance, but
  // in some circumstances the RequestContext was getting reused
  // across multiple FacesServlet invocations, so the caching
  // was more trouble than it was worth.  Re-enable some sort
  // of caching if it proves a performance issue.
  //
  FacesContext __getFacesContext()
  {
    FacesContext fContext = FacesContext.getCurrentInstance();
    // If we haven't hit the FacesServlet yet, then the
    // FacesContext won't be available yet - so go
    // to the filter and ask for a pseudo-FacesContext
    if (fContext == null)
    {
      fContext = TrinidadFilterImpl.getPseudoFacesContext();
    }

    return fContext;
  }

  /**
   * Components that do not render any html-markup cannot be
   * added as PPR targets. This method will walk up the component
   * tree finding the nearest ancestor that does render markup.
   * Components that return null for {@link UIComponent#getRendererType}
   * are treated as generating no markup.
   * @param component This component, and its ancestors will be searched.
   * @return the first component that does render html-markup.
   */
  private UIComponent _getNearestPPRTarget(UIComponent component)
  {
    while(component.getRendererType() == null)
    {
      component = component.getParent();
    }
    return component;
  }

  private UIXCookie _getUIXCookie()
  {
    FacesContext fContext = __getFacesContext();
    if (fContext == null)
      return null;

    Object request = fContext.getExternalContext().getRequest();
    Object response = fContext.getExternalContext().getResponse();

    if ((request instanceof HttpServletRequest) &&
        (response instanceof HttpServletResponse))
    {
      return UIXCookie.getUIXCookie((HttpServletRequest) request,
                                    (HttpServletResponse) response);
    }

    return null;
  }

  private Map<UIComponent, Set<UIComponent>> _getPartialListeners()
  {
    if (_partialListeners == null)
      _partialListeners = new HashMap<UIComponent, Set<UIComponent>>();

    return _partialListeners;
  }
  
  private RequestContextBean _bean;
  private HelpProvider        _provider;
  private Map<UIComponent, Set<UIComponent>> _partialListeners;
  private Set<String>         _partialTargets;
  private Agent               _agent;

  private DialogServiceImpl   _dialogService;

  private PageResolver        _pageResolver;
  private PageFlowScopeProvider _pageFlowScopeProvider;

  //todo: get factory from configuration (else implementations have to provide their own RequestContext)
  static private final AgentFactory _agentFactory = new AgentFactoryImpl();

  // static private final Object _GLOBAL_TRIGGER = new Object();
  static private final int    _DEFAULT_PAGE_FLOW_SCOPE_LIFETIME = 15;
  static private final String _CHANGE_MANAGER_KEY =
    "org.apache.myfaces.trinidadinternal.ChangeManager";
  static private final String _CHANGE_PERSISTENCE_INIT_PARAM =
    "org.apache.myfaces.trinidad.CHANGE_PERSISTENCE";

  // A mapping from string names (as used in the config file)
  // to accessibility objects
  static private final Map<String, Accessibility>
    _ACCESSIBILITY_NAMES = new HashMap<String, Accessibility>();
  
  static
  {
    _ACCESSIBILITY_NAMES.put("default", Accessibility.DEFAULT);
    _ACCESSIBILITY_NAMES.put("inaccessible", Accessibility.INACCESSIBLE);
    _ACCESSIBILITY_NAMES.put("screenReader", Accessibility.SCREEN_READER);
  }

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(RequestContextImpl.class);
}
