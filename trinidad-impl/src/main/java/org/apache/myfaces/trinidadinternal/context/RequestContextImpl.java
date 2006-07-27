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


import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.myfaces.trinidad.logging.ADFLogger;
import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.component.UIXCollection;
import org.apache.myfaces.trinidad.config.RegionManager;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.DialogService;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.PageResolver;
import org.apache.myfaces.trinidad.context.PageFlowScopeProvider;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;

import org.apache.myfaces.trinidadinternal.agent.AdfFacesAgentImpl;
import org.apache.myfaces.trinidadinternal.agent.AgentFactory;
import org.apache.myfaces.trinidadinternal.agent.AgentFactoryImpl;

import org.apache.myfaces.trinidadinternal.change.NullChangeManager;
import org.apache.myfaces.trinidadinternal.change.SessionChangeManager;

import org.apache.myfaces.trinidadinternal.el.FormatterMap;
import org.apache.myfaces.trinidadinternal.el.HelpProvider;
import org.apache.myfaces.trinidadinternal.el.OracleHelpProvider;

import org.apache.myfaces.trinidadinternal.metadata.RegionMetadata;

import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.PartialPageContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.trinidadinternal.share.config.UIXCookie;

import org.apache.myfaces.trinidadinternal.ui.expl.ColorPaletteUtils;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

import org.apache.myfaces.trinidadinternal.webapp.TrinidadFilterImpl;

/**
 * @author The Oracle ADF Faces Team
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
  }

  public void init(Object request)
  {
    attach();
  }


  public DialogService getDialogService()
  {
    return _dialogService;
  }

  public PageResolver getPageResolver()
  {
    return _pageResolver;
  }

  public PageFlowScopeProvider getPageFlowScopeProvider()
  {
    return _pageFlowScopeProvider;
  }

  public Map getPageFlowScope()
  {
    return _pageFlowScopeProvider.getPageFlowScope(__getFacesContext());
  }


  public void returnFromDialog(Object returnValue, Map returnParameters)
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
  public void launchDialog(
    UIViewRoot  dialogRoot,
    Map         dialogParameters,
    UIComponent source,
    boolean     useWindow,
    Map         windowProperties)
  {
    _pageFlowScopeProvider.pushPageFlowScope(__getFacesContext(),
                                             true);
    _dialogService.launchDialog(dialogRoot,
                                dialogParameters,
                                source,
                                useWindow,
                                windowProperties);
  }

  public UploadedFileProcessor getUploadedFileProcessor()
  {
    return (UploadedFileProcessor)
      _bean.getProperty(RequestContextBean.UPLOADED_FILE_PROCESSOR_KEY);
  }


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
  
  public boolean isPartialRequest(FacesContext context)
  {
    Map requestMap = context.getExternalContext().getRequestMap();
    if (Boolean.TRUE.equals(requestMap.get(XhtmlConstants.PARTIAL_PARAM)))
      return true;
    
    Map parameters = context.getExternalContext().getRequestParameterMap();
    if ("true".equals(parameters.get("partial")))
      return true;

    return false;
  }


  public boolean isDebugOutput()
  {
    return Boolean.TRUE.equals(
       _bean.getProperty(RequestContextBean.DEBUG_OUTPUT_KEY));
  }

  public boolean isClientValidationDisabled()
  {
    return Boolean.TRUE.equals(
       _bean.getProperty(RequestContextBean.CLIENT_VALIDATION_DISABLED_KEY));
  }

  public String getOutputMode()
  {
    return (String) _bean.getProperty(RequestContextBean.OUTPUT_MODE_KEY);
  }

  // get skinFamily; default to minimal if nothing is specified.
  public String getSkinFamily()
  {
    String skinFamily =
      (String) _bean.getProperty(RequestContextBean.SKIN_FAMILY_KEY);
    if (skinFamily == null)
      skinFamily = "minimal";
    return skinFamily;
  }

  public String getAccessibilityMode()
  {
    String s = (String) _bean.getProperty(
      RequestContextBean.ACCESSIBILITY_MODE_KEY);
    if (s != null)
      return s;

    UIXCookie cookie = _getUIXCookie();
    if (cookie != null)
    {
      if (cookie.getAccessibilityMode() != null)
        return cookie.getAccessibilityMode().getName();
    }

    return null;
  }

  public char getNumberGroupingSeparator()
  {
    Character c = (Character) _bean.getProperty(
      RequestContextBean.NUMBER_GROUPING_SEPARATOR_KEY);
    if (c != null)
      return c.charValue();

    return (char) 0;
  }

  public char getDecimalSeparator()
  {
    Character c = (Character) _bean.getProperty(
      RequestContextBean.DECIMAL_SEPARATOR_KEY);
    if (c != null)
      return c.charValue();

    return (char) 0;
  }


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
  public ChangeManager getChangeManager()
  {
    FacesContext context = __getFacesContext();
    Map appMap = context.getExternalContext().getApplicationMap();
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
        _LOG.info("ADF Faces is using HTTPSession for change persistence");
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
      Class managerClass = ClassLoaderUtils.loadClass(className);
      return (ChangeManager)managerClass.newInstance();
    }
    catch (Throwable throwable)
    {
      _LOG.warning("Unable to create ChangeManager:" + className,
                   throwable);
      return new NullChangeManager();
    }
  }


  public RegionManager getRegionManager()
  {
    FacesContext context = __getFacesContext();
    return RegionMetadata.getRegionMetadata(context);
  }

  public String getCurrencyCode()
  {
    return (String) _bean.getProperty(RequestContextBean.CURRENCY_CODE_KEY);
  }

  public String getOracleHelpServletUrl()
  {
    return (String) _bean.getProperty(
     RequestContextBean.ORACLE_HELP_SERVLET_URL_KEY);
  }

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


  public Map getHelpTopic()
  {
    HelpProvider provider = _getHelpProvider();
    if (provider == null)
      return null;

    return provider.getHelpTopicMap();
  }

  public Map getHelpSystem()
  {
    HelpProvider provider = _getHelpProvider();
    if (provider == null)
      return null;

    return provider.getHelpSystemMap();
  }

  //
  // Partial Page Rendering support
  //

  public void addPartialTarget(UIComponent newTarget)
  {
    FacesContext fContext = __getFacesContext();

    AdfRenderingContext afContext = AdfRenderingContext.getCurrentInstance();

    PartialPageContext pContext = null;

    if (afContext != null)
      pContext = afContext.getPartialPageContext();

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

  public void addPartialTriggerListeners
    (UIComponent listener,
     String[] triggers)
  {
    if ((listener == null) || (triggers == null))
      return;

    Map pl = _getPartialListeners();

    for (int i = 0; i < triggers.length; i++)
    {
      String trigger = triggers[i];

      Object master;

      // Wildcards removed for now....
      // if ("*".equals(trigger))
      //   master = _GLOBAL_TRIGGER;
      // else
      master = listener.getParent().findComponent(trigger);

      // Get the set of listeners on this trigger and add this component.
      Set listeners = (Set) pl.get(master);
      if (listeners == null)
      {
        listeners = new HashSet();
        pl.put(master, listeners);
      }
      listeners.add(listener);
    }
  }

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
  public Iterator getPartialTargets()
  {
    return _partialTargets.iterator();
  }

  /**
   * Get the clientIds of all components that have been updated (If a component
   * is listening on this component, it will add itself to the partialTargets
   * list).
   */
  public Set getPartialUpdates()
  {
    return _partialTargets;
  }

  public Map getColorPalette()
  {
    return ColorPaletteUtils.getColorPaletteMap();
  }

  public Map getFormatter()
  {
    return FormatterMap.sharedInstance();
  }

  public int getTwoDigitYearStart()
  {
    Integer twoDigitYearStart  = (Integer) _bean.getProperty(
      RequestContextBean.TWO_DIGIT_YEAR_START);

    if (twoDigitYearStart != null)
      return twoDigitYearStart.intValue();

    return 1950;
  }

  public Agent getAgent()
  {
    if (_agent == null)
    {
      Agent agent = _agentFactory.createAgent(__getFacesContext());
      // =-=AEW Does this need to be an AdfFacesAgent?  That should
      // only be necessary once we get to rendering...
      AdfFacesAgentImpl fAgent = new AdfFacesAgentImpl(__getFacesContext(),agent);
      _agent = fAgent;
    }

    return _agent;
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
    Map pl = _getPartialListeners();
    Set listeners = (Set) pl.get(key);
    if (listeners != null)
    {
      // protect from infinite recursion
      pl.remove(key);

      Iterator iter = listeners.iterator();
      while (iter.hasNext())
      {
        UIComponent listener = (UIComponent) iter.next();
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

  private Map _getPartialListeners()
  {
    if (_partialListeners == null)
      _partialListeners = new HashMap();

    return _partialListeners;
  }


  private RequestContextBean _bean;
  private HelpProvider        _provider;
  private Map                 _partialListeners;
  private Set                 _partialTargets = new HashSet();
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

  static private final ADFLogger _LOG =
    ADFLogger.createADFLogger(RequestContextImpl.class);
}
