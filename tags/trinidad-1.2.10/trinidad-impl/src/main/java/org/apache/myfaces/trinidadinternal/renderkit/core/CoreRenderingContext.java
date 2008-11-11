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
package org.apache.myfaces.trinidadinternal.renderkit.core;

import java.beans.Beans;

import java.io.File;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.faces.context.FacesContext;

import javax.servlet.ServletContext;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidadinternal.agent.AgentUtil;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgentImpl;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PartialPageUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.share.nls.MutableDecimalFormatContext;
import org.apache.myfaces.trinidadinternal.share.nls.MutableLocaleContext;
import org.apache.myfaces.trinidadinternal.skin.RequestSkinWrapper;
import org.apache.myfaces.trinidadinternal.skin.SkinNotAvailable;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;


public class CoreRenderingContext extends RenderingContext
{
  /**
   * String marker used to indicate the style class is empty and can
   * be ignored.
   */
  static public final String EMPTY_STYLE_CLASS = "";

  public CoreRenderingContext()
  {
    FacesContext context = FacesContext.getCurrentInstance();
    RequestContext afContext = RequestContext.getCurrentInstance();

    _properties = new HashMap<Object, Object>();

    _outputMode = afContext.getOutputMode();
    _agent = _initializeAgent(context,
                              afContext.getAgent(),
                              // Go back through getOutputMode()
                              // in case anyone has overidden getOutputMode()
                              getOutputMode());

    _initializeSkin(context, afContext);
    _initializePPR(context, afContext);
    // Get and cache (since it can be EL-bound)
    _accessibilityMode = afContext.getAccessibilityMode();
    _animationEnabled = afContext.isAnimationEnabled();

    // Initialize the accessibility profile, providing a default
    // instance if necessary.
    _accessibilityProfile = afContext.getAccessibilityProfile();
    if (_accessibilityProfile == null)
      _accessibilityProfile = AccessibilityProfile.getDefaultInstance();
  }


  /**
   * @TODO is there a better way when link renderer moved?
   */
  public boolean isDefaultLinkStyleDisabled()
  {
    return (_linkStyleDisabledCount > 0);
  }

  /**
   * Called by link containers prior to rendering their children
   * in order to suppress the rendering of the default link
   * style class (.OraLink).  Most link containers (like tabBar,
   * globalHeader) provide their own style classes - the default
   * OraLink style class ends up getting in the way.
   *
   * Important: Each call to setDefaultLinkStyleClassDisabled(true)
   * must be followed by a matching call to setDefaultLinkStyleClassDisabled(false).
   */
  public void setDefaultLinkStyleDisabled(boolean isDisabled)
  {
    if (isDisabled)
      _linkStyleDisabledCount++;
    else
      _linkStyleDisabledCount--;
  }


  /**
   * @TODO is there a better way when link renderer moved?
   */
  public boolean isLinkDisabled()
  {
    return _isLinkDisabled;
  }

  /**
   * Called by link containers to force a link to render as disabled
   *
   * Important: Each call to setLinkDisabled(true)
   * must be followed by a matching call to setLinkDisabled(false).
   */
  public void setLinkDisabled(boolean isDisabled)
  {
    _isLinkDisabled = isDisabled;
  }



  // Implementation of RenderingContext

  @Override
  public Map<Object, Object> getProperties()
  {
    return _properties;
  }

  @Override
  public Agent getAgent()
  {
    return _agent;
  }

  /**
   * Typesafe accessor for the TrinidadAgent APIs.
   */
  public TrinidadAgent getTrinidadAgent()
  {
    return (TrinidadAgent) getAgent();
  }

  @Override
  public boolean isRightToLeft()
  {
    if (_localeContext != null)
    {
      return _localeContext.isRightToLeft();
    }

    return RequestContext.getCurrentInstance().isRightToLeft();
  }

  @Override
  public String getOutputMode()
  {
    return _outputMode;
  }


  @Override
  public RequestContext.Accessibility getAccessibilityMode()
  {
    return _accessibilityMode;
  }

  @Override
  public AccessibilityProfile getAccessibilityProfile()
  {
    return _accessibilityProfile;
  }

  @Override
  public boolean isAnimationEnabled()
  {
    return _animationEnabled;
  }

  /**
   * This can return null if there is no form data
   */
  @Override
  public FormData getFormData()
  {
    return _formData;
  }

  @Override
  public void setFormData(FormData formData)
  {
    _formData = formData;
  }

  @Override
  public void clearFormData()
  {
    _formData = null;
  }


  @Override
  public Skin getSkin()
  {
    // this might switch the skin from portlet to desktop depending upon the request map parameters.
    if(!_checkedRequestMapSkin)
    {
      Skin requestedSkin = getRequestMapSkin();
      _checkedRequestMapSkin = true;
      if (requestedSkin != null)
      {
        _skin = requestedSkin;
        // recompute the cached style provider with the new skin
        getStyleContext().getStyleProvider(true);
      }
    }
    return _skin;
  }

  /**
   * Get an interface that can be used for style lookups and generation.
   */
  public StyleContext getStyleContext()
  {
    if (_styleContext == null)
    {
      FacesContext fContext = FacesContext.getCurrentInstance();
      _styleContext = new StyleContextImpl(this, getTemporaryDirectory(fContext));
    }

    return _styleContext;
  }


  @Override
  public LocaleContext getLocaleContext()
  {
    // Initialize the locale context lazily, because we may
    // not have the view root with the correct locale when
    // the RenderingContext gets created
    if (_localeContext == null)
    {
      _initializeLocaleContext(FacesContext.getCurrentInstance(),
                               RequestContext.getCurrentInstance());
    }

    return _localeContext;
  }

  @Override
  public PartialPageContext getPartialPageContext()
  {
    return _pprContext;
  }

  @Override
  public String getStyleClass(String styleClass)
  {
    if (styleClass == null) return null;

    styleClass = getSkinResourceMappedKey(styleClass);
    // Most likely the _styleMap is a shortened style class map. In the case of portlets,
    // it is a full name-> portlet style class map.
    String shortenedStyle = null;
    if (_styleMap != null)
    {
      shortenedStyle = _styleMap.get(styleClass);
    }

    if (shortenedStyle != null)
    {
      if (EMPTY_STYLE_CLASS == shortenedStyle)
        return null;

      styleClass = shortenedStyle;
    }
    else
    {
      // if we didn't shorten the style classes, then make sure the
      // namespace character '|' is not in the name.
      // we do the same thing in CSSUtils when we write the full selector
      // to the CSS file.
      styleClass = StyleUtils.convertToValidSelector(styleClass);
    }
    return styleClass;
  }

  @Override
  public Icon getIcon(String iconName)
  {
    iconName = getSkinResourceMappedKey(iconName);
    if (iconName == null)
      return null;

    Skin skin = getSkin();

    // If we're in right-to-left, and the code asking us hasn't
    // already slapped on a right-to-left suffix, then go looking
    // in right-to-left land
    if (isRightToLeft() && !iconName.endsWith(StyleUtils.RTL_CSS_SUFFIX))
    {
      // append :rtl to the mappedIconName. If no icon with that name,
      // default to getting the icon with the original mappedIconName.
      String rtlIconName = iconName + StyleUtils.RTL_CSS_SUFFIX;
      Icon rtlIcon = skin.getIcon(rtlIconName);

      if ((rtlIcon == null) || rtlIcon.isNull())
      {
        // we want :rtl icons to default to regular icons, not a NullIcon,
        //  which is what the Skin does.
        rtlIcon = skin.getIcon(iconName);
        if (rtlIcon != null)
        {
          // cache regular icon so we don't need to get it again!
          skin.registerIcon(rtlIconName, rtlIcon);
        }
      }

      return rtlIcon;
    }
    else
    {
      return skin.getIcon(iconName);
    }
  }

  /**
   * Store a map that provides abbreviations of styles.
   */
  public void setStyleMap(Map<String, String> mapping)
  {
    _styleMap = mapping;
  }


  /**
   * Store a Map that maps a skin's resource keys from one key to another.
   */
  @Override
  public void setSkinResourceKeyMap(Map<String, String> mapping)
  {
    _skinResourceKeyMap = mapping;
  }


  /**
   * Get the _skinResourceKeyMap Map.
   */
  @Override
  public Map<String, String> getSkinResourceKeyMap()
  {
    return _skinResourceKeyMap;
  }

  protected String getSkinResourceMappedKey(String key)
  {
    Map<String, String> keyMap = getSkinResourceKeyMap();

    if (keyMap != null)
    {
      String mappedKey = keyMap.get(key);
      // if it isn't in the map, just use the key itself.
      if (mappedKey != null)
      {
        key = mappedKey;
      }
    }

    return key;
  }

  /**
   * Return the default skin family, which is "minimal" for the
   * core renderkit.
   */
  protected String getDefaultSkinFamily()
  {
    return "minimal";
  }


  /**
   * Returns the skin that is requested on the request map if the exact skin exists.
   * <p>
   * If we are in a portlet, then we might need to recalculate the skin.
   * The portal container might have its own skin that it wants us to use instead
   * of what we picked based on the skin-family and render-kit-id.
   * If it does, it will send the skin-id and the skin's styleSheetDocument id
   * in the request map.
   * </p>
   * <p>
   * If we have the skin with that id and the stylesheetdocument's id match,
   * then we return that skin; else we return null, indicating that there is no
   * requestMap skin.
   * </p>
   * @return null if there is no local skin that matches the requestMap skin, if any.
   *         skin that is requested to be used on the requestMap if we can find that
   *         exact skin with the same stylesheetdocument id locally.
   */
  public Skin getRequestMapSkin()
  {
    // protect against rechecking this more than once.
    // if we already checked for the _requestMapSkin and it's null,
    // then we'll return it anyway because that means we have no request map skin.
    if (_checkedRequestMapSkin)
      return _requestMapSkin;
    _checkedRequestMapSkin = true;

    if (CoreRenderKit.OUTPUT_MODE_PORTLET.equals(getOutputMode()))
    {
      FacesContext context = FacesContext.getCurrentInstance();
      Map<String, Object> requestMap = context.getExternalContext().getRequestMap();

      // Get the requested Skin Id from the request Map
      Object requestedSkinId = requestMap.get(_SKIN_ID_PARAM);
      if (requestedSkinId != null)
      {
        SkinFactory factory = SkinFactory.getFactory();
        if (factory == null)
        {
          _LOG.warning("NO_SKIN_FACTORY");
          return null;
        }

        Skin requestedSkin = factory.getSkin(context, requestedSkinId.toString());
        if (requestedSkin != null)
        {
          // Get the skin's stylesheet id from the request Map and then compare it
          // to the local skin's stylesheet id to make sure they match.
          Object requestMapStyleSheetId = requestMap.get(_SKIN_STYLESHEET_ID_PARAM);
          if (requestMapStyleSheetId != null)
          {
            // set up the styleProvider first, so that it will create the /adf/style
            // directory. Otherwise the following code would get an error when it
            // tries to getStyleDir. This could possibly be done better.
            getStyleContext().getStyleProvider();

            String skinForPortalStyleSheetId = requestedSkin.getStyleSheetDocumentId(this);
            if (skinForPortalStyleSheetId != null &&
                skinForPortalStyleSheetId.equals(requestMapStyleSheetId))
            {
              // it is ok to use this skin
              // Switch the _skin here to be the tmpRequestedSkin
              if (_LOG.isFine())
                _LOG.fine("The skin " +requestedSkinId+
                  " specified on the requestMap will be used.");
              _requestMapSkin = requestedSkin;
              // wrap in the RequestSkinWrapper, else we get property/icon
              // not found errors
              return new RequestSkinWrapper(requestedSkin);
            }
            else
            {
              if (_LOG.isWarning())
                _LOG.warning("REQUESTMAP_SKIN_NOT_USED_BECAUSE_STYLESHEETDOCUMENT_ID_NOT_MATCH_LOCAL_SKIN",requestedSkinId);
            }
          }
          else
          {
            if (_LOG.isSevere())
              _LOG.severe("REQUESTMAP_SKIN_NOT_USED_BECAUSE_STYLESHEETDOCUMENT_ID_NOT_IN_REQUESTMAP",requestedSkinId);
          }
        }// end requestedSkin != null
        else
        {
          if (_LOG.isWarning())
          {
            _LOG.warning("REQUESTMAP_SKIN_NOT_USED_BECAUSE_NOT_EXIST",requestedSkinId);
          }
        }
      }

    } // end outputMode == portlet
    return null;
  }

  /**
   * Set the local variable _skin to be the Skin from the
   * SkinFactory that best matches
   * the <skin-family> and current render-kit-id.
   * @param context    FacesContext
   * @param afContext  RequestContext
   */
  @SuppressWarnings("unchecked")
  private void _initializeSkin(
    FacesContext   context,
    RequestContext afContext)
  {
    // get skinFamily
    String skinFamily = afContext.getSkinFamily();
    if (skinFamily == null)
      skinFamily = getDefaultSkinFamily();

    // get renderKitId, default is desktop renderKit
    String renderKitId = XhtmlConstants.APACHE_TRINIDAD_DESKTOP;
    if (CoreRenderKit.OUTPUT_MODE_PORTLET.equals(getOutputMode()))
    {
      renderKitId = XhtmlConstants.APACHE_TRINIDAD_PORTLET;
    }
    else if (TrinidadAgent.TYPE_PDA == _agent.getAgentType())
    {
      // =-=jmw @todo when we have proper renderKitId switching, I can
      // get rid of this bit of code. Should we use getViewRoot().getRenderKitId() instead?
      renderKitId = XhtmlConstants.APACHE_TRINIDAD_PDA;
    }


    SkinFactory factory = SkinFactory.getFactory();
    if (factory == null)
    {
      _LOG.warning("NO_SKIN_FACTORY");
      return;
    }

    Skin skin = factory.getSkin(null, skinFamily, renderKitId);

    if (skin == null)
    {
      if (_LOG.isWarning())
        _LOG.warning("CANNOT_GET_SKIN_FROM_SKINFACTORY", skinFamily);
    }

    if (skin == null)
        skin = SkinNotAvailable.getSkinNotAvailable();

    _skin = skin;
  }



  private TrinidadAgent _initializeAgent(
    FacesContext context,
    Agent        base,
    String       outputMode)
  {
    // First, get an TrinidadAgent out of the plain Agent
    // =-=AEW In theory, we should only be getting a plain Agent
    // out of the RequestContext:  for some reason, we're going
    // straight to an TrinidadAgent in RequestContext
    TrinidadAgent agent;
    if (base instanceof TrinidadAgent)
      agent = (TrinidadAgent) base;
    else
      agent = new TrinidadAgentImpl(context, base);

    // Now, merge in any capabilities that we need
    if (CoreRenderKit.OUTPUT_MODE_PRINTABLE.equals(outputMode))
    {
      return AgentUtil.mergeCapabilities(agent, _PRINTABLE_CAPABILITIES);
    }
    else if (CoreRenderKit.OUTPUT_MODE_EMAIL.equals(outputMode))
    {
      return AgentUtil.mergeCapabilities(agent, _EMAIL_CAPABILITIES);
    }
    else if (CoreRenderKit.OUTPUT_MODE_PORTLET.equals(outputMode))
    {
      return AgentUtil.mergeCapabilities(agent, _PORTLET_CAPABILITIES);
    }
    else
    {
      return agent;
    }
  }

  //
  // Initialize PPR, if needed
  //
  private void _initializePPR(
    FacesContext    fContext,
    RequestContext context)
  {
    // Don't bother if PPR isn't even supported
    if (!CoreRendererUtils.supportsPartialRendering(this))
      return;

    PartialPageContext partialPageContext =
      PartialPageUtils.createPartialPageContext(fContext,
                                                context);

    _pprContext = partialPageContext;
  }

  /**
   * Get the directory for temporary files.
   * @todo: move into the util package?
   */
  @SuppressWarnings("unchecked")
  static public String getTemporaryDirectory(FacesContext fContext)
  {
    String path = null;

    Map<String, Object> applicationMap =
      fContext.getExternalContext().getApplicationMap();

    if (applicationMap != null)
    {
      // In general, write to the Servlet spec'd temporary directory
      // local to this webapp.
      // =-=AEW Note that if we're not running in a servlet container (that is,
      // we're a portlet), we have to write to the global temporary
      // directory.  That's not good - does the portlet spec define
      // anything?
      File tempdir = (File)
        applicationMap.get("javax.servlet.context.tempdir");
      if (tempdir == null)
      {
        // In design-time land, just write to the temporary directory.
        // But what
        if (Beans.isDesignTime() ||
            !(fContext.getExternalContext().getContext() instanceof ServletContext))
        {
          tempdir = new File(System.getProperty("java.io.tmpdir"));
          path = tempdir.getAbsolutePath();
        }
        else
        {
          _LOG.severe("The java.io.File handle (\"javax.servlet.context.tempdir\") is not set in the ServletContext");
        }
      }
      else
      {
        path = tempdir.getAbsolutePath();
      }
    }

    return path;
  }

  private void _initializeLocaleContext(
    FacesContext    fContext,
    RequestContext context)
  {
    Locale translations = fContext.getViewRoot().getLocale();
    Locale formatting = context.getFormattingLocale();
    if (formatting == null)
      formatting = translations;

    MutableLocaleContext localeContext = new MutableLocaleContext(formatting,
                                                                  translations);

    localeContext.setReadingDirection(context.isRightToLeft() ?
                                      LocaleUtils.DIRECTION_RIGHTTOLEFT :
                                      LocaleUtils.DIRECTION_LEFTTORIGHT);
    localeContext.setTimeZone(context.getTimeZone());

    MutableDecimalFormatContext mdfc =
      new MutableDecimalFormatContext(localeContext.getDecimalFormatContext());

    char grouping = context.getNumberGroupingSeparator();
    if (grouping != (char) 0)
      mdfc.setGroupingSeparator(grouping);

    char decimal = context.getDecimalSeparator();
    if (decimal != (char) 0)
      mdfc.setDecimalSeparator(decimal);

    localeContext.setDecimalFormatContext(mdfc);
    _localeContext = localeContext;
  }


  private Skin                _skin;
  private boolean             _checkedRequestMapSkin = false;
  private Skin                _requestMapSkin;
  private FormData            _formData;
  private TrinidadAgent       _agent;
  private Map<String, String> _styleMap;
  private Map<String, String> _skinResourceKeyMap;
  private String              _outputMode;
  private RequestContext.Accessibility _accessibilityMode;
  private AccessibilityProfile         _accessibilityProfile;
  private boolean _animationEnabled;
  private PartialPageContext  _pprContext;
  private LocaleContext       _localeContext;
  private StyleContext        _styleContext;
  private Map<Object, Object> _properties;
  private int                 _linkStyleDisabledCount = 0;
  private boolean             _isLinkDisabled = false;

  static private final String _SKIN_ID_PARAM =
    "org.apache.myfaces.trinidad.skin.id";
  static private final String _SKIN_STYLESHEET_ID_PARAM =
    "org.apache.myfaces.trinidad.skin.stylesheet.id";

  // Maps describing the capabilities of our output modes
  // -= Simon Lessard =-
  // FIXME: Cannot use CapabilityKey in the generic definition because
  //        CapabilityKey is not in the public API and those map are
  //        used as a parameter in an API call receiving a
  //        Map<Object, Object> argument
  static private final Map<Object, Object> _PRINTABLE_CAPABILITIES =
    new HashMap<Object, Object>();

  static private final Map<Object, Object> _EMAIL_CAPABILITIES =
    new HashMap<Object, Object>();

  static private final Map<Object, Object> _PORTLET_CAPABILITIES =
    new HashMap<Object, Object>();

  static
  {
    _PRINTABLE_CAPABILITIES.put(TrinidadAgent.CAP_INTRINSIC_EVENTS,
                                Boolean.FALSE);
    _PRINTABLE_CAPABILITIES.put(TrinidadAgent.CAP_SCRIPTING_SPEED,
                                TrinidadAgent.SCRIPTING_SPEED_CAP_NONE);
    _PRINTABLE_CAPABILITIES.put(TrinidadAgent.CAP_NAVIGATION,
                                Boolean.FALSE);
    _PRINTABLE_CAPABILITIES.put(TrinidadAgent.CAP_EDITING,
                                Boolean.FALSE);
    _PRINTABLE_CAPABILITIES.put(TrinidadAgent.CAP_PARTIAL_RENDERING,
                                Boolean.FALSE);


    _EMAIL_CAPABILITIES.put(TrinidadAgent.CAP_INTRINSIC_EVENTS,
                            Boolean.FALSE);
    _EMAIL_CAPABILITIES.put(TrinidadAgent.CAP_SCRIPTING_SPEED,
                            TrinidadAgent.SCRIPTING_SPEED_CAP_NONE);
    _EMAIL_CAPABILITIES.put(TrinidadAgent.CAP_EDITING,
                            Boolean.FALSE);
    _EMAIL_CAPABILITIES.put(TrinidadAgent.CAP_STYLE_ATTRIBUTES,
                            TrinidadAgent.STYLES_INTERNAL);
    _EMAIL_CAPABILITIES.put(TrinidadAgent.CAP_PARTIAL_RENDERING,
                            Boolean.FALSE);

    _PORTLET_CAPABILITIES.put(TrinidadAgent.CAP_PARTIAL_RENDERING,
                            Boolean.FALSE);
    _PORTLET_CAPABILITIES.put(TrinidadAgent.CAP_MULTIPLE_WINDOWS,
                            Boolean.FALSE);
  }

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(CoreRenderingContext.class);
}
