/*
 * Copyright  2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.core;

import java.beans.Beans;

import java.io.File;

import java.util.HashMap;
import java.util.Map;

import javax.faces.context.FacesContext;

import javax.servlet.ServletContext;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adf.context.Agent;
import org.apache.myfaces.adf.context.AdfFacesContext;

import org.apache.myfaces.adfinternal.agent.AdfFacesAgent;
import org.apache.myfaces.adfinternal.agent.AdfFacesAgentImpl;
import org.apache.myfaces.adfinternal.agent.AgentUtil;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.FormData;

import org.apache.myfaces.adfinternal.share.config.AccessibilityMode;

import org.apache.myfaces.adfinternal.share.nls.LocaleContext;
import org.apache.myfaces.adfinternal.skin.Skin;
import org.apache.myfaces.adfinternal.skin.SkinFactory;
import org.apache.myfaces.adfinternal.skin.SkinNotAvailable;
import org.apache.myfaces.adfinternal.skin.icon.Icon;

import org.apache.myfaces.adfinternal.style.StyleContext;
import org.apache.myfaces.adfinternal.style.util.StyleUtils;

import org.apache.myfaces.adfinternal.renderkit.core.ppr.PartialPageContext;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.PartialPageUtils;
import org.apache.myfaces.adfinternal.share.nls.MutableDecimalFormatContext;
import org.apache.myfaces.adfinternal.share.nls.MutableLocaleContext;
import org.apache.myfaces.adfinternal.util.nls.LocaleUtils;

public class CoreAdfRenderingContext extends AdfRenderingContext
{
  public CoreAdfRenderingContext()
  {
    FacesContext context = FacesContext.getCurrentInstance();
    AdfFacesContext afContext = AdfFacesContext.getCurrentInstance();

    _outputMode = afContext.getOutputMode();
    _agent = _initializeAgent(context, afContext.getAgent(), _outputMode);

    _initializeSkin(afContext);
    _initializePPR(context, afContext);
    _accessibilityMode = AccessibilityMode.getAccessibilityMode(
                              afContext.getAccessibilityMode());
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



  // Implementation of AdfRenderingContext

  public Map getProperties()
  {
    return _properties;
  }

  public AdfFacesAgent getAgent()
  {
    return _agent;
  }

  public boolean isRightToLeft()
  {
    if (_localeContext != null)
    {
      return _localeContext.isRightToLeft();
    }

    return AdfFacesContext.getCurrentInstance().isRightToLeft();
  }

  public String getOutputMode()
  {
    return _outputMode;
  }


  public Object getAccessibilityMode()
  {
    return _accessibilityMode;
  }

  /**
   * This will create a FormData object if it's null.
   */
  public FormData getFormData()
  {
    assert(_formData != null);
    return _formData;
  }

  public void setFormData(FormData formData)
  {
    _formData = formData;
  }

  public void clearFormData()
  {
    _formData = null;
  }


  public Skin getSkin()
  {
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
      _styleContext = new StyleContextImpl(this,
                                           getSkin(),
                                           getTemporaryDirectory(fContext));
    }

    return _styleContext;
  }


  public LocaleContext getLocaleContext()
  {
    // Initialize the locale context lazily, because we may
    // not have the view root with the correct locale when
    // the AdfRenderingContext gets created
    if (_localeContext == null)
    {
      _initializeLocaleContext(FacesContext.getCurrentInstance(),
                               AdfFacesContext.getCurrentInstance());
    }

    return _localeContext;
  }

  public PartialPageContext getPartialPageContext()
  {
    return _pprContext;
  }

  public String getStyleClass(String styleClass)
  {
    if (styleClass == null) return null;

    styleClass = getSkinResourceMappedKey(styleClass);
    String shortenedStyle = null;
    if (_styleMap != null)
    {
      shortenedStyle = (String) _styleMap.get(styleClass);
    }

    if (shortenedStyle != null)
      styleClass = shortenedStyle;
    else
    {
      // if we didn't shorten the style classes, then make sure the
      // namespace character '|' is not in the name.
      // we do the same thing in CSSUtils when we write the full selector
      // to the CSS file.
      styleClass = StyleUtils.convertToValidSelector(styleClass.toString());
    }
    return styleClass;
  }


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
  public void setStyleMap(Map mapping)
  {
    _styleMap = mapping;
  }


  /**
   * Store a Map that maps a skin's resource keys from one key to another.
   */
  public void setSkinResourceKeyMap(Map mapping)
  {
    _skinResourceKeyMap = mapping;
  }


  /**
   * Get the _skinResourceKeyMap Map.
   */
  public Map getSkinResourceKeyMap()
  {
    return _skinResourceKeyMap;
  }

  protected String getSkinResourceMappedKey(String key)
  {
    Map keyMap = getSkinResourceKeyMap();

    if (keyMap != null)
    {
      String mappedKey = (String) keyMap.get(key);
      // if it isn't in the map, just use the key itself.
      if (mappedKey != null)
      {
        key = mappedKey;
      }
    }

    return key;
  }

  /**
   * Set the local variable _skin to be the Skin from the
   * SkinFactory that best matches
   * the <skin-family> and current render-kit-id.
   * @param fContext FacesContext
   * @param context  AdfFacesContext
   */
  private void _initializeSkin(AdfFacesContext afContext)
  {
    String skinFamily = afContext.getSkinFamily();
    String renderKitId = "org.apache.myfaces.adf.desktop";

    // =-=jmw @todo when we have proper renderKitId switching, I can
    // get rid of this bit of code.
    if (AdfFacesAgent.TYPE_PDA == _agent.getAgentType())
    {
      renderKitId = "org.apache.myfaces.adf.pda";
    }


    SkinFactory factory = SkinFactory.getFactory();
    if (factory == null)
    {
      if (_LOG.isWarning())
        _LOG.warning("There is no SkinFactory");
      return;
    }

    Skin skin = factory.getSkin(null, skinFamily, renderKitId);

    if (skin == null)
    {
      if (_LOG.isWarning())
        _LOG.warning("Could not get skin " + skinFamily +
                     " from the SkinFactory");
    }

    if (skin == null)
        skin = SkinNotAvailable.getSkinNotAvailable();

    _skin = skin;
  }

  private AdfFacesAgent _initializeAgent(
    FacesContext context,
    Agent        base,
    String       outputMode)
  {
    // First, get an AdfFacesAgent out of the plain Agent
    // =-=AEW In theory, we should only be getting a plain Agent
    // out of the AdfFacesContext:  for some reason, we're going
    // straight to an AdfFacesAgent in AdfFacesContext
    AdfFacesAgent agent;
    if (base instanceof AdfFacesAgent)
      agent = (AdfFacesAgent) base;
    else
      agent = new AdfFacesAgentImpl(context, base);

    // Now, merge in any capabilities that we need
    if (CoreRenderKit.OUTPUT_MODE_PRINTABLE.equals(outputMode))
    {
      return AgentUtil.mergeCapabilities(agent, _PRINTABLE_CAPABILITIES);
    }
    else if (CoreRenderKit.OUTPUT_MODE_EMAIL.equals(outputMode))
    {
      return AgentUtil.mergeCapabilities(agent, _EMAIL_CAPABILITIES);
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
    AdfFacesContext context)
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
  static public String getTemporaryDirectory(FacesContext fContext)
  {
    String path = null;

    Map applicationMap = fContext.getExternalContext().getApplicationMap();
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
    AdfFacesContext context)
  {
    MutableLocaleContext localeContext = new MutableLocaleContext(
                                   fContext.getViewRoot().getLocale());

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


  private Skin               _skin;
  private FormData           _formData;
  private AdfFacesAgent      _agent;
  private Map                _styleMap;
  private Map                _skinResourceKeyMap;
  private String             _outputMode;
  private Object             _accessibilityMode;
  private PartialPageContext _pprContext;
  private LocaleContext      _localeContext;
  private StyleContext       _styleContext;
  private Map                _properties = new HashMap();
  private int                _linkStyleDisabledCount = 0;
  private boolean            _isLinkDisabled = false;

  // Maps describing the capabilities of our output modes
  static private final Map _PRINTABLE_CAPABILITIES = new HashMap();
  static private final Map _EMAIL_CAPABILITIES = new HashMap();
  static
  {
    _PRINTABLE_CAPABILITIES.put(AdfFacesAgent.CAP_INTRINSIC_EVENTS,
                                Boolean.FALSE);
    _PRINTABLE_CAPABILITIES.put(AdfFacesAgent.CAP_SCRIPTING_SPEED,
                                AdfFacesAgent.SCRIPTING_SPEED_CAP_NONE);
    _PRINTABLE_CAPABILITIES.put(AdfFacesAgent.CAP_SCRIPTING_SPEED,
                                AdfFacesAgent.SCRIPTING_SPEED_CAP_NONE);
    _PRINTABLE_CAPABILITIES.put(AdfFacesAgent.CAP_NAVIGATION,
                                Boolean.FALSE);
    _PRINTABLE_CAPABILITIES.put(AdfFacesAgent.CAP_EDITING,
                                Boolean.FALSE);
    _PRINTABLE_CAPABILITIES.put(AdfFacesAgent.CAP_PARTIAL_RENDERING,
                                Boolean.FALSE);


    _EMAIL_CAPABILITIES.put(AdfFacesAgent.CAP_INTRINSIC_EVENTS,
                            Boolean.FALSE);
    _EMAIL_CAPABILITIES.put(AdfFacesAgent.CAP_SCRIPTING_SPEED,
                            AdfFacesAgent.SCRIPTING_SPEED_CAP_NONE);
    _EMAIL_CAPABILITIES.put(AdfFacesAgent.CAP_EDITING,
                            Boolean.FALSE);
    _EMAIL_CAPABILITIES.put(AdfFacesAgent.CAP_STYLE_ATTRIBUTES,
                            AdfFacesAgent.STYLES_INTERNAL);
    _EMAIL_CAPABILITIES.put(AdfFacesAgent.CAP_PARTIAL_RENDERING,
                            Boolean.FALSE);
  }

  static private final ADFLogger _LOG =
    ADFLogger.createADFLogger(CoreAdfRenderingContext.class);
}
