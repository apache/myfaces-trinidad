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

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.style.Styles;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.HtmlRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.StyleSheetRenderer;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.skin.SkinStyleProvider;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;


class StyleContextImpl implements StyleContext
{
  public StyleContextImpl(
    RenderingContext arc,
    String generatedFilesPath)
  {
    _arc = arc;
    _generatedFilesPath = generatedFilesPath;

    // Our style/skin code assumes that we have access to a
    // non-null accessibility profile.  Check that here.
    assert(_arc.getAccessibilityProfile() != null);
  }

  public StyleProvider getStyleProvider()
  {
    if (_styleProvider == null)
    {
      Skin skin = ((CoreRenderingContext) _arc).getSkin();
      _styleProvider = _getDefaultStyleProvider(skin);
    }
    return _styleProvider;
  }

  /* added this in case we switch the skin after the styleProvider was cached above. */
  /* we want to recompute, not get it from the cache. */
  public StyleProvider getStyleProvider(boolean recompute)
  {
    if (recompute)
    {
      _styleProvider = null;
      _isDisableStyleCompression = null;
      // recalculate in case the skin switched in the portlet case
      isDisableStyleCompression();
    }

    return getStyleProvider();
  }

  /**
   *
   */
  public Styles getStyles()
  {
    if (_styles == null)
      _styles = getStyleProvider().getStyles(this);
    return _styles;
  }

  /**
   * Returns the end user's locale.
   */
  public LocaleContext getLocaleContext()
  {
    return _arc.getLocaleContext();
  }

  public String getGeneratedFilesPath()
  {
    return _generatedFilesPath;
  }

  /**
   * Returns the end user's Agent.
   */
  public TrinidadAgent getAgent()
  {
    return ((CoreRenderingContext) _arc).getTrinidadAgent();
  }
  public boolean checkStylesModified()
  {
    FacesContext context = FacesContext.getCurrentInstance();
    String checkTimestamp =
      context.getExternalContext().getInitParameter(Configuration.CHECK_TIMESTAMP_PARAM);
    return "true".equals(checkTimestamp);
  }
  /*
   * checks to see if the Skin is dirty by calling skin.isDirty()
   */
  public boolean isDirty()
  {
    if (Beans.isDesignTime())
    {
      // In Design Time mode, if we have a skin-id on the request scope,
      // then this means the Design Time wants the skin to regenerate. To do this,
      // we say the skin is dirtys
      FacesContext context = FacesContext.getCurrentInstance();
      Object requestSkinId = 
        ((CoreRenderingContext) _arc).getRequestMapSkinId(context);
      if (requestSkinId != null)
      {
        // set the skin to dirty as well for double insurance, like if someone checks the 
        // skin directly to see if it is dirty.
        _arc.getSkin().setDirty(true); 
        return true;
      }
    }
    return _arc.getSkin().isDirty();
  }

  public boolean disableStandardsMode()
  {
    FacesContext fContext = FacesContext.getCurrentInstance();
    return HtmlRenderer.isStandardsModeDisabled(fContext);
  }

  public AccessibilityProfile getAccessibilityProfile()
  {
    return _arc.getAccessibilityProfile();
  }

  // Creates a default StyleProvider
  private StyleProvider _getDefaultStyleProvider(Skin skin)
  {
    String cachePath =  _generatedFilesPath + "/adf/styles/cache/";

    try
    {
      return SkinStyleProvider.getSkinStyleProvider(skin, cachePath);
    }
    catch (RuntimeException e)
    {
      _LOG.severe("CANNOT_GET_STYLESHEET_CACHE", e);
    }

    // Return a non-null StyleProvider instance
    return NullStyleProvider.getInstance();
  }

  public boolean isPortletMode()
  {
    return CoreRenderKit.OUTPUT_MODE_PORTLET.equals(_arc.getOutputMode());
  }
  
  @Override
  public boolean isRequestSecure()
  {
    if (_isRequestSecure == null) 
    {
      String scheme = _getRequestScheme();
      _isRequestSecure =  "https".equals(scheme);
    }
    return _isRequestSecure;
  }

  static private String _getRequestScheme()
  {
    FacesContext context = FacesContext.getCurrentInstance();
    ExternalContext external = context.getExternalContext();
    return ExternalContextUtils.getRequestScheme(external);
  }

  /**
   *
   * @return true if we should disable style compression. e.g.,
   * if StyleSheetRenderer.DISABLE_CONTENT_COMPRESSION is true or the skin is a portlet skin
   * or we are in portlet mode and not doing skin sharing.
   */
  public boolean isDisableStyleCompression()
  {
    if (_isDisableStyleCompression == null)
    {
      FacesContext context = FacesContext.getCurrentInstance();
      String disableContentCompression =
        context.getExternalContext().
        getInitParameter(StyleSheetRenderer.DISABLE_CONTENT_COMPRESSION);
      boolean disableContentCompressionBoolean = "true".equals(disableContentCompression);

      // the user wants to explicitly disable the content compression and show the full styleclass
      // names
      if (disableContentCompressionBoolean)
        _isDisableStyleCompression = Boolean.TRUE;

      // we still need to check if we don't want to compress even if the disable content
      // compression flag is true
      if (CoreRenderKit.OUTPUT_MODE_PORTLET.equals(_arc.getOutputMode()))
      {
        Skin skin = ((CoreRenderingContext) _arc).getSkin();
        boolean isPortletSkin =
        CoreRenderKit.OUTPUT_MODE_PORTLET.equals(skin.getRenderKitId());

        if (isPortletSkin)
          _isDisableStyleCompression = Boolean.TRUE;
        else
        {
          // we must be skin sharing. Check if the stylesheetids of the producer and consumer skin
          // match.
          // if so then we do whatever the disableContentCompression says to do.
          // if not, we must not compress so that we don't have conflicts with the producer
          // stylesheet which does compress.

          if (!(((CoreRenderingContext) _arc).
            isRequestMapStyleSheetIdAndSkinEqual(context, skin)))
          {
            _isDisableStyleCompression = Boolean.TRUE;
          }
        }
      }
    }
    // if _isDisableStyleCompression is still null,
    // default it to false since disabling styling compression defaults to false

    if (_isDisableStyleCompression == null)
      _isDisableStyleCompression = Boolean.FALSE;

    return Boolean.TRUE.equals(_isDisableStyleCompression);

  }

  // Implementation of StyleProvider which does nothing - used as a
  // placeholder when we can't get the real StyleProvider
  static private class NullStyleProvider implements StyleProvider
  {
    private NullStyleProvider() {}

    static public StyleProvider getInstance()
    {
      if (_sInstance == null)
        _sInstance = new NullStyleProvider();

      return _sInstance;
    }

    public String getContentStyleType(StyleContext context)
    {
      return null;
    }

    public Map<String, String> getShortStyleClasses(StyleContext context)
    {
      return null;
    }

    public List<String> getStyleSheetURIs(StyleContext context)
    {
      return null;
    }

    public Styles getStyles(StyleContext context)
    {
      return null;
    }

    public ConcurrentMap<String, Icon> getIcons(StyleContext context)
    {
      return null;
    }

    public ConcurrentMap<Object, Object> getSkinProperties(StyleContext context)
    {
      return null;
    }

    private static StyleProvider _sInstance;
  }


  private RenderingContext _arc;
  private String  _generatedFilesPath;
  private StyleProvider _styleProvider;
  private Styles _styles;
  private Boolean  _isDisableStyleCompression;
  private Boolean _isRequestSecure;

  static private final String _SKIN_ID_PARAM =
    "org.apache.myfaces.trinidad.skin.id";

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(StyleContextImpl.class);
}
