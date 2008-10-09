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

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.HtmlRenderer;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.skin.SkinStyleProvider;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleMap;
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
      _styleProvider = null;
    
    return getStyleProvider();
  }

  public StyleMap getStyleMap()
  {
    if (_styleMap == null)
      _styleMap = getStyleProvider().getStyleMap(this);
    return _styleMap;
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

    public StyleMap getStyleMap(StyleContext context)
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
  private StyleMap _styleMap;

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(StyleContextImpl.class);
}
