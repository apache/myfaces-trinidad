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
package org.apache.myfaces.trinidadinternal.renderkit.core;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.HtmlRenderer;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidadinternal.skin.SkinStyleProvider;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.StyleMap;

class StyleContextImpl implements StyleContext
{
  public StyleContextImpl(
    RenderingContext arc,
    String generatedFilesPath)
  {
    _arc = arc;
    _generatedFilesPath = generatedFilesPath;
    _styleProvider = _getDefaultStyleProvider(arc.getSkin());
    _styleMap = _styleProvider.getStyleMap(this);
  }


  public StyleProvider getStyleProvider()
  {
    return _styleProvider;
  }

  public StyleMap getStyleMap()
  {
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
    // =-=AEW Expose a configuration option if this
    // is a performance issue
    return true;
  }

  public boolean disableStandardsMode()
  {
    FacesContext fContext = FacesContext.getCurrentInstance();
    return HtmlRenderer.isStandardsModeDisabled(fContext);
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
      _LOG.severe("Could not get stylesheet cache", e);
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

    public String getStyleSheetURI(StyleContext context)
    {
      return null;
    }

    public StyleMap getStyleMap(StyleContext context)
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
