/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.trinidadinternal.renderkit;

import java.util.Map;
import java.util.MissingResourceException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.agent.AdfFacesAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.PartialPageContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.FormData;
import org.apache.myfaces.trinidadinternal.share.config.AccessibilityMode;
import org.apache.myfaces.trinidadinternal.share.nls.LocaleContext;
import org.apache.myfaces.trinidadinternal.skin.Skin;
import org.apache.myfaces.trinidadinternal.skin.icon.Icon;
import org.apache.myfaces.trinidadinternal.style.StyleContext;

/**
 * @todo REMOVE DEPENDENCY ON AcessibilityMode
 */
abstract public class RenderingContext
{
  /**
   * Retrieves the AdfRenderingContext active for the current thread.
   */
  static public RenderingContext getCurrentInstance()
  {
    return (RenderingContext) _CURRENT_CONTEXT.get();
  }

  static public final Object INACCESSIBLE_MODE =
   AccessibilityMode.INACCESSIBLE_MODE;
  static public final Object SCREEN_READER_MODE =
   AccessibilityMode.SCREEN_READER_MODE;

  public RenderingContext()
  {
    attach();
  }

  /**
   * A map of properties specific to rendering.
   */
  abstract public Map           getProperties();

  abstract public AdfFacesAgent getAgent();
  /**
   * @todo REMOVE LocaleContext
   */
  abstract public LocaleContext getLocaleContext();
  abstract public StyleContext getStyleContext();
  abstract public FormData getFormData();
  abstract public void setFormData(FormData data);
  abstract public void clearFormData();

  //
  // Skin methods.
  //

  /**
   * Get the Skin.  Icons, properties, etc. should never be retrieved directly
   * from the skin, but always through the AdfRenderingContext so they
   * can be properly transformed.
   */
  abstract public Skin getSkin();

  public String getTranslatedString(String key)
  {
    if (key == null)
      return null;

    try
    {
      return getSkin().getTranslatedString(getLocaleContext(), key);
    }
    catch (MissingResourceException mre)
    {
      // Instead of halting execution, return "???<key>???",
      // just like JSF and JSTL will do, and log a severe error
      _LOG.severe("Could not get resource key {0} from skin {1}",
                  new String[]{key, getSkin().getId()});
      return "???" + key + "???";
    }
  }

  abstract public Icon getIcon(String iconName);


  abstract public String getStyleClass(String styleClass);
  abstract public void   setSkinResourceKeyMap(Map mapping);
  abstract public Map    getSkinResourceKeyMap();
  abstract public boolean isRightToLeft();
  abstract public String getOutputMode();
  abstract public Object getAccessibilityMode();

  /**
   * @TODO This is a hack API to enable caching of the client ID.
   *  All fine, but we should have a more general mechanism.
   */
  public String getCurrentClientId() { return _currentClientId; }
  public void setCurrentClientId(String currentClientId)
  {
    _currentClientId = currentClientId;
  }

  private String _currentClientId;



  abstract public PartialPageContext getPartialPageContext();


  public void release()
  {
    Object o = _CURRENT_CONTEXT.get();
    // Clean up first...
    _CURRENT_CONTEXT.set(null);

    // Then see if there's a problem, and scream if there is one
    if (o == null)
      throw new IllegalStateException("AdfRenderingContext was already " +
                                      "released or had never been attached.");
    if (o != this)
      throw new IllegalStateException("Trying to release a different " +
                     "AdfRenderingContext than the current context.");
  }

  /**
   * Attaches an AdfRenderingContext to the current thread.  This method is
   * protected, and therefore can only be called by an AdfRenderingContext
   * object itself.
   */
  protected void attach()
  {
    Object o = _CURRENT_CONTEXT.get();
    // We want to catch two different problems:
    // (1) A failure to call release()
    // (2) An attempt to attach an instance when the thread already has one
    // For #1, anything more than a warning is dangerous, because throwing
    // an exception would permanently make the thread unusable.
    // For #2, I'd like to throw an exception, but I can't distinguish
    // this scenario from #1.
    if (o != null)
    {
      _LOG.warning("Trying to attach AdfRenderingContext " +
                   "to a thread that already had one.");
    }

    _CURRENT_CONTEXT.set(this);
  }


  static private final ThreadLocal _CURRENT_CONTEXT = new ThreadLocal();
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(RenderingContext.class);
}
