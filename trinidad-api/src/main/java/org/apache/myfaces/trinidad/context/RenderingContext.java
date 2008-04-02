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
package org.apache.myfaces.trinidad.context;

import java.util.Map;
import java.util.MissingResourceException;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.Icon;

/**
 */
abstract public class RenderingContext
{
  /**
   * Retrieves the RenderingContext active for the current thread.
   */
  static public RenderingContext getCurrentInstance()
  {
    return _CURRENT_CONTEXT.get();
  }

  public RenderingContext()
  {
    attach();
  }

  /**
   * A map of properties specific to rendering.
   */
  abstract public Map<Object, Object> getProperties();

  abstract public Agent getAgent();
  abstract public LocaleContext getLocaleContext();
  abstract public FormData getFormData();
  abstract public void setFormData(FormData data);
  abstract public void clearFormData();

  //
  // Skin methods.
  //

  /**
   * Get the Skin.  Icons, properties, etc. should never be retrieved directly
   * from the skin, but always through the RenderingContext so they
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
      _LOG.severe("CANNOT_GET_RESOURCE_KEY", new String[]{key, getSkin().getId()});
      return "???" + key + "???";
    }
  }

  abstract public Icon getIcon(String iconName);


  abstract public String getStyleClass(String styleClass);
  abstract public void   setSkinResourceKeyMap(Map<String, String> mapping);
  abstract public Map<String, String> getSkinResourceKeyMap();
  abstract public boolean isRightToLeft();
  abstract public String getOutputMode();
  abstract public RequestContext.Accessibility getAccessibilityMode();
  abstract public AccessibilityProfile getAccessibilityProfile();
  abstract public boolean isAnimationEnabled();

  // TODO This is a hack API to enable caching of the client ID.
  // All fine, but we should have a more general mechanism.
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
    _CURRENT_CONTEXT.remove();

    // Then see if there's a problem, and scream if there is one
    if (o == null)
      throw new IllegalStateException(_LOG.getMessage(
        "RENDERINGCONTEXT_ALREADY_RELEASED_OR_NEVER_ATTACHED"));
    if (o != this)
      throw new IllegalStateException(_LOG.getMessage(
        "TRY_RELEASING_DIFFERENT_RENDERINGCONTEXT"));
  }

  /**
   * Attaches an RenderingContext to the current thread.  This method is
   * protected, and therefore can only be called by an RenderingContext
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
      _LOG.warning("TRYING_ATTACH_RENDERERINGCONTEXT");
    }

    _CURRENT_CONTEXT.set(this);
  }


  static private final ThreadLocal<RenderingContext> _CURRENT_CONTEXT = 
    new ThreadLocal<RenderingContext>();
  
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(RenderingContext.class);
}
