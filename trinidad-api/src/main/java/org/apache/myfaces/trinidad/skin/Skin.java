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
package org.apache.myfaces.trinidad.skin;

import java.util.MissingResourceException;

import org.apache.myfaces.trinidad.context.LocaleContext;

/**
 * Defines the components (icons, styles, etc)
 * which are used to implement a particular skin.
 * @todo. look through UIExtension comments.
 *
 * @see SkinFactory
 * @see org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext#getSkinFactory
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/Skin.java#0 $) $Date: 10-nov-2005.18:58:54 $
 * @author The Oracle ADF Faces Team
 */
abstract public class Skin
{
  /**
   * Returns an string identifier which uniquely identies
   * this Skin implementation.  Skin implementations
   * can be retrieved by id via SkinFactory.getSkin().
   * @see org.apache.myfaces.trinidadinternal.skin.SkinFactory#getSkin
   */
  abstract public String getId();

  /**
   * Returns the name of the skin "family" for this skin.
   * The family name is used when specifying a preferred skin
   * in trinidad-config.xml.
   * This provides a way to refer to a group of
   * related skin implementations while allowing the
   * particular skin instance to be selected based on the
   * current render-kit-id.
   */
  abstract public String getFamily();

  /**
   * Returns the renderKitId for the Skin.
   */
  abstract public String getRenderKitId();

  /**
   * Returns the name of the XSS style sheet for this Skin.
   */
  abstract public String getStyleSheetName();

  /**
   * Returns a translated String in the LocaleContext's translation Locale.
   */
  abstract public String getTranslatedString(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException;

  /**
   * Returns a translated value in the LocaleContext's translation Locale.
   * This value may or may not be a String, and developers should avoid
   * calling toString() unless absolutely necessary.
   * @param lContext The LocaleContext which provides the translation Locale.
   *                 Cannot be null.
   * @param key The key of the translation to retrieve. Cannot be null.
   * @throws NullPointerException if lContext or key is null.
   */
  abstract public Object getTranslatedValue(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException;

  /**
   * Our renderers call this to get the icon. This returns a renderable
   * icon. (ReferenceIcons are resolved -- the real icon they point to is
   * returned)
   */
  abstract public Icon getIcon(String  iconName);

  /**
   * Returns an Icon object; can be a ReferenceIcon.
   * @param iconName  The name of the icon to retrieve. Cannot be null
   * @throws NullPointerException if iconName is null.
   */
  abstract public Icon getIcon(
    String  iconName,
    boolean resolveIcon);

  /**
   * Retrieves a property that was set via a call to setProperty().
   * Some Renderer implementations may store properties on the
   * Skin instance to avoid having to re-compute Skin-specific
   * values on each render.
   */
  abstract public Object getProperty(Object key);

  /**
   * Sets a value for the specified property key.
   * Some Renderer implementations may store properties on the
   * Skin instance to avoid having to re-compute Skin-specific
   * values on each render.
   */
  abstract public void setProperty(
    Object key,
    Object value);

  /**
   * Registers an Icon for the specified icon name.
   * @param iconName  The name of the icon. Cannot be null.
   * @param icon      The Icon to register.
   * @throws NullPointerException if iconName is null.
   */
  abstract public void registerIcon(
    String  iconName,
    Icon    icon);
    
  /**
   * Registers a style sheet which defines extension-specific
   * styles.  The styles specified by this style sheet will be
   * merged with the Skin's own styles.
   * @param styleSheetName The name of the style sheet which
   *          defines the extension's styles.
   * @throws NullPointerException if styleSheetName is null.
   */
  abstract public void registerStyleSheet(
    String styleSheetName
    );
}
