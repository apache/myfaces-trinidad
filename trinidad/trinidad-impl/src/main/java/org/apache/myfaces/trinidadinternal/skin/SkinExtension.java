/*
 * Copyright  2003-2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidadinternal.skin;

import java.io.IOException;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Stack;

import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.Icon;

import org.apache.myfaces.trinidadinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.trinidadinternal.util.OptimisticHashMap;

import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.xml.StyleSheetDocumentUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;




/**
 * A Skin which extends another Skin, possibly adding
 * customizations.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/SkinExtension.java#0 $) $Date: 10-nov-2005.18:58:55 $
 * @author The Oracle ADF Faces Team
 */
public class SkinExtension extends SkinImpl
{
  /**
   * Creates a Skin which extends the specified base
   * Skin.
   *
   * @param baseSkin The base Skin that this custom
   *        Skin "extends". If it is a Skin designed for "org.apache.myfaces.trinidad.desktop"
   *        render-kit-id, then its base skin should be SimpleDesktopSkin.
   *        If it is a Skin designed for "org.apache.myfaces.trinidad.pda" render-kit-id,
   *        then its base skin should be SimplePdaSkin. Currently, we
   *        do not allow one SkinExtension to extend another, but we may
   *        someday.
   *        Must be non-null.
   * @param id A string which can be used to uniquely identify the
   *           Skin .
   *           Must be non-null.
   * @param family The Skin family name that this
   *               SkinExtension belongs to. For example, you might have
   *               a Skin that makes your pages look purple for the
   *               desktop renderkit and a Skin that makes your pages
   *               look purple for the pda renderkit.
   *               You can set the skin-family to "purple" in
   *               trinidad-config.xml, and the Skin with skin-family
   *               and render-kit-id match will be chosen.
   *               Must be non-null.
   * @param renderKitId The render-kit-id that this Skin is designed for.
   * @throws NullPointerException if baseSkin, id, or family is null.
   *
   */
  public SkinExtension(
    Skin baseSkin,
    String id,
    String family,
    String renderKitId
    )
  {
    if (baseSkin == null)
      throw new NullPointerException("Null baseSkin");
    if (id == null)
      throw new NullPointerException("Null id");
    if (family == null)
      throw new NullPointerException("Null family");
    if (renderKitId == null)
      renderKitId = "org.apache.myfaces.trinidad.desktop";

    _baseSkin = (SkinImpl) baseSkin;
    _id = id;
    _family = family;
    _renderKitId = renderKitId;
  }



  /**
   * Returns the base Skin which this custom Skin
   * "extends".
   */
  public Skin getBaseSkin()
  {
    return _baseSkin;
  }

  /**
   * Returns the id of this custom Skin.
   */
  @Override
  public String getId()
  {
    return _id;
  }

  /**
   * Returns the name of the Skin family that this
   * SkinExtension belongs to.
   */
  @Override
  public String getFamily()
  {
    return _family;
  }

  /**
   * Returns the name of the XSS style sheet for this Skin if
   * on has been set
   * @see setStyleSheetName
   */
  @Override
  public String getStyleSheetName()
  {
    return _styleSheetName;
  }

  /**
   * Returns the name of the render-kit-id for this Skin.
   */
  @Override
  public String getRenderKitId()
  {
    return _renderKitId;
  }


  /**
   * Returns the name of the bundle for the extension.
   */
  @Override
  public String getBundleName()
  {
    if (_bundleName != null)
      return _bundleName;
    return _baseSkin.getBundleName();
  }

  /**
   * Returns the name of the bundle for the extension.
   */
  public void setBundleName(String bundleName)
  {
    _bundleName = bundleName;
  }

  /**
   * Override of Skin.registerIcon().
   */
  @Override
  public void registerIcon(
    String  iconName,
    Icon    icon
    )
  {
    // If a null icon is being registered, use a NullIcon
    // as a placeholder so that we don't look for a non-null
    // Icon in the base LAF.
    if (icon == null)
      icon = _NULL_ICON;

    super.registerIcon(iconName, icon);
  }

  /**
   * Override of Skin.getTranslatedValue() which
   * supports pulling translations from component providers
   * as well as the base Skin.
   */
  @Override
  public Object getTranslatedValue(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException
  {
    // Short-circuit when there is no customized translation
    if (_bundleName == null)
    {
      return getBaseSkin().getTranslatedValue(lContext, key);
    }

    // First, check the local translations cache
    Object value = _getCachedTranslatedValue(lContext, key);
    if (value != null)
    {
      // testTranslationKey(key);//jmw test for testing translation keys
      if (value == _NULL_TRANSLATION)
        return null;

      return value;
    }

    // Next, check to see if we can get a translation from
    // a bundle that has been explicitly registered on this
    // SkinExtension instance.  (We can just use
    // Skin.getTranslatedValue() for this.)
    // Note: In order to avoid MissingResourceExceptions, we
    // first check to see if the translation key is available
    // before looking up the message in the ResourceBundle.
    if (_isTranslationKeyAvailable(lContext, key))
    {
      try
      {
        value = super.getTranslatedValue(lContext, key);
      }
      catch (MissingResourceException e)
      {
        // It is possible that the call to getBundle() might
        // fail with a MissingResourceException if the customer
        // has only provided a custom bundle for certain languages.
        // This is okay, so we just eat these exceptions.
        ;
      }
    }

    // If we didn't find a value in the local bundle, try getting
    // the translation from the base Skin.
    if (value == null)
    {
      Skin baseSkin = getBaseSkin();
      value = baseSkin.getTranslatedValue(lContext, key);
    }

    // If we found an translation, store it in the cache so that
    // we don't have to search for it again next time.
    _putCachedTranslatedValue(lContext, key, value);

    return value;
  }

  /**
   * Try to pull a locally set property, if null
   * pull a property from the base skin.
   * This means you cannot set a local property to null and expect it to
   * "null out" a property on the base skin.
   */
  @Override
  public Object getProperty(Object key)
  {
    Object value = super.getProperty(key);

    if ( value == null)
    {
      Skin baseSkin = getBaseSkin();
      value =  baseSkin.getProperty(key);
    }

    return value;

  }

  /**
   * Override of Skin.getIcon() to look in the base skin for the icon
   * if it isn't registered yet with this skin.
   */
  @Override
  public Icon getIcon(
    String  iconName,
    boolean resolve
    )
  {
    // First check to see if we already have an Icon registered.
    // it might be a ReferenceIcon. If so, resolve the reference and
    // register the true icon (if resolve is true)

    Icon icon = super.getIcon(iconName, false);

    if (icon != null)
    {
      if (icon == _NULL_ICON)
        return null;

      if (resolve)
      {
        // if icon is a ReferenceIcon, I need to get the actual icon, and
        // re-register it.
        if (icon instanceof ReferenceIcon)
        {
          // go find the actual icon
          Icon resolvedIcon = _resolveReferenceIcon((ReferenceIcon)icon,
                                                     null);

          // register icon so we don't have to resolve reference next time.
          registerIcon(iconName, resolvedIcon);

          return resolvedIcon;
        }
      }
      return icon;
    }

    // If we don't have the icon locally, check the base skin.

    Skin baseSkin = getBaseSkin();
    // get baseSkin's icon. Don't resolve to a real icon. If it is a
    // ReferenceIcon, return the ReferenceIcon.
    icon = baseSkin.getIcon(iconName, false);

    // we found the icon on the base Skin, but it is a ReferenceIcon.
    // find the actual icon
    if (resolve)
    {
      if (icon instanceof ReferenceIcon)
      {
        Icon resolvedIcon = _resolveReferenceIcon((ReferenceIcon)icon,
                                                   null);
        // register icon so we don't have to resolve reference next time.
        registerIcon(iconName, resolvedIcon);

        return resolvedIcon;
      }
    }

    // If we found the icon in the base Skin, register it on ourselves
    // so that we don't have to repeatedly retrieve the icon from
    // the base Skin.
    registerIcon(iconName, icon);


    return icon;
  }

  /**
   * Sets the name of the style sheet for this Skin.
   */
  public void setStyleSheetName(String styleSheetName)
  {
    _styleSheetName = styleSheetName;
  }

  /**
   * Override of Skin.getStyleSheetDocument() which merges
   * styles from the base Skin's style sheet and the
   * SkinExtension's style sheet.
   */
  @Override
  public StyleSheetDocument getStyleSheetDocument(StyleContext context)
  {
    // The SkinExtension's StyleSheetDocument is produced
    // by merging the styles provided by the base Skin with
    // the styles provided by the SkinExtension's own style
    // sheet.

    // Get the StyleSheetDocument from the base Skin
    SkinImpl baseSkin = (SkinImpl) getBaseSkin();
    StyleSheetDocument baseDocument = baseSkin.getStyleSheetDocument(context);

    // Get the StyleSheetDocument for the SkinExtension - we
    // just use super.getStyleSheetDocument() to get our own styles
    StyleSheetDocument extensionDocument = super.getStyleSheetDocument(
                                                   context);

    // Now, check to see if either of the StyleSheetDocuments have
    // changed. We synchronize here to prevent multiple threads coming
    // through and mucking with our StyleSheetDocument instance variables.
    synchronized (this)
    {
      if ((baseDocument != _baseStyleSheetDocument) ||
          (extensionDocument != _extensionStyleSheetDocument))
      {
        _baseStyleSheetDocument = baseDocument;
        _extensionStyleSheetDocument = extensionDocument;

        // One of the StyleSheetDocuments has changed - rebuild
        // the "full" StyleSheetDocument
        _fullStyleSheetDocument =
          StyleSheetDocumentUtils.mergeStyleSheetDocuments(baseDocument,
                                                           extensionDocument);
      }

      return _fullStyleSheetDocument;
    }
  }


  // Gets the translated value from the local translations cache.
  private Object _getCachedTranslatedValue(
    LocaleContext lContext,
    String        key
    )
  {
    // Get the translation Locale
    Locale locale = lContext.getTranslationLocale();

    // Get all of the translations for the current translation Locale
    Map<String, Object> localeTranslations = _translations.get(locale);

    if (localeTranslations != null)
      return localeTranslations.get(key);

    return null;
  }

  // Stores the translated value in the local translations cache
  // Note the synchronization!  This is necessary because we may be
  // putting translations into the cache from multiple request threads.
  synchronized private void _putCachedTranslatedValue(
    LocaleContext lContext,
    String        key,
    Object        value
    )
  {
    // Use a placeholder to mark null translations
    if (value == null)
      value = _NULL_TRANSLATION;

    // Get the translation Locale
    Locale locale = lContext.getTranslationLocale();

    // Get all of the translations for the current translation locale
    Map<String, Object> localeTranslations = _translations.get(locale);

    if (localeTranslations == null)
    {
      // If we didn't previously have any translations for this
      // Locale, create storage for translations now...
      localeTranslations = new OptimisticHashMap<String, Object>();
      _translations.put(locale, localeTranslations);
    }

    // Store the new component translations array
    localeTranslations.put(key, value);
  }

  // Checks whether there is a translation with the specified
  // key in the custom ResourceBundle.  We call this method
  // before calling ResourceBundle.getObject() because custom
  // bundles are not required to provide translations for all
  // messages.  If we didn't first check _isTranslationKeyAvailable(),
  // we might see a lot of MissingResourceExceptions.
  private boolean _isTranslationKeyAvailable(
    LocaleContext lContext,
    String        key
    )
  {
    String bundleName = getBundleName();
    if (bundleName == null)
      return false;

    Map<String, Boolean> keys = _getTranslationKeys(lContext);

    return keys.containsKey(key);
  }

  // Returns the a Map which contains the translation keys for
  // the specified locale.
  @SuppressWarnings("unchecked")
  private Map<String, Boolean> _getTranslationKeys(
    LocaleContext lContext
    )
  {
   // We store the translation keys map in the translation cache
    Map<String, Boolean> keys = 
      (Map<String, Boolean>)_getCachedTranslatedValue(lContext,
                                              _TRANSLATION_KEYS_KEY);

    if (keys == null)
    {
      String bundleName = getBundleName();
      assert bundleName != null;

      keys = new HashMap<String, Boolean>();

      ResourceBundle bundle = null;

      try
      {
        bundle = lContext.getBundle(bundleName);
      }
      catch (MissingResourceException e)
      {
        // It is possible that the call to getBundle() might
        // fail with a MissingResourceException if the customer
        // has only provided a custom bundle for certain languages.
        // This is okay, so we just eat these exceptions.
        ;
      }

      if (bundle != null)
      {
        Enumeration<String> en = bundle.getKeys();

        if (en != null)
        {
          while (en.hasMoreElements())
            keys.put(en.nextElement(), Boolean.TRUE);
        }
      }

      if (keys.isEmpty())
        keys = Collections.emptyMap();
      else
        keys = Collections.unmodifiableMap(keys);

      _putCachedTranslatedValue(lContext,
                                _TRANSLATION_KEYS_KEY,
                                keys);
    }

    return keys;
  }
  /**
   * Find the actual icon
   * @param refIcon a ReferenceIcon instance
   * @param referencedIconStack  The stack of reference icon names which have
   *          already been visited.  Used to detect circular dependencies.
   * @return icon which is resolved. i.e., it is not a ReferenceIcon.
   */
  private Icon _resolveReferenceIcon(
    ReferenceIcon refIcon,
    Stack<String> referencedIconStack)
  {

    String refName = refIcon.getName();

    // make sure we don't have a circular dependency
    if ( _stackContains(referencedIconStack, refName))
    {
      if (_LOG.isWarning())
        _LOG.warning(_CIRCULAR_INCLUDE_ERROR + refName);
      return null;
    }
    
    if (referencedIconStack == null)
    {
      // -= Simon Lessard =- 
      // TODO: Check if something better than Stack can be used
      referencedIconStack = new Stack<String>();
    }

    referencedIconStack.push(refName);
    //

    // see if the referenced icon is registered already registered in this skin.
    Icon icon = super.getIcon(refName, false);

    if (icon != null)
    {
      if (icon instanceof ReferenceIcon)
      {
          Icon resolvedIcon = _resolveReferenceIcon((ReferenceIcon)icon,
                                                     referencedIconStack);
          return resolvedIcon;
      }
      else
      {
        return icon;
      }
    }
    else
    {

      Skin baseSkin = getBaseSkin();
      icon = baseSkin.getIcon(refName, false);

      if (icon instanceof ReferenceIcon)
      {
        Icon resolvedIcon = _resolveReferenceIcon((ReferenceIcon)icon,
                                                   referencedIconStack);
        return resolvedIcon;
      }
      else
      {
        return icon;
      }
    }
  }

    // Tests whether the value is present in the (possibly null) stack.
  private static boolean _stackContains(
      Stack<String> stack, 
      Object value)
  {
    if (stack == null)
      return false;

    return stack.contains(value);
  }

  private SkinExtension() {}


  // Icon class that we use as a placeholder for null icons
  private static class NullIcon extends Icon
  {
    @Override
    public void renderIcon(
      FacesContext        context,
      RenderingContext    arc,
      Map<String, Object> attrs
      ) throws IOException
    {
      // null icons don't render anything
    }
  }

  private String      _id;
  private String      _family;
  private String      _renderKitId;
  private SkinImpl    _baseSkin;
  private String      _styleSheetName;
  private String      _bundleName;

  // Now that we look into possibly multiple ResourceBundles
  // to find a translation (eg. the local bundle, a component
  // provider's bundle, the super-LAFs bundle), translation lookups
  // can become expensive.  So, we keep a local cache all translated
  // resources which is populated as we go along.  Translations are
  // hashed by Locale/component name/key.  This is more
  // hashing than usual to get a translation, but at least this is
  // a finite/fixed expense.  At least this way translation lookup
  // performance won't degrade if a custom bundle is provided - or
  // if we need to pull the translation from some other Skin.
  //
  // This HashMap hashes Locales -> HashMaps.
  // The HashMaps map translation key to message.
  private OptimisticHashMap<Locale, Map<String, Object>> _translations = 
    new OptimisticHashMap<Locale, Map<String, Object>>(13);

  // The StyleSheetDocument for the base LookAndFeel's style sheet
  private StyleSheetDocument _baseStyleSheetDocument;

  // The StyleSheetDocument for the LookAndFeelExtension's style sheet
  private StyleSheetDocument _extensionStyleSheetDocument;

  // The complete StyleSheetDocument which merges the styles
  // provided by the base LookAndFeel with the styles added by
  // the SkinExtension.
  private StyleSheetDocument _fullStyleSheetDocument;

  // Special key that we use for storing the translation keys
  // in the translation cache.
  private static final String _TRANSLATION_KEYS_KEY = "_uixLafTransKeys";

  // Placeholder for null icons
  private static final Icon _NULL_ICON = new NullIcon();

  // Placeholder for null translations
  private static final Object _NULL_TRANSLATION = new Object();

  // Error messages
  private static final String _CIRCULAR_INCLUDE_ERROR =
    "Circular dependency detected in skin reference icon ";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SkinExtension.class);
}
