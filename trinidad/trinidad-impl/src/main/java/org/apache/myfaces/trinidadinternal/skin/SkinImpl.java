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
package org.apache.myfaces.trinidadinternal.skin;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Stack;

import java.util.concurrent.ConcurrentHashMap;

import java.util.concurrent.atomic.AtomicInteger;

import javax.faces.context.ExternalContext;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidad.skin.Skin;

import org.apache.myfaces.trinidad.skin.SkinAddition;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinProperties;
import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidadinternal.share.expl.Coercions;
import org.apache.myfaces.trinidadinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.xml.StyleSheetDocumentUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.parse.IconNode;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.parse.SkinPropertyNode;

/**
 * Defines the components (icons, styles, etc)
 * which are used to implement a particular skin.
 *
 * This implementation class adds the details that should
 * not be exposed outside of this API.
 *
 * @see SkinFactory
 * @see org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext#getSkinFactory
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/Skin.java#0 $) $Date: 10-nov-2005.18:58:54 $
 */
abstract public class SkinImpl extends Skin
{

  /**
   * Returns an string identifier which uniquely identies
   * this Skin implementation.  Skin implementations
   * can be retrieved by id via SkinFactory.getSkin().
   * @see org.apache.myfaces.trinidadinternal.skin.SkinFactory#getSkin
   */
  @Override
  public String getId()
  {
    return null;
  }

  /**
   * Returns the name of the skin "family" for this skin.
   * The family name is used when specifying a preferred skin
   * in trinidad-config.xml.
   * This provides a way to refer to a group of
   * related skin implementations while allowing the
   * particular skin instance to be selected based on the
   * current render-kit-id.
   */
  @Override
  public String getFamily()
  {
    return null;
  }

  /**
   * Returns the renderKitId for the Skin.
   */
  @Override
  public String getRenderKitId()
  {
    return null;
  }
  
  /**
   * Returns the id of the Skin's stylesheet document. This is the StyleSheetDocument's 
   * id for the StyleContext.
   */
   @Override
  public String getStyleSheetDocumentId(RenderingContext arc)
  {
    StyleContext sContext = ((CoreRenderingContext)arc).getStyleContext();
    return getStyleSheetDocument(sContext).getDocumentId(sContext);
  }

  /**
   * Returns the name of the style sheet for this Skin.
   */
  @Override
  abstract public String getStyleSheetName();

  /**
   * Returns a translated String in the LocaleContext's translation Locale.
   */
  @Override
  public String getTranslatedString(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException
  {
    Object o  = getTranslatedValue(lContext, key);
    if (o == null)
      return null;

    return o.toString();
  }

  /**
   * Returns a translated value in the LocaleContext's translation Locale.
   * This value may or may not be a String, and developers should avoid
   * calling toString() unless absolutely necessary.
   * @param lContext The LocaleContext which provides the translation Locale.
   *                 Cannot be null.
   * @param key The key of the translation to retrieve. Cannot be null.
   * @throws NullPointerException if lContext or key is null.
   * @throws MissingResourceException if the resource key cannot be found in the skin's bundle
   * or the skin additions' bundles.
   */
  @Override
  public Object getTranslatedValue(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException
  {
    if (lContext == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_LOCALE_CONTEXT"));
    if (key == null)
      throw new NullPointerException("Null key");

    List<String> resourceBundleNames = _getResourceBundleNames();
    
    // if there is nothing to check, return null
    if (resourceBundleNames.size() == 0)
      return null;
      
    Object translatedValue = getCachedTranslatedValue(lContext, key);
    if (translatedValue == null)
    {
      throw new MissingResourceException("Can't find resource for bundle "
                                         +resourceBundleNames
                                         +", key "+key,
                                         getBundleName(),
                                         key);
    }
    
    return translatedValue;


  }


  /**
   * Our renderers call this to get the icon. This returns a renderable
   * icon. (ReferenceIcons are resolved -- the real icon they point to is
   * returned)
   */
  @Override
  public Icon getIcon(
    String  iconName
    )
  {
    return getIcon(iconName, true);
  }

  /**
   * Returns an Icon object; can be a ReferenceIcon.
   * @param iconName  The name of the icon to retrieve. Cannot be null
   * @throws NullPointerException if iconName is null.
   */
  @Override
  public Icon getIcon(
    String  iconName,
    boolean resolveIcon
    )
  {
    if (iconName == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_ICONNAME"));

    Icon icon = _icons.get(iconName);
    if (resolveIcon)
    {
      if (icon instanceof ReferenceIcon)
      {
        // find the true icon, not a ReferenceIcon
        icon = _resolveReferenceIcon((ReferenceIcon)icon,
                                     null);
      }
    }

    return icon;
  }

  /**
   * Registers an Icon for the specified icon name.
   * @param iconName  The name of the icon. Cannot be null.
   * @param icon      The Icon to register.
   * @throws NullPointerException if iconName is null.
   */
  @Override
  synchronized public void registerIcon(
    String  iconName,
    Icon    icon
    )
  {
    if (iconName == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_ICONNAME"));

    _icons.put(iconName, icon);
  }
 
  /**
   * Adds a SkinAddition on this Skin. You can call this method as many times 
   * as you like for the Skin, and it will add the SkinAddition to the list of 
   * SkinAdditions.
   * However, it does not make sense to call this method more than once
   * with the same SkinAddition object.
   * This is meant for the skin-addition use-cases, where a custom component 
   * developer has a style sheet and/or resource bundle for their custom   
   * components, and they want the style sheet and/or resource bundle 
   * to work for this Skin and the children Skins.
   * The stylesheets specified in the SkinAdditions will be merged with the 
   * Skin's own styles.
   * The resource bundles specified in the SkinAdditions will be looked into 
   * if the translated key is not found in the Skin's own resource bundle 
   * during the call to getTranslatedString or getTranslatedValue.
   * 
   * @param skinAddition The SkinAddition object to add to the Skin.
   * @throws NullPointerException if SkinAddition is null.
   */
  public void addSkinAddition (
    SkinAddition skinAddition
    )
  {
    // TODO change error message to use the error message resource bundle.
     if (skinAddition == null)
       throw new NullPointerException(
               "A null SkinAddition object was passed to addSkinAddition.");

     if (_skinAdditions == null)
     {
       _skinAdditions = new ArrayList<SkinAddition>();
     }
     _skinAdditions.add(skinAddition);
  }
  
  /**
   * Gets an unmodifiable List of SkinAdditions that have been added 
   * on this Skin. To add to the SkinAdditions List, 
   * call addSkinAddition(SkinAddition)
   * @return List an unmodifiable List of SkinAdditions.
   * @see #addSkinAddition(SkinAddition)
   */
  public List<SkinAddition> getSkinAdditions()
  {
    if (_skinAdditions == null)
    {
      return Collections.emptyList();
    }
    else  
      return Collections.unmodifiableList(_skinAdditions);
  }

   /**
    * Returns the style class map, or null if there is no map.
    * Some StyleProvider implementations, such as the FileSystemStyleCache,
    * automatically provide compressed versions style class names.  The
    * short style classes can be used instead of the full style class
    * names to reduce the overall size of generated content.
    * @param arc RenderingContext
    * @return Map&lt;String, String&gt; It should be a map that contains the full style class name
    * as the key, and the value could be a shortened style class name,
    * or a portlet style class name, etc.
    */
  @Override
   public Map<String, String> getStyleClassMap(
     RenderingContext arc
     )
   {
     ExternalContext external  = FacesContext.getCurrentInstance().getExternalContext();
     if (!"true".equals(
          external.getInitParameter(
            Configuration.DISABLE_CONTENT_COMPRESSION)))
     {
       StyleContext sContext = ((CoreRenderingContext)arc).getStyleContext();
       StyleProvider sProvider = sContext.getStyleProvider();
       return sProvider.getShortStyleClasses(sContext);   
     }
     return null;
   }
  
  /**
   * Returns the StyleSheetDocument object which defines all of the
   * styles for this Skin, including any styles that are
   * contributed by skin-additions.
   */
  public StyleSheetDocument getStyleSheetDocument(StyleContext context)
  {
    // We synchronize here because technically speaking multiple
    // threads can simultaneously update the underlying style sheet
    // objects.  However, in practice, this should never be an issue -
    // especially in production systems where modification checking
    // should always be disabled.  In this case, the style sheet objects
    // are created when the first request is received and are never
    // updated after that point.  So, if this synchronization turns
    // out to be a bottleneck, it should be safe to remove this
    // synchronization - at least when modification checking is
    // disabled, if not altogether.
    synchronized (this)
    {
      if ((_styleSheetDocument == null) || _checkStylesModified(context))
        _styleSheetDocument = _createStyleSheetDocument(context);

      return _styleSheetDocument;
    }
  }

  /**
   * Retrieves a property that was set via a call to setProperty().
   * Some Renderer implementations may store properties on the
   * Skin instance to avoid having to re-compute Skin-specific
   * values on each render.
   */
  @Override
  public Object getProperty(Object key)
  {
    return _properties.get(key);
  }

  /**
   * Sets a value for the specified property key.
   */
  @Override
  public void setProperty(
    Object key,
    Object value
    )
  {
    _properties.put(key, value);
  }
  

  /**
   * @param styleSheetName
   * @see #addSkinAddition(SkinAddition)
   * @deprecated Use addSkinAddition instead
   */
  public void registerStyleSheet(String styleSheetName) 
  {
    //TODO Take out deprecated after sufficient amount of time has passed
    // deprecated July, 2007
    SkinAddition addition = new SkinAddition(styleSheetName, null);
    addSkinAddition(addition);
  }

  /**
   * Returns a translated value in the LocaleContext's translation Locale, or null
   * if the key could not be found.
   * This value may or may not be a String, and developers should avoid
   * calling toString() unless absolutely necessary.
   * This method protects against MissingResourceExceptions by checking that the key exists 
   * before calling the bundle's getObject method. It eats any MissingResourceExceptions as
   * a result of not finding the bundle, since there can be multiple bundles per skin, and
   * we could get a lot of MissingResourceExceptions otherwise.
   * Then the method caches the value once it is found in a particular
   * resource bundle. 
   * This method is useful for SkinExtensions which will also check their ancestor skins
   * for the resource if it is not found in the SkinExtension. MissingResourceExceptions would
   * be numerous if we didn't protect against them.
   * If you want to throw a MissingResourceException once all the ancestor skins and their
   * bundles and registered bundles are checked, then you should call getTranslatedValue for the
   * most base skin, and it will throw a MissingResourceException if the
   * key was not found in any of the bundles.
   * @see #getTranslatedValue(LocaleContext, String)
   * @param lContext The LocaleContext which provides the translation Locale.
   *                 Cannot be null.
   * @param key The key of the translation to retrieve. Cannot be null.
   * @throws NullPointerException if lContext or key is null.
   * @return Object translated value of the key;
   *         null if bundleName and skin-addition bundleNames are null for this Skin;
   *         null if the key cannot be found in the bundle or registered bundles -or-
   *         

   */
  protected Object getCachedTranslatedValue(
    LocaleContext lContext,
    String        key
    )
  {
    if (lContext == null)
      throw new NullPointerException(_LOG.getMessage(
        "NULL_LOCALE_CONTEXT"));
    if (key == null)
      throw new NullPointerException("Null key");

    List<String> resourceBundleNames = _getResourceBundleNames();

    return _getCachedTranslationValueFromLocale(lContext, 
                                                resourceBundleNames, key);
    
  }
  
  /**
   * Put the locale/key/value in the cache (i.e., translations map). This is useful for subclasses
   * to call so that they can store/retrieve the key/value locally rather than always
   * having to look in the parent skins' maps.
   * @param lContext
   * @param key
   * @param value
   */
  protected void putTranslatedValueInLocaleCache(
    LocaleContext lContext,
    String        key,
    Object        value)
  {
    Locale locale = lContext.getTranslationLocale();
    
    KeyValueMapStatus keyValueMapStatus = _translations.get(locale);
    if (keyValueMapStatus != null)
    {
      Map keyValueMap = keyValueMapStatus.getKeyValueMap();
      if (keyValueMap != null)
      {
        keyValueMap.put(key, value);
      }
    }
    else
    {
      // in the usual program flow, this won't get called here because the keyValueMapStatus
      // is created in getCachedTranslatedValue's call of _getCachedTranslationValueFromLocale
      // it is here as a safeguard in case the keyValueMapStatus isn't there.
      _createKeyValueMapStatusInCache(locale, key, value);    
    }
  }

  /**
  * Returns the name of the ResourceBundle for this Skin instance.
  * This does not include the SkinAddition resource bundles.
  * We differentiate between the two types of resource bundles so that
  * the Skin's own resource bundle can take precedence.
  */
  abstract protected String getBundleName();


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
      referencedIconStack = new Stack<String>();
    }

    referencedIconStack.push(refName);

    Icon icon = getIcon(refName, false);

    if ((icon instanceof ReferenceIcon) && (icon != null))
    {

      return _resolveReferenceIcon((ReferenceIcon)icon,
                                    referencedIconStack);

    }

    return icon;
  }

  // Tests whether the value is present in the (possibly null) stack.
  private static boolean _stackContains(Stack<String> stack, Object value)
  {
    if (stack == null)
      return false;

    return stack.contains(value);
  }

  // Checks to see whether any of our style sheets have been updated
  private boolean _checkStylesModified(
    StyleContext context
    )
  {
    boolean modified = false;

    if (_skinStyleSheet != null)
      modified = _skinStyleSheet.checkModified(context);

    // We also check all of the skin-addition style sheets even
    // if we already know that the skin's style sheet has been
    // modified.  We need to do this because we want to call
    // StyleSheetEntry.checkModified() for each entry - otherwise
    // out of date StyleSheetEntries may not get updated.
    if (_skinAdditionStyleSheets != null)
    {
      for (int i = 0; i < _skinAdditionStyleSheets.length; i++)
      {
        StyleSheetEntry entry = _skinAdditionStyleSheets[i];
        if (entry.checkModified(context))
          modified = true;
      }
    }

    return modified;
  }
  
  private void _registerIconsAndPropertiesFromStyleSheetEntry(
    StyleSheetEntry entry)
  {
    // register the icons and properties if there are any.
    // get a List of IconNodes, and register them.
    if (entry != null)
    {
      // register icons
      List<IconNode> icons = entry.getIcons();
      if (icons != null)
      {
        for(IconNode iconNode : icons)
        {
          registerIcon(iconNode.getIconName(), iconNode.getIcon());
        }
      }
      
      // register properties
      List<SkinPropertyNode> skinProperties = entry.getSkinProperties();
      
      if (skinProperties != null)
      {
        for(SkinPropertyNode property : skinProperties)
        {
          Object propValueObj = property.getPropertyValue();
          // Store the property selector + property Name as the Skin Property Key.
          // e.g., use af|breadCrumbs-tr-show-last-item

          String key = property.getPropertySelector() +
                       property.getPropertyName();
          // look up in map to get conversion
          Class<?> type = _PROPERTY_CLASS_TYPE_MAP.get(key);
          if (type != null)
          {
            try
            {
              // coerce the value to the type
              propValueObj = Coercions.coerce(null, (String)propValueObj,
                            type);
            }
            catch (IllegalArgumentException ex)
            {
              if (_LOG.isWarning())
                _LOG.warning(ex);
            }
          }


          setProperty(key, propValueObj);
        }
      }
    }    
  }

  // Creates the StyleSheetDocument for this Skin
  // (as a side effect, this also registers icons and skin properties
  // defined in the skin's style-sheet.)
  private StyleSheetDocument _createStyleSheetDocument(
    StyleContext context
    )
  {
    // If we haven't created the StyleSheetDocument yet,
    // then presumably we haven't attempted to load the
    // style sheet files.  Create StyleSheetEntry instances
    // for each style sheet file that we depend on.
    if (_styleSheetDocument == null)
    {
      String styleSheetName = getStyleSheetName();

      if (styleSheetName != null)
      {
        _skinStyleSheet = StyleSheetEntry.createEntry(context, styleSheetName);
        _registerIconsAndPropertiesFromStyleSheetEntry(_skinStyleSheet);
      }

      // Now create entries for skin-addition-specific style sheets.
      _skinAdditionStyleSheets = _getSkinAdditionsStyleSheets(context);
    }

    // Now merge all of the documents provided by all of our
    // entries into a single StyleSheetDocument.
    StyleSheetDocument document = null;

    if (_skinStyleSheet != null)
      document = _skinStyleSheet.getDocument();

    // Merge in any skin-addition style sheets on top of
    // the skin's style sheet
    if (_skinAdditionStyleSheets != null)
    {
      for (int i = 0; i < _skinAdditionStyleSheets.length; i++)
      {
        StyleSheetEntry entry = _skinAdditionStyleSheets[i];
        if (entry != null)
        {
          // add the icons and properties that are in the 
          // skin-addition's StyleSheetEntry
           _registerIconsAndPropertiesFromStyleSheetEntry(entry);
           
          // now merge the css properties
          StyleSheetDocument additionDocument = entry.getDocument();
  
          if (additionDocument != null)
          {
            // Merge the skin-addition's StyleSheetDocument on top of
            // the current StyleSheetDocument.  Note: This is not
            // exactly efficient - we would be better off creating
            // an array of StyleSheetDocuments and merging them all
            // in one pass.  But since this code should rarely be
            // executed, this shouldn't be a bottleneck...
            document = StyleSheetDocumentUtils.mergeStyleSheetDocuments(
                                                 document,
                                                 additionDocument);
  
          }
        }
      }
    }

    // We're done!
    if (document != null)
      return document;

    // If we weren't able to produce a StyleSheetDocument for whatever
    // reason (maybe we don't have any style sheet, maybe there were
    // I/O problems), create a empty StyleSheetDocument so that we
    // don't repeatedly try to re-create the document.
    return new StyleSheetDocument(null, 
                                  null, 
                                  StyleSheetDocument.UNKNOWN_TIMESTAMP);
  }

  // Gets the StyleSheetEntries for skin-additions
  private StyleSheetEntry[] _getSkinAdditionsStyleSheets(StyleContext context)
  {
    List<String> skinAdditionStyleSheetNames = _getSkinAdditionsStyleSheetNames();
    if (skinAdditionStyleSheetNames.size() == 0)
      return null;

    // Create a list to hold our StyleSheetEntries
    int count = skinAdditionStyleSheetNames.size();
    List<StyleSheetEntry> entries = new ArrayList<StyleSheetEntry>(count);

    // Loop through all registered style sheet names and
    // try to create a StyleSheetEntry for each name.
    for(String name : skinAdditionStyleSheetNames)
    {
      StyleSheetEntry entry = StyleSheetEntry.createEntry(context, name);
      if (entry != null)
      {
        entries.add(entry);
      }
    }

    if (!entries.isEmpty())
    {
      _skinAdditionStyleSheets = new StyleSheetEntry[entries.size()];
      return entries.toArray(_skinAdditionStyleSheets);
    }

    return null;
  }
  
  /*
   * Returns an  List of skin-addition style sheets for the Skin. 
   * These stylesheets are added with addSkinAddition.
   * This List does not include the skin's own stylesheet.
   * @return List<String> of skin addition stylesheet names. It will
   * return a List of size 0 if no skin addition stylesheets exist.
   * @see #addSkinAddition(String, String)
   * @see #getStyleSheetName()
   */
  private List<String> _getSkinAdditionsStyleSheetNames() 
  {
    // Get all the SkinAdditions's style sheet names.
    // Get the style sheet names and create a List
    // Cache this list in an instance variable

    if (_skinAdditionStyleSheetNames != null)
      return _skinAdditionStyleSheetNames;
  
    // loop through all the SkinAdditions and get the resource bundles
    List<SkinAddition> additions = getSkinAdditions();

    List<String> styleSheetNames = new ArrayList<String>(additions.size());

    for (SkinAddition addition : additions)
    {
      String name = addition.getStyleSheetName();
      if (name != null)
      {
        styleSheetNames.add(name);
      }
    }
    
    // cache in instance variable
    _skinAdditionStyleSheetNames = styleSheetNames;

    return _skinAdditionStyleSheetNames;
  }
  
  /*
   * Returns the List of all the ResourceBundles for the Skin --
   * including Skin's bundle and the skin addition bundles
   * These resourceBundles are added with addSkinAddition.
   * @see #addSkinAddition(String, String)
   * @see #getBundleName()
   */
  private List<String> _getResourceBundleNames() 
  {
    // Get all the SkinAdditions's resource bundles.
    // Get the resource bundle names and create a List
    // Cache this list in instance variable
    
    // return if already cached
    if (_resourceBundleNames != null)
      return _resourceBundleNames;
  
    // We haven't retrieved the bundle names yet, so do so now.
    String bundleName = getBundleName();
    
    List<SkinAddition> additions = getSkinAdditions();
    
    int resourceBundleCount = additions.size();
    if (bundleName != null)
      resourceBundleCount++;
    
    List<String> bundleNameList = new ArrayList<String>(resourceBundleCount);
    
    if (bundleName != null)
        bundleNameList.add(bundleName);
    
    for (SkinAddition addition : additions)
    {
      String name = addition.getResourceBundleName();
      if (name != null)
      {
        bundleNameList.add(name);
      }
    }
 
    // cache in instance variable
    _resourceBundleNames = bundleNameList;
    
    return _resourceBundleNames;
  }

  
  // get the cached value for the locale and key from the _translations map.
  // If the value does not exist, then find it in the resource bundles,
  // searching the Skin's bundle first, then each skin addition resource
  // bundle until it is found. This method fills in the cached key/value map
  // as we look for the key/value. It keeps track of which bundles we looked
  // in so that we don't have to look in them any more for this session.
  private Object _getCachedTranslationValueFromLocale(
    LocaleContext lContext,
    List<String> resourceBundleNames,
    String        key
    )
  {
    Locale locale = lContext.getTranslationLocale();
    
    KeyValueMapStatus keyValueMapStatus = _translations.get(locale);
    Map keyValueMap = null;
    
    if (keyValueMapStatus != null)
    {
      keyValueMap = keyValueMapStatus.getKeyValueMap();
      if (keyValueMap != null)
      {
        Object value = keyValueMap.get(key);
        if (value != null)
        {
          return value; 
        }
      }
    }
    else
    {
      // create the keyValueMapStatus object and put it on the locale

      keyValueMapStatus = _createKeyValueMapStatusInCache(locale, key, null);
      keyValueMap = keyValueMapStatus.getKeyValueMap();

    }
    
    // at this point the keyValueMapStatus is set on the locale, 
    // and we know we have to fill it in.
    // getProcessedBundlesIndex will tell us which resource bundles
    // we have already processed (locale bundle + skin-addition bundles)
    // we increment this number after we look in each bundle and update
    // the keyValueMap.
            
    int numberOfBundleNames = resourceBundleNames.size();
    // if there is nothing to check, return null
    if (numberOfBundleNames == 0)
      return null;
    
    // in theory, multiple threads could get the same processedBundleIndex
    // here, so we could get all these threads updating the same map, but
    // it will eventually update the index, so I won't worry about this now.
    int startIndex = keyValueMapStatus.getProcessedBundlesIndex();
    for (int i=startIndex; i < numberOfBundleNames;)
    {
      String bundleName = resourceBundleNames.get(i);
      // 'true' means to check if the key already exists in the keyValueMap and 
      // if so do not override.
      _fillInKeyValueMap(lContext, bundleName, keyValueMap, (i==0));
      i = keyValueMapStatus.incrementAndGetProcessedBundlesIndex();
      Object value = keyValueMap.get(key);
      if (value != null)
      {
        return value;
      } 
    }   
    
    // nothing was found
    return null;

  }
  
  // this method provides a single point of entry for creating KeyValueMapStatus object and
  // putting the locale/keyValueMapStatus in the _translations map. If value != null,
  // it adds the key/value to the keyValueMap.
  // It synchronizes on the _translations parameter
  // It returns the newly created KeyValueMapStatus object.
  private KeyValueMapStatus _createKeyValueMapStatusInCache(
    Locale locale,
    String key,
    Object value 
  )
  {
    KeyValueMapStatus keyValueMapStatus = null;
    
    // create the keyValueMapStatus object and put it on the locale
    synchronized (_translations)
    {
      // check to see if another thread has put locale in the map
      if (!_translations.contains(locale))
      {
        keyValueMapStatus = new KeyValueMapStatus();
        if (value != null)
        {
          Map keyValueMap = keyValueMapStatus.getKeyValueMap();
          keyValueMap.put(key, value);
        }
        _translations.put(locale, keyValueMapStatus);
      }
      else
      {
        keyValueMapStatus = _translations.get(locale);
      }
    }

    return keyValueMapStatus;
    
  }

  /**
   * Fill in the keyValueMap with all the keys and values for the ResourceBundle.
   * The ResourceBundle is found by calling lContext.getBundle(bundleName).
   * MissingResourceExceptions are not thrown, since it is possible that 
   * SkinExtensions have only provided custom bundles for certain languages.
   * @param lContext LocaleContext,  LocaleContext maintains a cache of found ResourceBundles
   * @param bundleName the resource bundle's name.
   * @param keyValueMap A Map of bundle keys to their values
   * @param checkForKey If true, we will check if the key is already in the map 
   *                    and not re-add it. If false, we don't bother checking, 
   *                    we just add it. When this method is called for the
   *                    first bundle, then we know we do not need to check.
   */
  private void _fillInKeyValueMap(
    LocaleContext lContext, 
    String        bundleName,
    Map           keyValueMap,
    boolean       checkForKey)
  {
  
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
        {
          String bundleKey = en.nextElement();
          // if checkForKey is true, don't override an existing key/value
          if (checkForKey)
          {
            if (!keyValueMap.containsKey(bundleKey))
            {
              Object value = bundle.getObject(bundleKey);
              if (value != null)
                keyValueMap.put(bundleKey, value);
            }            
          }
          else
          {
            Object value = bundle.getObject(bundleKey);
            if (value != null)
              keyValueMap.put(bundleKey, value);             
          }
        }
      }
    }       
  }
  
  // This is the 'value' of the _translations map.
  // This contains a translation key/value map which contains
  // all the translation keys and values in the resource bundles
  // we have processed thus far for a particular locale.
  // It also contains an index which keeps track of how
  // many of the bundles we have checked so far, so that
  // we don't recheck a bundle.
  // This is to help with performance, since getting
  // values from a resource bundle is expensive.
  private static class KeyValueMapStatus
  {
   
    KeyValueMapStatus()
    {
      _keyValueMap = new ConcurrentHashMap<String, Object>();
      _processedBundlesIndex = new AtomicInteger(0);
    }
    
    // get the current key/value Map
    public Map<String, Object> getKeyValueMap()
    {
      return _keyValueMap;
    }
    
    // Get the current value of processedBundlesIndex.
    public int getProcessedBundlesIndex()
    {
      return _processedBundlesIndex.get();
    } 
    
    // Atomically increment by one the current value of processBundlesIndex.
    // @return the updated value
    public int incrementAndGetProcessedBundlesIndex()
    {
      return _processedBundlesIndex.incrementAndGet();
    }         

    Map<String, Object> _keyValueMap;
    // This keeps track of the number of bundles that have been processed.
    // A Skin can have multiple bundles registered on it -- a local resource 
    // bundle + any number of skin-addition bundles.  
    // When we get a key (getTranslatedValue), we check each bundle
    // and fill in the keyValueMap until we find the key. 
    // We update this index after we process each
    // bundle, so that we don't recheck a bundle. We only have to check a bundle 
    // once per locale per session, because we cache the keys/values for each
    // bundle we check in the _keyValueMap.
    AtomicInteger       _processedBundlesIndex;
  }
    
  // Now that we look into possibly multiple ResourceBundles
  // to find a translation (eg. the local bundle + a skin-addition's
  // bundle), translation lookups can become expensive.
  // To get a value, we call: lContext.getBundle(name).getObject(key).
  // We speed things up by caching the translations
  // in this _translations map. 
  // As we get a call to getTranslatedValue with a key, we
  // look through our keyValueMap. If it isn't there, we loop
  // through each resource bundle we haven't yet checked, 
  // and we get all the keys and values
  // and when we have all the keys/values for the
  // bundle, we return if the key/value is there. Otherwise,
  // we check the next bundle and so on.
  // If we never get a request for a key in bundle X, that bundle X's
  // keys/values will never be put in the keyvalue map. This is a good thing.
  private ConcurrentHashMap<Locale, KeyValueMapStatus> _translations =
    new ConcurrentHashMap<Locale, KeyValueMapStatus>(13);

  // HashMap that maps icon name to Icons
  private ConcurrentHashMap<String, Icon> _icons = new ConcurrentHashMap<String, Icon>();

  // The StyleSheetDocument which contains all of the styles
  // for this Skin - including styles contributed by skin-additions.
  private StyleSheetDocument _styleSheetDocument;

  // A StyleSheetEntry which defines the styles that are
  // provided by this Skin's style sheet only (does
  // not include skin-additions styles).
  private StyleSheetEntry _skinStyleSheet;

  // List of skin-additions style sheet names for this Skin
  private List<String> _skinAdditionStyleSheetNames;

  // Array of skin-additions StyleSheetEntries
  private StyleSheetEntry[] _skinAdditionStyleSheets;
  
  // List of all the resource bundle names (bundleName + skin-additions)
  private List<String> _resourceBundleNames; 
  
  // List of skin-additions for this Skin
  private List<SkinAddition> _skinAdditions;  

  // HashMap of Skin properties
  private ConcurrentHashMap<Object, Object> _properties= new ConcurrentHashMap<Object, Object>();

  // Map of property to class type
  private static final Map<String, Class<?>> _PROPERTY_CLASS_TYPE_MAP;
  static
  {
    _PROPERTY_CLASS_TYPE_MAP = new HashMap<String, Class<?>>();
    
    _PROPERTY_CLASS_TYPE_MAP.put(
      SkinProperties.AF_NAVIGATIONPATH_SHOW_LAST_ITEM_PROPERTY_KEY, Boolean.class);
    _PROPERTY_CLASS_TYPE_MAP.put(
      SkinProperties.AF_TABLE_SELECTION_BAR_IN_TABLE, Boolean.class);
    _PROPERTY_CLASS_TYPE_MAP.put(
      SkinProperties.AF_TABLE_REPEAT_CONTROL_BAR, Boolean.class);
  }

  // Error messages
  private static final String _CIRCULAR_INCLUDE_ERROR =
    "Circular dependency detected in skin reference icon ";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(Skin.class);
}
