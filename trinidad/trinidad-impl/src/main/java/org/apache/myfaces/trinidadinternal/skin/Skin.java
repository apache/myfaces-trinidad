/*
 * Copyright  2000-2006 The Apache Software Foundation.
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

import java.util.HashMap;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;

import java.util.Stack;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.share.expl.Coercions;
import org.apache.myfaces.trinidadinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.parse.IconNode;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.parse.SkinPropertyNode;
import org.apache.myfaces.trinidadinternal.util.OptimisticHashMap;

import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.xml.StyleSheetDocumentUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;

import org.apache.myfaces.trinidadinternal.share.nls.LocaleContext;

import org.apache.myfaces.trinidadinternal.skin.icon.Icon;



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
  public String getId()
  {
    return null;
  }

  /**
   * Returns the name of the skin "family" for this skin.
   * The family name is used when specifying a preferred skin
   * in adf-faces-config.xml.
   * This provides a way to refer to a group of
   * related skin implementations while allowing the
   * particular skin instance to be selected based on the
   * current render-kit-id.
   */
  public String getFamily()
  {
    return null;
  }

  /**
   * Returns the renderKitId for the Skin.
   */
  public String getRenderKitId()
  {
    return null;
  }

  /**
   * Returns the name of the XSS style sheet for this Skin.
   */
  abstract public String getStyleSheetName();

  /**
   * Returns a translated String in the LocaleContext's translation Locale.
   */
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
   */
  public Object getTranslatedValue(
    LocaleContext lContext,
    String        key
    ) throws MissingResourceException
  {
    //testTranslationKey(key); //jmw test

    if (lContext == null)
      throw new NullPointerException("Null lContext");
    if (key == null)
      throw new NullPointerException("Null key");

    String bundleName = getBundleName();
    if (bundleName == null)
      return null;

    return lContext.getBundle(bundleName).getObject(key);
  }



  /**
   *  This is a test function.
   *  This looks at the stack that I have on the request map for the
   *  current component being rendered -- I store the prefix in
   *  UIComponentUINode.
   * @param key
   */
   /********* jmw for testing translation keys
  public static void testTranslationKey(
    String key)
  {

    javax.faces.context.FacesContext fcontext =
      javax.faces.context.FacesContext.getCurrentInstance();
    Stack translationKeyStack  = (Stack)fcontext.getExternalContext().
                                        getRequestMap().get("TRANSLATION_KEY");


    String translationKeyPrefix = null;

    if ((translationKeyStack != null) && !translationKeyStack.empty())
      translationKeyPrefix = (String)translationKeyStack.peek();


 //   System.out.println(translationKeyPrefix + " / " + key); //jmw test

    String keyPrefix = null;
    int index = key.indexOf('.');
    if (index != -1)
      keyPrefix = key.substring(0, index);

    if (translationKeyPrefix != null)
    {
      if (!(translationKeyPrefix.equalsIgnoreCase(keyPrefix)))
      {
        System.out.println("***NO MATCH " + translationKeyPrefix + " / " + key + "");
      }
    }
    if ((translationKeyPrefix == null) && (key != null) &&
      !(key.equals("WINDOW_CREATION_ERROR") || key.equals("NO_SCRIPT_MESSAGE")))
    {
      System.out.println("***nothing was rendered, but I have a key of " + key);
    }

  }
  ****/

  /**
   * Returns the name of the ResourceBundle for the Skin.
   */
  abstract protected String getBundleName();

  /**
   * Our renderers call this to get the icon. This returns a renderable
   * icon. (ReferenceIcons are resolved -- the real icon they point to is
   * returned)
   */
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
  public Icon getIcon(
    String  iconName,
    boolean resolveIcon
    )
  {
    if (iconName == null)
      throw new NullPointerException("Null iconName");

    Icon icon = (Icon)_icons.get(iconName);
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
   * Find the actual icon
   * @param refIcon a ReferenceIcon instance
   * @param referencedIconStack  The stack of reference icon names which have
   *          already been visited.  Used to detect circular dependencies.
   * @return icon which is resolved. i.e., it is not a ReferenceIcon.
   */
  private Icon _resolveReferenceIcon(
    ReferenceIcon refIcon,
    Stack         referencedIconStack)
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
      referencedIconStack = new Stack();

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
  private static boolean _stackContains(Stack stack, Object value)
  {
    if (stack == null)
      return false;

    return stack.contains(value);
  }

  /**
   * Registers an Icon for the specified icon name.
   * @param iconName  The name of the icon. Cannot be null.
   * @param icon      The Icon to register.
   * @throws NullPointerException if iconName is null.
   */
  synchronized public void registerIcon(
    String  iconName,
    Icon    icon
    )
  {
    if (iconName == null)
      throw new NullPointerException("Null iconName");

    _icons.put(iconName, icon);
  }

  /**
   * Registers a style sheet which defines extension-specific
   * styles.  The styles specified by this style sheet will be
   * merged with the Skin's own styles.  The full set
   * of styles can be obtained by calling getStyleSheetDocument().
   * @todo Is this even supported???
   * @param styleSheetName The name of the style sheet which
   *          defines the extension's styles.  This style sheet
   *          should be installed under the directory specified by
   *          Configuration.STYLES_DIRECTORY path.
   * @see #getStyleSheetDocument
   * @throws NullPointerException if styleSheetName is null.
   */
  public void registerStyleSheet(
    String styleSheetName
    )
  {
    if (styleSheetName == null)
      throw new NullPointerException("Null styleSheetName");

    if (_extensionStyleSheetNames == null)
      _extensionStyleSheetNames = new ArrayList();

    _extensionStyleSheetNames.add(styleSheetName);
  }

  /**
   * Returns the StyleSheetDocument object which defines all of the
   * styles for this Skin, including any styles that are
   * contributed by UIExtensions.
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
  public Object getProperty(Object key)
  {
    if (_properties == null)
      return null;

    return _properties.get(key);
  }

  /**
   * Sets a value for the specified property key.
   */
  synchronized public void setProperty(
    Object key,
    Object value
    )
  {
    if (_properties == null)
      _properties = new OptimisticHashMap();

    _properties.put(key, value);
  }

  // Checks to see whether any of our style sheets have been updated
  private boolean _checkStylesModified(
    StyleContext context
    )
  {
    boolean modified = false;

    if (_skinStyleSheet != null)
      modified = _skinStyleSheet.checkModified(context);

    // We also check all of the UIExtension style sheets even
    // if we already know that the skin's style sheet has been
    // modified.  We need to do this because we want to call
    // StyleSheetEntry.checkModified() for each entry - otherwise
    // out of date StyleSheetEntries may not get updated.
    if (_extensionStyleSheets != null)
    {
      for (int i = 0; i < _extensionStyleSheets.length; i++)
      {
        StyleSheetEntry entry = _extensionStyleSheets[i];
        if (entry.checkModified(context))
          modified = true;
      }
    }

    return modified;
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

      // =-=jmw I'm not sure where a good place is to parse the css file and
      // register the icons and properties. For now, I suppose I can do it here.

      if (styleSheetName != null)
      {
        _skinStyleSheet = StyleSheetEntry.createEntry(context, styleSheetName);


        // register the icons and properties if there are any.
        // this is a strange place for this. Where is a better spot???
        // get a List of IconNodes, and register them.
        if (_skinStyleSheet != null)
        {
          // register icons
          List icons = _skinStyleSheet.getIcons();
          if (icons != null)
          {
            Iterator e = icons.listIterator();
            while (e.hasNext())
            {
              IconNode iconNode = (IconNode)e.next();
              registerIcon(iconNode.getIconName(),
                           iconNode.getIcon());

            }
          }
          // register properties
          List skinProperties = _skinStyleSheet.getSkinProperties();
          if (skinProperties != null)
          {
            Iterator e = skinProperties.listIterator();
            while (e.hasNext())
            {
              SkinPropertyNode property = (SkinPropertyNode)e.next();
              Object propValueObj = property.getPropertyValue();
              // convert to a type if possible first
              // =-=jmw should I get the type for af|navigationPath-ora-show-last-item or just
              // -ora-show-last-item. in other words, will the property name be the same type
              // no matter which component it is being used for? If so, then
              // just storing -ora-show-last-item in the map is good enough.
              // For now, just for the heck of it, use af|navigationPath-ora-show-last-item

              String key = property.getPropertySelector() +
                           property.getPropertyName();
              // look up in map to get conversion
              Class type = (Class)_PROPERTY_CLASS_TYPE_MAP.get(key);
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


      // Now create entries for UIExtension-specific style sheets.
      _extensionStyleSheets = _getExtensionStyleSheets(context);
    }

    // Now merge all of the documents provided by all of our
    // entries into a single StyleSheetDocument.
    StyleSheetDocument document = null;

    if (_skinStyleSheet != null)
      document = _skinStyleSheet.getDocument();

    // Merge in any UIExtension style sheets on top of
    // the skin's style sheet
    if (_extensionStyleSheets != null)
    {
      for (int i = 0; i < _extensionStyleSheets.length; i++)
      {
        StyleSheetEntry entry = _extensionStyleSheets[i];
        StyleSheetDocument extensionDocument = entry.getDocument();

        if (extensionDocument != null)
        {
          // Merge the UIExtension's StyleSheetDocument on top of
          // the current StyleSheetDocument.  Note: This is not
          // exactly efficient - we would be better off creating
          // an array of StyleSheetDocuments and merging them all
          // in one pass.  But since this code should rarely be
          // executed, this shouldn't be a bottleneck...
          document = StyleSheetDocumentUtils.mergeStyleSheetDocuments(
                                               document,
                                               extensionDocument);
          // =-=jmw @todo when we have extension documents, we'll need
          // to register icons and skin properties from those on the skin?
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
    return new StyleSheetDocument(null, null);
  }

  // Gets the StyleSheetEntries for UIExtensions
  private StyleSheetEntry[] _getExtensionStyleSheets(StyleContext context)
  {
    if (_extensionStyleSheetNames == null)
      return null;

    // Create a list to hold our StyleSheetEntries
    int count = _extensionStyleSheetNames.size();
    ArrayList entries = new ArrayList(count);

    // Loop through all registered style sheet names and
    // try to create a StyleSheetEntry for each name.
    Iterator iter = _extensionStyleSheetNames.iterator();
    if (iter != null)
    {
      while (iter.hasNext())
      {
        String name = (String)iter.next();
        StyleSheetEntry entry = StyleSheetEntry.createEntry(context, name);
        if (entry != null)
          entries.add(entry);
      }
    }

    if (!entries.isEmpty())
    {
      _extensionStyleSheets = new StyleSheetEntry[entries.size()];
      return (StyleSheetEntry[])entries.toArray(_extensionStyleSheets);
    }

    return null;
  }

  // HashMap that maps icon name to Icons
  private OptimisticHashMap _icons = new OptimisticHashMap();

  // The StyleSheetDocument which contains all of the styles
  // for this Skin - including styles contributed by UIExtensions.
  private StyleSheetDocument _styleSheetDocument;

  // A StyleSheetEntry which defines the styles that are
  // provided by this Skin's style sheet only (does
  // not include UIExtension styles).
  private StyleSheetEntry _skinStyleSheet;

  // List of extension style sheet names
  private List _extensionStyleSheetNames;

  // Array of UIExtension StyleSheetEntries
  private StyleSheetEntry[] _extensionStyleSheets;

  // HashMap of Skin properties
  private OptimisticHashMap _properties;

  // Map of property to class type
  private static final Map _PROPERTY_CLASS_TYPE_MAP = new HashMap();
  static
  {
    _PROPERTY_CLASS_TYPE_MAP.put(
      XhtmlConstants.AF_NAVIGATIONPATH_SHOW_LAST_ITEM_PROPERTY_KEY, Boolean.class);
    _PROPERTY_CLASS_TYPE_MAP.put(
      XhtmlConstants.AF_TABLE_SELECTION_BAR_IN_TABLE, Boolean.class);
    _PROPERTY_CLASS_TYPE_MAP.put(
      XhtmlConstants.AF_TABLE_REPEAT_CONTROL_BAR, Boolean.class);
  }

  // Error messages
  private static final String _CIRCULAR_INCLUDE_ERROR =
    "Circular dependency detected in skin reference icon ";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(Skin.class);
}
