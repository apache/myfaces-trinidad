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

package org.apache.myfaces.trinidadinternal.style.xml.parse;

import java.awt.Color;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.Stack;
import java.util.Vector;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;
import java.util.Comparator;
import org.apache.myfaces.trinidad.util.IntegerUtils;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidad.context.LocaleContext;

import org.apache.myfaces.trinidadinternal.style.PropertyParseException;
import org.apache.myfaces.trinidadinternal.style.Style;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.UserStyleSheet;
import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;
import org.apache.myfaces.trinidadinternal.style.util.ModeUtils;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;


/**
 * Private implementation of StyleSheetDocument.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/StyleSheetDocument.java#0 $) $Date: 10-nov-2005.18:58:12 $
 * @author The Oracle ADF Faces Team
 */
public class StyleSheetDocument
{
  /**
   * Value used to indicate that the document timestamp is not know.
   */
  public static final long UNKNOWN_TIMESTAMP = -1;

  /**
   * Creates a StyleSheetDocument
   * @deprecated colorScheme elements are no longer supported
   */
  @Deprecated
  public StyleSheetDocument(
    StyleSheetNode[] styleSheets,
    ColorSchemeNode[] colorSchemes
    )
  {
    this (styleSheets, null, UNKNOWN_TIMESTAMP);
  }

  /**
   * Creates a StyleSheetDocument
   * @deprecated colorScheme elements are no longer supported
   */
  @Deprecated
  public StyleSheetDocument(
    StyleSheetNode[] styleSheets,
    ColorSchemeNode[] colorSchemes,
    String documentVersion
    )
  {
    this(styleSheets, documentVersion, UNKNOWN_TIMESTAMP);
  }

  /**
   * Creates a StyleSheetDocument
   * @param styleSheets The StyleSheetNodes which define the contents
   *          of this StyleSheetDocument.
   * @param documentVersion The version identifier for this StyleSheetDocument.
   * @param timestamp The timestamp for this StyleSheetDocument.
   */
  public StyleSheetDocument(
    StyleSheetNode[] styleSheets,
    String           documentVersion,
    long             documentTimestamp
    )
  {
    if (styleSheets != null)
    {
      _styleSheets = new StyleSheetNode[styleSheets.length];
      System.arraycopy(styleSheets, 0, _styleSheets, 0, styleSheets.length);
    }

    _documentVersion = documentVersion;
    _documentTimestamp = documentTimestamp;
  }

  /**
   * Returns the version identifier for this style sheet document.
   */
  public String getDocumentVersion()
  {
    return _documentVersion;
  }

  /**
   * Returns a timestamp which indicates when the underlying source
   * files which were used to create this StyleSheetDocument were
   * last modified.  The semantics of this method are slightly
   * different than the typical last modified timestamp (eg.
   * File.lastModified()) in that the timestamp is determined when
   * the StyleSheetDocument is created.  The StyleSheetDocument
   * itself represents the contents of a style sheet at a
   * particular moment in time - and does not detect later
   * updates to the underlying source files.
   */
  public long getDocumentTimestamp()
  {
    return _documentTimestamp;
  }

  /**
   * Implementation of StyleSheetDocument.getStyleSheets();
   */
  public Iterator<StyleSheetNode> getStyleSheets()
  {
    if(_styleSheets==null)
    {
      // -= Simon Lessard =- 
      // TODO: Collections.emptyList() maybe?
      return  (Arrays.asList(new StyleSheetNode[0])).iterator();
    }
    else
    {
      return (Arrays.asList(_styleSheets)).iterator();
    }
  }

  /**
   * Implementation of StyleSheetDocument.getColorSchemes();
   */
  public Iterator<ColorSchemeNode> getColorSchemes()
  {
    // -= Simon Lessard =- 
    // TODO: Wouldn't Collections.emptyList() be better?
    return (Arrays.asList(new ColorSchemeNode[0])).iterator();
  }

  /**
   * Returns all StyleSheetNodes which can be applied to the specified
   * context, sorted from lowest to highest precedence.
   */
  public Iterator<StyleSheetNode> getStyleSheets(StyleContext context)
  {
    // =-=ags Should this include the UserStyleSheet?
    if(_getStyleSheets(context,false)==null)
      return (Arrays.asList(new StyleSheetNode[0])).iterator();
    else
      return (Arrays.asList(_getStyleSheets(context, false))).iterator();
  }

  /**
   * Returns an Iterator of StyleNode objects for the specified context.
   */
  @SuppressWarnings("unchecked")
  public Iterator<StyleNode> getStyles(StyleContext context)
  {
    // Get the matching style sheets, including the UserStyleSheet
    StyleSheetNode[] styleSheets = _getStyleSheets(context, true);
    if (styleSheets == null)
      return EmptyIterator.getInstance();

    // We are going to loop through every StyleNode in every StyleSheetNode,
    // resolving each one along the way.  We store resolved StyleNodes in
    // a Vector, so that the generated CSS somewhat matches the order that
    // the style elements appear in the XSS document.
    ArrayList<StyleNode> styles = new ArrayList<StyleNode>();

    // We also need to provide a Map for storing selector-based
    // styles and another for storing name-based styles, used by
    // _resolveStyle() to store results
    HashMap<String, StyleNode> resolvedStyles = 
      new HashMap<String, StyleNode>();
    HashMap<String, StyleNode> resolvedNamedStyles = 
      new HashMap<String, StyleNode>();

    // Now, loop through all StyleNodes in all StyleSheetNodes
    // Note: The algorithm used here is actually much more inefficient
    // than it needs to be.  We're using a n-squared algorithm here -
    // for every style we find, we need to loop through all other styles.
    // The reason for this is because this allows us to share code
    // (namely _resolveStyle()) with getStyleBySelector/Name().  However,
    // we could probably double the performance of CSS generation by
    // optimizing this routine to make a single pass through all styles,
    // and then resolve everything in one step.  (Currently, it takes
    // about 100ms to generate a CSS version of blaf.xss - we could reduce
    // this to about 50ms if needed.)
    for (int i = 0; i < styleSheets.length; i++)
    {
      StyleSheetNode styleSheet = styleSheets[i];
      Iterator<StyleNode> e = styleSheet.getStyles();
      if (e != null)
      {
        while (e.hasNext())
        {
          StyleNode node = e.next();
          String id = null;
          boolean isNamed = false;

          if (node.getName() != null)
          {
            id = node.getName();
            isNamed = true;
          }
          else
          {
            id = node.getSelector();
          }

          StyleNode resolvedNode = _resolveStyle(context,
                                                 styleSheets,
                                                 resolvedStyles,
                                                 resolvedNamedStyles,
                                                 null,
                                                 null,
                                                 id,
                                                 isNamed);

          // If we got a node, add it in to our list
          if (resolvedNode != null)
          {

            // cache already resolved styles so we don't
            // resolve them again. This saves time.

            String namedStyle = resolvedNode.getName();
            String selectorStyle = resolvedNode.getSelector();
            if (namedStyle != null)
            {
              resolvedNamedStyles.put(namedStyle, resolvedNode);
            }
            else if (selectorStyle != null)
            {
              resolvedStyles.put(selectorStyle, resolvedNode);
            }


            // add it to our list

            if (!_containsStyle(styles, resolvedNode))
              styles.add(resolvedNode);
          }
        }
      }
    }

    return styles.iterator();
  }

  /**
   * Returns the fully-resolved StyleNode for the style with the
   * specified selector.
   */
  public StyleNode getStyleBySelector(
    StyleContext context,
    String       selector
    )
  {
    return _getStyle(context, selector, false);
  }

  /**
   * Returns the fully-resolved StyleNode for the style with the
   * specified name.
   */
  public StyleNode getStyleByName(
    StyleContext context,
    String       name
    )
  {
    return _getStyle(context, name, true);
  }

  // Returns array of matching style sheets
  private StyleSheetNode[] _getStyleSheets(
    StyleContext context,
    boolean      includeUserStyleSheet
    )
  {
    LocaleContext localeContext = context.getLocaleContext();
    Locale locale = localeContext.getTranslationLocale();
    int direction = LocaleUtils.getReadingDirection(localeContext);
    int mode = NameUtils.getMode(ModeUtils.getCurrentMode(context));
    TrinidadAgent agent = context.getAgent();

    // -= Simon Lessard =- 
    // TODO: Check if synchronization is truly required    
    Vector<StyleSheetNode> v = new Vector<StyleSheetNode>(); // Vector of matching style sheets
    Iterator<StyleSheetNode> e = getStyleSheets();  // Enum of all style sheets

    // Loop through the style sheets, storing matches in the Vector
    while (e.hasNext())
    {
      StyleSheetNode styleSheet = e.next();

      if (styleSheet.compareVariants(locale, direction, agent, mode) > 0)
        v.addElement(styleSheet);
    }

    int count = v.size();
    if (count == 0)
      return null;

    // Sort the matching style sheets by specificity
    StyleSheetNode[] styleSheets = new StyleSheetNode[count];
    v.copyInto(styleSheets);
    Comparator<StyleSheetNode> comparator = 
      new StyleSheetComparator(locale,
                               direction,
                               agent,
                               mode,
                               _styleSheets);

    Arrays.sort(styleSheets, comparator);


    // If we've got a UserStyleSheet, convert it to a StyleSheetNode and
    // add it to the end of the list (highest precedence)
    if (includeUserStyleSheet)
    {
      UserStyleSheet userStyleSheet =
        UserStyleSheet.getUserStyleSheet(context);
      if (userStyleSheet != null)
      {
        StyleSheetNode[] tmpSheets =
          new StyleSheetNode[styleSheets.length + 1];
        System.arraycopy(styleSheets, 0, tmpSheets, 0, styleSheets.length);
        tmpSheets[tmpSheets.length - 1] =
          _createStyleSheetNode(userStyleSheet);

        styleSheets = tmpSheets;
      }
    }

    return styleSheets;
  }

  // Gets the style with the specified selector/name
  private StyleNode _getStyle(
    StyleContext context,
    String       id,
    boolean      isNamed
    )
  {
    StyleSheetNode[] styleSheets = _getStyleSheets(context, true);
    if (styleSheets == null)
      return null;

    return _resolveStyle(context,
                         styleSheets,
                         new HashMap<String, StyleNode>(19),  // Resolved styles
                         new HashMap<String, StyleNode>(19),  // Resolved named styles
                         null,               // Include stack
                         null,               // Named include stack
                         id,
                         isNamed);
  }

  /**
   * Resolves the (named or selector-based) style with the specified id.
   * @param context The StyleContext
   * @param styleSheets The StyleSheetNodes to use for resolving the style,
   *          sorted from lowest to highest precedence.
   * @param resolvedStyles The set of already resolved styles, hashed by
   *          selector
   * @param resolvedNamedStyles The set of already resolved named styles,
   *          hashed by name.
   * @param includesStack The stack of included style selectors which have
   *          already been visited.  Used to detect circular dependencies.
   * @param namedIncludesStack The stack of included style names which have
   *          already been visited.  Used to detect circular dependencies.
   * @param id The selector or name of the style to resolve
   * @param isNamed A boolean indicating whether the id is a name or selector.
   * @return Returns the fully resolved StyleNode, or null if the style
   *           could not be resolved.  Also, as a side-effect, the
   *           resolved style is stored in the appropriate resolved style
   *           Map.
   */
  private StyleNode _resolveStyle(
    StyleContext           context,
    StyleSheetNode[]       styleSheets,
    Map<String, StyleNode> resolvedStyles,
    Map<String, StyleNode> resolvedNamedStyles,
    Stack<String>          includesStack,
    Stack<String>          namedIncludesStack,
    String                 id,
    boolean                isNamed
    )
  {
    assert (styleSheets != null);
    assert (resolvedStyles != null);
    assert (resolvedNamedStyles != null);
    assert (id != null);

    // First, let's check to see if we've already got a StyleNode for this id
    StyleNode style = null;
    String selector = null;
    String name = null;


    if (isNamed)
    {
      style = resolvedNamedStyles.get(id);
      name = id;
    }
    else
    {
      style = resolvedStyles.get(id);
      selector = id;
    }

    // If we've already got a style, return it and we're done!
    if (style != null)
    {
      // We use _ERROR_STYLE_NODE for internal error tracking, but we
      // never return it - return null instead.
      if (style == _ERROR_STYLE_NODE)
        return null;

      return style;
    }

    // Next, make sure we don't have a circular dependency
    if ((isNamed && _stackContains(namedIncludesStack, id)) ||
        (!isNamed && _stackContains(includesStack, id)))
    {
      if (_LOG.isWarning())
        _LOG.warning(_CIRCULAR_INCLUDE_ERROR + id);
      return null;
    }

    // Create the StyleEntry that we're going to use to store properties
    // as we are resolving this style
    StyleEntry entry = new StyleEntry(selector, name);

    // Push this style onto the appropriate include stack
    if (isNamed)
    {
      // -= Simon Lessard =- 
      // TODO: Check if synchronization is truly required
      if (namedIncludesStack == null)
        namedIncludesStack = new Stack<String>();

      namedIncludesStack.push(id);
    }
    else
    {
      // -= Simon Lessard =- 
      // TODO: Check if synchronization is truly required
      if (includesStack == null)
        includesStack = new Stack<String>();

      includesStack.push(id);
    }

    // Now, we loop through all of the StyleSheetNodes and collect
    // properties defined by matching StyleNodes.  We resolve any
    // included styles along the way.
    for (int i = 0; i < styleSheets.length; i++)
    {
      Iterator<StyleNode> nodes = styleSheets[i].getStyles();
      if (nodes != null)
      {
        while (nodes.hasNext())
        {
          StyleNode node = nodes.next();

          if ((isNamed && name.equals(node.getName())) ||
               (!isNamed && selector.equals(node.getSelector())))
          {
            // We've got a match!  We need to do the following:
            // 0. Check to see whether we need to reset our properties.
            // 1. Resolve any included styles, and shove those properties
            //    into our StyleEntry.
            // 2. Resolve any included properties, and shove those properties
            //    into our StyleEntry.
            // 3. Resolve all compound properites, and shove those properties
            //    into the StyleEntry
            // 4. Remove all properties that were inhibited.
            // 5. Shove all properties from the matching StyleNode into our
            //    StyleEntry, overwriting included values
            // -= Simon Lessard =-
            // FIXME: That sequence looks buggy. If more than 1 matching node 
            //        is found, then the included properties of the second will
            //        have priority over the properties found at step 5 on the
            //        first node, which is most likely incorrect.
            //
            //        A possible fix would be to put entries from the 5 steps 
            //        into 5 different lists then resolve all priorities at the 
            //        end.
            
            // 0. Reset properties?
            if (node.__getResetProperties() || node.isInhibitingAll())
              entry.resetProperties();

            // 1. Resolve included styles
            Iterator<IncludeStyleNode> includedStyles = node.getIncludedStyles();
            if (includedStyles != null)
            {
              while (includedStyles.hasNext())
              {
                IncludeStyleNode includeStyle = includedStyles.next();
                String includeID = null;
                boolean includeIsNamed = false;

                if (includeStyle.getName() != null)
                {
                  includeID = includeStyle.getName();
                  includeIsNamed = true;
                }
                else
                {
                  includeID = includeStyle.getSelector();
                }

                StyleNode resolvedNode = _resolveStyle(context,
                                                       styleSheets,
                                                       resolvedStyles,
                                                       resolvedNamedStyles,
                                                       includesStack,
                                                       namedIncludesStack,
                                                       includeID,
                                                       includeIsNamed);

                if (resolvedNode != null)
                  _addIncludedProperties(entry, resolvedNode);
              }
            }

            // 2. Resolve included properties
            Iterator<IncludePropertyNode> includedProperties = 
              node.getIncludedProperties();
            if (includedProperties != null)
            {
              while (includedProperties.hasNext())
              {
                IncludePropertyNode includeProperty = includedProperties.next();
                String includeID = null;
                boolean includeIsNamed = false;

                if (includeProperty.getName() != null)
                {
                  includeID = includeProperty.getName();
                  includeIsNamed = true;
                }
                else
                {
                  includeID = includeProperty.getSelector();
                }

                StyleNode resolvedNode = _resolveStyle(context,
                                                       styleSheets,
                                                       resolvedStyles,
                                                       resolvedNamedStyles,
                                                       includesStack,
                                                       namedIncludesStack,
                                                       includeID,
                                                       includeIsNamed);

                if (resolvedNode != null)
                {
                  _addIncludedProperty(entry,
                                       resolvedNode,
                                       includeProperty.getPropertyName(),
                                       includeProperty.getLocalPropertyName());
                }
              }
            }

            // 3. Add compound properties
            Iterator<CompoundPropertyNode> compoundProperties = 
              node.getCompoundProperties();
            
            if (compoundProperties != null)
            {
              while (compoundProperties.hasNext())
              {
                CompoundPropertyNode compoundProperty = compoundProperties.next();

                // Build up the value String for the compound property
                StringBuffer buffer = new StringBuffer();
                Iterator<Object> values = compoundProperty.getValues();
                if (values != null)
                {
                  while (values.hasNext())
                  {
                    Object o = values.next();

                    if (buffer.length() != 0)
                      buffer.append(' ');

                    if (o instanceof String)
                      buffer.append((String)o);
                    else if (o instanceof IncludePropertyNode)
                    {
                      IncludePropertyNode includeProperty =
                        (IncludePropertyNode)o;

                      String includeID = null;
                      boolean includeIsNamed = false;

                      if (includeProperty.getName() != null)
                      {
                        includeID = includeProperty.getName();
                        includeIsNamed = true;
                      }
                      else
                      {
                        includeID = includeProperty.getSelector();
                      }

                      StyleNode resolvedStyle =
                        _resolveStyle(context,
                                      styleSheets,
                                      resolvedStyles,
                                      resolvedNamedStyles,
                                      includesStack,
                                      namedIncludesStack,
                                      includeID,
                                      includeIsNamed);

                      if (resolvedStyle != null)
                      {
                        String value = _getPropertyValue(resolvedStyle,
                                            includeProperty.getPropertyName());
                        if (value != null)
                          buffer.append(value);
                      }

                    }
                    else
                    {
                      assert false;
                    }
                  }
                }

                entry.addProperty(new PropertyNode(compoundProperty.getName(),
                                                   buffer.toString()));
              }
            }

            // 4. Check inhibited properties
            Iterator<String> inhibitedProperties = node.getInhibitedProperties();
            while (inhibitedProperties.hasNext())
            {
              entry.removeProperty(inhibitedProperties.next());
            }
            

            // 5. Add non-included properties
            Iterator<PropertyNode> properties = node.getProperties();
            if (properties != null)
            {
              while (properties.hasNext())
                entry.addProperty(properties.next());
            }
          }
        }
      }
    }

    // Pop the include stack
    if (isNamed)
    {
      namedIncludesStack.pop();
    }
    else
    {
      includesStack.pop();
    }

    // Convert the StyleEntry to a StyleNode and return it
    return entry.toStyleNode();
  }

  // Adds all of the properties from the StyleNode into the StyleEntry
  // as included properties.
  private void _addIncludedProperties(
    StyleEntry entry,
    StyleNode  node
    )
  {
    if (node == null)
      return;

    Iterator<PropertyNode> properties = node.getProperties();
    if (properties != null)
    {
      while (properties.hasNext())
        entry.addIncludedProperty(properties.next());
    }
  }

  // Adds the specified property from the StyleNode into the StyleEntry.
  private void _addIncludedProperty(
    StyleEntry entry,
    StyleNode  node,
    String     propertyName,
    String     localPropertyName
    )
  {
    if (node == null)
      return;

    Iterator<PropertyNode> properties = node.getProperties();
    if (properties != null)
    {
      while (properties.hasNext())
      {
        PropertyNode property = properties.next();
        if (propertyName.equals(property.getName()))
        {
          if (!propertyName.equals(localPropertyName))
          {
            property = new PropertyNode(localPropertyName,
                                        property.getValue());
          }

          entry.addIncludedProperty(property);
        }
      }
    }
  }

  // Returns a count of the non-null items in the Vector
  private static int _getNonNullCount(ArrayList<?> list)
  {
    if (list == null)
      return 0;

    int count = 0;
    for (int i = 0; i < list.size(); i++)
    {
      if (list.get(i) != null)
        count++;
    }

    return count;
  }

  // Static utility method used by StyleEntry & FontSizeConverter
  // to get the real font size, taking relative size into account
  static PropertyNode _getRealFontSize(
    PropertyNode property,
    int relativeFontSize
    )
  {
    if (relativeFontSize == 0)
      return property;

    String value = property.getValue();

    // Strip off units
    String units = _POINT_UNITS;

    if (value.endsWith(_POINT_UNITS))
    {
      value = value.substring(0, value.length() - _POINT_UNITS.length());
      units = _POINT_UNITS;
    }
    else if (value.endsWith(_PIXEL_UNITS))
    {
      value = value.substring(0, value.length() - _PIXEL_UNITS.length());
      units = _PIXEL_UNITS;
    }

    int size = 0;

    try
    {
      size = Integer.parseInt(value);
    }
    catch (NumberFormatException e)
    {
      assert false:"Could not parse font size: " + value;

      return property;
    }

    size += relativeFontSize;

    String newValue = IntegerUtils.getString(size) + units;
    return new PropertyNode(_FONT_SIZE_NAME, newValue);
  }

  // Returns the value of the property with the specified name
  private String _getPropertyValue(StyleNode style, String propertyName)
  {
    Iterator<PropertyNode> properties = style.getProperties();
    if (properties != null)
    {
      while (properties.hasNext())
      {
        PropertyNode property = properties.next();
        if (propertyName.equals(property.getName()))
          return property.getValue();
      }
    }

    return null;
  }

  // Coverts a UserStyleSheet into a StyleSheetNode
  private StyleSheetNode _createStyleSheetNode(UserStyleSheet userStyleSheet)
  {
    // Convert each Style in the userStyleSheet to a StyleNode
    // -= Simon Lessard =- 
    // TODO: Check if synchronization is truly required
    Vector<StyleNode> v = new Vector<StyleNode>();

    // First, add the selector-based styles
    Iterator<Object> selectors = userStyleSheet.getSelectors();
    while (selectors.hasNext())
    {
      String selector = (String)selectors.next();
      Style style = userStyleSheet.getStyle(selector);

      v.addElement(_createStyleNode(selector, style, false));
    }

    // Now, add in the named styles
    Iterator<Object> names = userStyleSheet.getNames();
    while (names.hasNext())
    {
      String name = (String)names.next();
      Style style = userStyleSheet.getNamedStyle(name);

      v.addElement(_createStyleNode(name, style, true));
    }

    StyleNode[] nodes = new StyleNode[v.size()];
    v.copyInto(nodes);

    return new StyleSheetNode(nodes, // The StyleNodes
                              null,  // locales - we don't care
                              LocaleUtils.DIRECTION_DEFAULT,  // direction
                              null,  // browsers - we don't care
                              null,  // versions - we don't care
                              null,   // platforms - we don't care
                              ModeUtils.MODE_DEFAULT);
  }

  // Creates a StyleNode for the specified Style.  The key is either a
  // selector or a name, depending on the value of the isNamed parameter.
  private StyleNode _createStyleNode(String key, Style style, boolean isNamed)
  {
    // Covert the properties into PropertyNodes
    // -= Simon Lessard =- 
    // TODO: Check if synchronization is truly required
    Vector<PropertyNode> v = new Vector<PropertyNode>();
    Iterator<Object> names = style.getPropertyNames();

    while (names.hasNext())
    {
      String name = (String)names.next();
      String value = style.getProperty(name);

      v.addElement(new PropertyNode(name, value));
    }

    PropertyNode[] nodes = new PropertyNode[v.size()];
    v.copyInto(nodes);

    if (isNamed)
    {
      return new StyleNode(key, null, nodes, null, null, null, null);
    }
    
    return new StyleNode(null, key, nodes, null, null, null, null);
  }

  // Tests whether the value is present in the (possibly null) stack.
  private static boolean _stackContains(Stack<?> stack, Object value)
  {
    if (stack == null)
      return false;

    return stack.contains(value);
  }

  // Tests whether the value is present in the (possibly null) stack.
  private static boolean _containsStyle(List<StyleNode> v, StyleNode node)
  {
    String id = null;
    boolean isNamed = false;

    if (node.getName() != null)
    {
      id = node.getName();
      isNamed = true;
    }
    else
    {
      id = node.getSelector();
    }
    
    for(StyleNode otherNode : v)
    {
      if ((isNamed && id.equals(otherNode.getName())) ||
           (!isNamed && id.equals(otherNode.getSelector())))
        return true;
    }

    return false;
  }

  // Comparator for StyleSheetNodes which sorts by variant specificity
  private static class StyleSheetComparator implements Comparator<StyleSheetNode>
  {
    public StyleSheetComparator(
      Locale locale,
      int direction,
      TrinidadAgent agent,
      int mode,
      StyleSheetNode[] styleSheets
      )
    {
      _direction = direction;
      _locale = locale;
      _agent = agent;
      _styleSheets = styleSheets;
      _mode = mode;
    }

    public int compare(StyleSheetNode item1, StyleSheetNode item2)
    {
      if (item1 == item2)
        return 0;

      int match1 = item1.compareVariants(_locale, 
                                         _direction, 
                                         _agent, 
                                         _mode);
      
      int match2 = item2.compareVariants(_locale, 
                                         _direction, 
                                         _agent, 
                                         _mode);

      if (match1 == match2)
      {
        return _compareOrder(item1, item2);
      }

      if (match1 < match2)
        return -1;

      return 1;
    }

    private int _compareOrder(Object item1, Object item2)
    {
      assert (item1 != item2);

      for (int i = 0; i < _styleSheets.length; i++)
      {
        StyleSheetNode styleSheet = _styleSheets[i];
        if (styleSheet == item1)
          return -1;
        if (styleSheet == item2)
          return 1;
      }

      // Huh?  This should never happen
      assert false;

      return 0;
    }

    private Locale _locale;
    private int    _direction;
    private TrinidadAgent  _agent;
    private int _mode;
    // We use the style sheet node array to determine the
    // precedence of two stylesheets with the same attributes.
    // Later style sheets take precedence over earlier ones
    private StyleSheetNode[] _styleSheets;
  }

  // Private style class that we use when we're building up
  // our list of styles for a particular end user environment.
  private static class StyleEntry
  {
    // The selector of the style
    public final String selector;

    // The name of the style
    public final String name;

    // Empty private constructor - this exists only to prevent
    // the compiler from complaining about our blank final vars
    // It should never be called.
    private StyleEntry()
    {
      assert false;
      selector = null;
      name = null;
    }

    // Create a StyleEntry with the specified selector, name
    public StyleEntry(String selector, String name)
    {
      this.selector = selector;
      this.name = name;
    }

    // Add the specified property.
    // If we already have a PropertyNode with the same name as the
    // property, we remove the old value, as newly added properties
    // take precedence.
    public void addProperty(PropertyNode property)
    {
      if (_properties == null)
        _properties = new ArrayList<PropertyNode>(5);


      // Relative font sizes are a special case - they get added to
      // the _relativeFontSize value instead of to the _properties list.
      if (_isRelativeFontSize(property))
      {
        _addRelativeFontSize(property);
      }
      else if (_isRelativeColor(property))
      {
        _addRelativeColor(property);
      }
      else
      {
        // Remove the old property value before adding the new value
        String name = property.getName();
        removeProperty(name);

        _properties.add(property);
        _propertyCount++;

        // If we are setting an absolute font size, we override
        // any relative sizes that have already been specified
        if (name.equals(_FONT_SIZE_NAME))
          _relativeFontSize = 0;
      }
    }

    // Removes the property with the specified name
    public void removeProperty(String name)
    {
      if (_removeProperty(_properties, name))
        _propertyCount--;
    }

    // Clears out all properties
    public void resetProperties()
    {
      _properties = null;
      _propertyCount = 0;
      _relativeFontSize = 0;
    }

    // Returns the count of properties defined by this style
    public int getPropertyCount()
    {
      return _propertyCount;
    }

    // Returns an Iterator of the properties defined by this style
    public Iterator<PropertyNode> getProperties()
    {
      if (_properties == null)
        return null;

      return new
        FontSizeConverter(
          new NonNullIterator<PropertyNode>(
            _properties.iterator()),
          _relativeFontSize);
    }

    // Adds an "included" property.  Include propreties are
    // properties which are indirectly included in the style
    // via an <includeStyle> element.
    public void addIncludedProperty(PropertyNode property)
    {
      // We no longer track included properties separately.
      // The reason for this is that it should be possible
      // to override a <property> value using an included
      // property in a custom style sheet.  (See bug 2896824.)
      // So, we just add included properties to the main
      // property list.
      addProperty(property);
    }

    // Converts this StyleEntry to a StyleNode
    public StyleNode toStyleNode()
    {
      // Create an PropertyNode array to pass to the new StyleNode.
      // The PropertyNode array includes both the included properties
      // and the properties defined directly within this style.
      int count = _getNonNullCount(_properties);

      if (count == 0)
        return null;

      PropertyNode[] properties = new PropertyNode[count];

      // Copy in the properties
      _nonNullCopyInto(_properties, properties, 0);

      // Adjust the font size based for relative font
      if (_relativeFontSize != 0)
      {
        for (int i = 0; i < properties.length; i++)
        {
          PropertyNode property = properties[i];
          if (_FONT_SIZE_NAME.equals(property.getName()))
          {
            properties[i] = _getRealFontSize(property, _relativeFontSize);
            break;
          }
        }
      }

      // Create and return our StyleNode.  We don't need to specify
      // a name or included styles, as they have already been resolved.
      return new StyleNode(name, selector, properties, null, null, null, null);
    }

    // Tests whether a property with the specified name is
    // contained within the Vector of PropertyNodes
    // -= Simon Lessard =-
    // FIXME: Never used locally as of 2006-08-04
    @SuppressWarnings("unused")
    private boolean _containsProperty(
        ArrayList<PropertyNode> properties, 
        String name)
    {
      if (properties == null)
        return false;
      
      for(PropertyNode property : properties)
      {
        if ((property != null) && (name.equals(property.getName())))
          return true;
      }

      return false;
    }

    // Removes the PropertyNode with the specified name from the
    // Vector of properties.  Note - we assume that the properties
    // Vector will contain at most one property with the specified
    // name.  Returns a boolean indicating whether the specified
    // property was found (and thus removed).
    private boolean _removeProperty(
        ArrayList<PropertyNode> properties, 
        String name)
    {
      if (properties == null)
        return false;

      for (int i = 0; i < properties.size(); i++)
      {
        PropertyNode property = properties.get(i);

        if ((property != null) && property.getName().equals(name))
        {
          // We don't actually remove the old value, just
          // null it out.  We do this to avoid the calls
          // to System.arraycopy() that would occur if we
          // actually removed the property.  For us, time
          // is more important than memory usage.
          properties.set(i, null);
          return true;
        }
      }

      return false;
    }

    // Copies the non-null entries from the source vector to the
    // target Object array, starting at the specified index
    private void _nonNullCopyInto(
        ArrayList<? extends Object> source, 
        Object[] target, 
        int start)
    {
      if (source == null)
        return;

      for (int i = 0; i < source.size(); i++)
      {
        Object o = source.get(i);

        if (o != null)
          target[start++] = o;
      }
    }

    // Is the property a relative font size?
    private boolean _isRelativeFontSize(PropertyNode property)
    {
      if (!_FONT_SIZE_NAME.equals(property.getName()))
        return false;

      String value = property.getValue();
      if ((value != null) && value.length() > 0)
      {
        char c = value.charAt(0);
        return ((c == '+') || (c == '-'));
      }

      return false;
    }

    // Is the property a relative color?
    private boolean _isRelativeColor(PropertyNode property)
    {
      // Relative colors start with "-/+#"
      String value = property.getValue();

      if (value == null)
        return false;

      int length = value.length();

      // Length should either be 8 ("+#RRGGBB") or 5 ("+#RGB")
      if ((length != 8) && (length != 5))
        return false;

      char c0 = value.charAt(0);

      return (((c0 == '+') || (c0 == '-')) && (value.charAt(1) == '#'));
    }

    // Add the relative font size into _relativeFontSize
    private void _addRelativeFontSize(PropertyNode property)
    {
      assert (_isRelativeFontSize(property));

      String value = property.getValue();
      boolean increment = (value.charAt(0) == '+');

      // Rip off the +/- and units
      if (value.endsWith(_POINT_UNITS))
        value = value.substring(1, value.length() - _POINT_UNITS.length());
      else if (value.endsWith(_PIXEL_UNITS))
        value = value.substring(1, value.length() - _PIXEL_UNITS.length());
      else
        value = value.substring(1);

      int size = 0;

      try
      {
        size = Integer.parseInt(value);
      }
      catch (NumberFormatException e)
      {
        return;
      }

      if (increment)
        _relativeFontSize += size;
      else
        _relativeFontSize -= size;
    }

    // Add the relative color property
    private void _addRelativeColor(PropertyNode property)
    {
      assert (_isRelativeColor(property));

      String relativeValue = property.getValue();
      boolean increment = (relativeValue.charAt(0) == '+');

      Color relativeColor = null;

      try
      {
        // Note CSS-specific parsing code here!
        relativeColor = CSSUtils.parseColor(relativeValue.substring(1));
      }
      catch (PropertyParseException e)
      {
        // This should have been logged when the document was first parsed.
        ;
      }

      if (relativeColor == null)
      {
        return;
      }

      // Now, get the current absolute value
      String absoluteValue = _getPropertyValue(property.getName());

      if (absoluteValue == null)
        return;

      // Try parsing the absolute value into a Color
      Color absoluteColor = null;

      try
      {
        // Note CSS-specific parsing code here!
        absoluteColor = CSSUtils.parseColor(absoluteValue);
      }
      catch (PropertyParseException e)
      {
        // This should have been logged when the document was first parsed.
        ;
      }

      if (absoluteColor == null)
        return;

      // Apply the relative value to the absolute value
      int red = absoluteColor.getRed();
      int green = absoluteColor.getGreen();
      int blue = absoluteColor.getBlue();

      if (increment)
      {
        red = Math.min((red + relativeColor.getRed()), 255);
        green = Math.min((green + relativeColor.getGreen()), 255);
        blue = Math.min((blue + relativeColor.getBlue()), 255);
      }
      else
      {
        red = Math.max((red - relativeColor.getRed()), 0);
        green = Math.max((green - relativeColor.getGreen()), 0);
        blue = Math.max((blue - relativeColor.getBlue()), 0);
      }

      // Convert the resolved color values into a #RRGGBB string
      Color resolvedColor = new Color(red, green, blue);
      String resolvedValue = CSSUtils.getColorValue(resolvedColor);

      // Finally, add in the resolved property node
      addProperty(new PropertyNode(property.getName(), resolvedValue));
    }

    // Returns the String value for the specified property, or null
    // if no value has been added
    private String _getPropertyValue(String name)
    {
      if (_properties != null)
      {
        for(PropertyNode property : _properties)
        {
          if ((property != null) && (name.equals(property.getName())))
            return property.getValue();
        }
      }

      return null;
    }

    // The set of properties (PropertyNodes) defined by this style
    private ArrayList<PropertyNode> _properties;

    // We keep count of the number of non-null values in each vector
    private int _propertyCount;

    // _relativeFontSize accumulates the total relative font size
    // from any "font-size" properties with relative values.
    private int _relativeFontSize;
  }

  // Private Iterator implementation which strips null values
  // from a wrapped Iterator.  StyleEntry uses this to avoid
  // exposing null properties which result from removal of duplicate
  // properties (really, nulling out of duplicate properties).
  private static class NonNullIterator<T> implements Iterator<T>
  {
    public NonNullIterator(Iterator<T> wrappedIterator)
    {
      _wrappedIterator = wrappedIterator;

      // Initialize the cache next element
      _next = _getNonNullNext();
    }

    public boolean hasNext()
    {
      return (_next != null);
    }

    public T next()
    {
      T next = _next;
      _next = _getNonNullNext();

      return next;
    }

    public void remove()
    {
      _wrappedIterator.remove();
    }

    // Returns the next non null value in the wrapped enum
    private T _getNonNullNext()
    {
      while (_wrappedIterator.hasNext())
      {
        T next = _wrappedIterator.next();

        if (next != null)
          return next;
      }

      return null;
    }

    // The wrapped enumeration
    private Iterator<T> _wrappedIterator;

    // The next non-null value in the wrapped enumeration
    private T _next;
  }

  // Iterator implementation for empty set
  private static class EmptyIterator<T> implements Iterator<T>
  {
    private EmptyIterator() {}

    public static Iterator<StyleNode> getInstance()
    {
      if (_sInstance == null)
        _sInstance = new EmptyIterator<StyleNode>();

      return _sInstance;
    }

    public boolean hasNext()
    {
      return false;
    }

    public T next()
    {
      throw new NoSuchElementException();
    }

    public void remove()
    {
      throw new UnsupportedOperationException();
    }

    private static Iterator<StyleNode> _sInstance;
  }


  // A silly Iterator which converts "font-size" PropertyNodes to
  // absolute values, using a specified relative font size
  private static class FontSizeConverter implements Iterator<PropertyNode>
  {
    public FontSizeConverter(
      Iterator<PropertyNode> wrappedIterator,
      int relativeFontSize
      )
    {
      _wrappedIterator = wrappedIterator;
      _relativeFontSize = relativeFontSize;
    }

    public boolean hasNext()
    {
      return _wrappedIterator.hasNext();
    }

    public void remove()
    {
      _wrappedIterator.remove();
    }

    public PropertyNode next()
    {
      PropertyNode property = _wrappedIterator.next();

      if ((_relativeFontSize == 0) ||
           !_FONT_SIZE_NAME.equals(property.getName()))
      {
        return property;
      }

      return _getRealFontSize(property, _relativeFontSize);
    }

    // The wrapped enumeration
    private Iterator<PropertyNode> _wrappedIterator;
    private int         _relativeFontSize;
  }

  private StyleSheetNode[] _styleSheets;
  private String           _documentVersion;
  private long             _documentTimestamp;

  static final String _FONT_SIZE_NAME = "font-size";

  // Unit strings.  For now, we only support points and pixels
  // for relative font sizes.
  static final String _POINT_UNITS    = "pt";
  static final String _PIXEL_UNITS    = "px";

  // A StyleNode used as a placeholder for a style which couldn't be resolved
  private static final StyleNode _ERROR_STYLE_NODE = new StyleNode("error",
                                                                   "error",
                                                                   null,
                                                                   null,
                                                                   null,
                                                                   null,
                                                                   null);

  // Error messages
  private static final String _CIRCULAR_INCLUDE_ERROR =
    "Circular dependency detected in style ";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StyleSheetDocument.class);
}
