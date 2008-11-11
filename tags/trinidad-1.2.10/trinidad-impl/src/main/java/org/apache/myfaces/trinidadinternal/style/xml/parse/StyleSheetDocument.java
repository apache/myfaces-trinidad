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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

import java.awt.Color;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.Stack;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.LocaleContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.IntegerUtils;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.style.PropertyParseException;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;
import org.apache.myfaces.trinidadinternal.style.util.ModeUtils;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;


/**
 * Parsed representation of a Trinidad style sheet document.
 * 
 * The StyleSheetDocument provides access to both style as well as icons
 * information, but not to skin properties.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/StyleSheetDocument.java#0 $) $Date: 10-nov-2005.18:58:12 $
 */
public class StyleSheetDocument
{
  /**
   * Value used to indicate that the document timestamp is not know.
   */
  public static final long UNKNOWN_TIMESTAMP = -1;

  /**
   * Creates a StyleSheetDocument
   */
  @Deprecated
  public StyleSheetDocument(
    StyleSheetNode[] styleSheets
    )
  {
    this (styleSheets, null, UNKNOWN_TIMESTAMP);
  }

  /**
   * Creates a StyleSheetDocument
   */
  @Deprecated
  public StyleSheetDocument(
    StyleSheetNode[] styleSheets,
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
   * @param documentTimestamp The timestamp for this StyleSheetDocument.
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
   * Returns the StyleSheetDocument's id.
   * StyleSheetDocument contains one or more StyleSheetNodes. 
   * StyleSheetNodes contain StyleNodes & locale, browser, direction, etc.
   * We compute the hashcode of the StyleSheetDocument's styleSheetNodes(StyleContext) and
   * return that as a document id. One use of this id is to use it in the Skin's generated
   * CSS filename so the filename will change if the Skin's StyleSheetNodes change.
   * @param sContext The current StyleContext which is used to get the stylesheets that match,
   * and then we create an id based on those stylesheets
   * @return String The document Id as a String.
   */
  public String getDocumentId(StyleContext sContext)
  {
    int hashCode = 17;
    Iterator<StyleSheetNode> styleSheetNodes = getStyleSheets(sContext);
    while (styleSheetNodes.hasNext())
    {
      hashCode = hashCode*37 +  styleSheetNodes.next().getStyleSheetId();      
    }
    return Integer.toString(Math.abs(hashCode), 36);
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
   * Returns all StyleSheetNodes which can be applied to the specified
   * context, sorted from lowest to highest precedence.
   */
  public Iterator<StyleSheetNode> getStyleSheets(StyleContext context)
  {
    StyleSheetList styleSheets = _getStyleSheets(context);
    return styleSheets.styleSheets().iterator();
  }

  /**
   * Returns an Iterator of StyleNode objects for the specified context.
   */
  @SuppressWarnings("unchecked")
  public Iterator<StyleNode> getStyles(StyleContext context)
  {
    StyleSheetList styleSheets = _getStyleSheets(context);
    if (styleSheets.isEmpty())
    {
      List<StyleNode> emptyList = Collections.emptyList();
      return emptyList.iterator();
    }

    // We are going to loop through every StyleNode in every StyleSheetNode,
    // resolving each one along the way.  We store resolved StyleNodes in
    // a List, so that the generated CSS somewhat matches the order that
    // the style elements appear in the XSS document.
    ArrayList<StyleNode> styles = new ArrayList<StyleNode>();

    // We also need to provide a Map for storing selector-based
    // styles and another for storing name-based styles, used by
    // _resolveStyle() to store results
    HashMap<String, StyleNode> resolvedStyles = 
      new HashMap<String, StyleNode>();
    HashMap<String, StyleNode> resolvedNamedStyles = 
      new HashMap<String, StyleNode>();

    // Keep track of all selectors and names that we've already
    // found, so that we don't bother re-resolving them
    // These differ from the above Maps in that the Maps are
    // mutated by _resolveStyle - so an included selector, for
    // instance, might already be present in _resolveStyle but
    // not yet have been encountered by this top loop.  We
    // could eliminate the need for this if we were willing
    // to give up the ordering of the list
    Set<String> foundSelectors = new HashSet<String>();
    Set<String> foundNames = new HashSet<String>();

    // Now, loop through all StyleNodes in all StyleSheetNodes
    
    // We want to cache IDs in this use of StyleSheetList
    styleSheets.cacheIds();

    for (StyleSheetNode styleSheet : styleSheets.styleSheets())
    {
      Iterable<StyleNode> styleNodeList = styleSheet.getStyles();

      for (StyleNode node : styleNodeList)
      {
        String id;
        boolean isFound;
        boolean isNamed;

        // Is this a named node or a selector?
        if (node.getName() != null)
        {
          isNamed = true;
          id = node.getName();
          isFound = foundNames.contains(id);
        }
        else
        {
          isNamed = false;
          id = node.getSelector();
          isFound = foundSelectors.contains(id);
        }

        // If we've already seen that node/selector, no need to look
        // for it again
        if (!isFound)
        {
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
            styles.add(resolvedNode);
            if (isNamed)
              foundNames.add(id);
            else
              foundSelectors.add(id);
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

  // Returns array of matching style sheets sorted by specificity
  private StyleSheetList _getStyleSheets(
    StyleContext context
    )
  {
    LocaleContext localeContext = context.getLocaleContext();
    Locale locale = localeContext.getTranslationLocale();
    int direction = LocaleUtils.getReadingDirection(localeContext);
    int mode = NameUtils.getMode(ModeUtils.getCurrentMode(context));
    TrinidadAgent agent = context.getAgent();
    AccessibilityProfile accProfile = context.getAccessibilityProfile();

    List<StyleSheetNode> v = new ArrayList<StyleSheetNode>(); // List of matching style sheets
    Iterator<StyleSheetNode> e = getStyleSheets();  // Iterator of all style sheets

    // Loop through the style sheets, storing matches in the List
    while (e.hasNext())
    {
      StyleSheetNode styleSheet = e.next();

      if (styleSheet.compareVariants(locale, direction, agent, mode, accProfile) > 0)
        v.add(styleSheet);
    }

    int count = v.size();
    if (count == 0)
      return new StyleSheetList(null);

    // Sort the matching style sheets by specificity
    StyleSheetNode[] styleSheets = v.toArray(new StyleSheetNode[count]);
    Comparator<StyleSheetNode> comparator = 
      new StyleSheetComparator(locale,
                               direction,
                               agent,
                               mode,
                               _styleSheets,
                               accProfile);

    Arrays.sort(styleSheets, comparator);

    return new StyleSheetList(styleSheets);
  }

  // Gets the style with the specified selector/name
  private StyleNode _getStyle(
    StyleContext context,
    String       id,
    boolean      isNamed
    )
  {
    StyleSheetList styleSheets = _getStyleSheets(context);
    if (styleSheets.isEmpty())
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
    StyleSheetList         styleSheets,
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
      // FIXME: AdamWiner - _ERROR_STYLE_NODE is in fact never used!
      
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

    List<StyleNode> nodeList = styleSheets.styleNodes(id, isNamed);
    if (nodeList != null)
    {
      for (StyleNode node : nodeList)
      {
        // We've got a match!  We need to do the following:
        // 0. Check to see whether we need to reset our properties.
        // 1. Resolve any included styles, and shove those properties
        //    into our StyleEntry.
        // 2. Resolve any included properties, and shove those properties
        //    into our StyleEntry.
        // 3. Remove all properties that were inhibited.
        // 4. Shove all properties from the matching StyleNode into our
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
        Iterable<IncludeStyleNode> includedStyles = node.getIncludedStyles();
        for (IncludeStyleNode includeStyle : includedStyles)
        {
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
  
  
        // 2. Resolve included properties
        Iterable<IncludePropertyNode> includedProperties = node.getIncludedProperties();
        for (IncludePropertyNode includeProperty : includedProperties)
        {
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
  
        // 3. Check inhibited properties
        Iterable<String> inhibitedProperties = node.getInhibitedProperties();
        for (String inhibitedPropertyName : inhibitedProperties)
        {
          entry.removeProperty(inhibitedPropertyName);
        }
        
  
        // 4. Add non-included properties
        Iterable<PropertyNode> properties = node.getProperties();
        for (PropertyNode propertyNode : properties)
        {
          entry.addProperty(propertyNode);
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

    StyleNode resolvedNode = entry.toStyleNode();

    // If we got a node, add it in to our list
    if (resolvedNode != null)
    {
      // cache already resolved styles so we don't
      // resolve them again. This saves (a lot of) time.
      // TODO: AdamWiner: entry.toStyleNode() will return
      // null if it's an empty style.  But that means
      // that this cache doesn't get used, so we end
      // up hammering on these.  This doesn't appear
      // to be a performance issue at this time.
      String namedStyle = resolvedNode.getName();
      if (namedStyle != null)
      {
        resolvedNamedStyles.put(namedStyle, resolvedNode);
      }
      else 
      {
        String selectorStyle = resolvedNode.getSelector();
        if (selectorStyle != null)
        {
          resolvedStyles.put(selectorStyle, resolvedNode);
        }
      }
    }
    
    // Convert the StyleEntry to a StyleNode and return it
    return resolvedNode;
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

    Iterable<PropertyNode> properties = node.getProperties();
    for (PropertyNode propertyNode : properties)
    {
      entry.addIncludedProperty(propertyNode);
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

    Iterable<PropertyNode> properties = node.getProperties();
    for (PropertyNode property : properties)
    {
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

  // Returns a count of the non-null items in the List
  private static int _getNonNullCount(List<?> list)
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


  // Tests whether the value is present in the (possibly null) stack.
  private static boolean _stackContains(Stack<?> stack, Object value)
  {
    if (stack == null)
      return false;

    return stack.contains(value);
  }

  /**
   * Simple encapsulation of an array of StyleSheetNodes, to
   * support maintaining a cache of (name | selector --> StyleNode)
   */
  private static class StyleSheetList
  {
    public StyleSheetList(StyleSheetNode[] styleSheets)
    {
      _styleSheets = styleSheets;
    }
    
    /**
     * @return true if the list is empty
     */
    public boolean isEmpty()
    {
      return (_styleSheets == null) || (_styleSheets.length == 0);
    }

    /**
     * Forcibly caches IDs.  Caching IDs is only useful if
     * we're going to be retrieving a lot (generally, all)
     * of the nodes at some point.
     */
    public void cacheIds()
    {
      // Typically, there are a lot more selector nodes than
      // name nodes.  These numbers come from some statistics gathered
      // on Trinidad and other libraries
      _nameNodes = new HashMap<String, List<StyleNode>>(256);
      _selectorNodes = new HashMap<String, List<StyleNode>>(1024);
      
      for (int i = 0; i < _styleSheets.length; i++)
      {
        StyleSheetNode styleSheet = _styleSheets[i];
        Iterable<StyleNode> styleNodeList = styleSheet.getStyles();

        for (StyleNode node : styleNodeList)
        {
          if (node.getName() != null)
            _addToMap(_nameNodes, node, node.getName());
          else
            _addToMap(_selectorNodes, node, node.getSelector());
        }
      }
    }
    
    /**
     * Return a List of StyleNodes based on the "id" (either
     * a selector or name)
     * @param id the selector or name
     * @param isNamed if true, interpret "id" as a name, otherwise
     *   as a selector
     * @return the list of StyleNodes (potentially null)
     */
    public List<StyleNode> styleNodes(String id, boolean isNamed)
    {
      if (_styleSheets == null)
        return Collections.emptyList();
      Map<String, List<StyleNode>> m = isNamed ? _nameNodes : _selectorNodes;
      // Cached version - go to the Map
      if (m != null)
        return m.get(id);
      
      // Uncached version - iterate through everything and build up
      // the List
      List<StyleNode> l = new ArrayList<StyleNode>();
      for (int i = 0; i < _styleSheets.length; i++)
      {
        StyleSheetNode styleSheet = _styleSheets[i];
        Iterable<StyleNode> styleNodeList = styleSheet.getStyles();

        for (StyleNode node : styleNodeList)
        {
          if (isNamed)
          {
            if (id.equals(node.getName()))
              l.add(node);
          }
          else
          {
            if (id.equals(node.getSelector()))
              l.add(node);
          }
        }
      }
      
      return l;
    }
    
    /**
     * @return an unmodifiable list of all StyleSheetNodes
     */
    public List<StyleSheetNode> styleSheets()
    {
      if (_styleSheets == null)
      {
        return Collections.emptyList();
      }
      else
      {
        return Collections.unmodifiableList(Arrays.asList(_styleSheets));
      }
    }

    static private void _addToMap(
      Map<String, List<StyleNode>> m, 
      StyleNode                    node,
      String                       id)
    {
      List<StyleNode> l = m.get(id);
      if (l == null)
      {
        // Most properties, in fact, have only one entry
        // The most I've seen is 8.  This could probably
        // be reduced to 2
        l = new ArrayList<StyleNode>(4);
        m.put(id, l);
      }
      
      l.add(node);
    }

    
    private Map<String, List<StyleNode>> _nameNodes;
    private Map<String, List<StyleNode>> _selectorNodes;
    
    private final StyleSheetNode[] _styleSheets;
  }
  
  // Comparator for StyleSheetNodes which sorts by variant specificity
  private static class StyleSheetComparator implements Comparator<StyleSheetNode>
  {
    public StyleSheetComparator(
      Locale locale,
      int direction,
      TrinidadAgent agent,
      int mode,
      StyleSheetNode[] styleSheets,
      AccessibilityProfile accessibilityProfile
      )
    {
      _direction = direction;
      _locale = locale;
      _agent = agent;
      _styleSheets = styleSheets;
      _mode = mode;
      _accProfile = accessibilityProfile;
    }

    public int compare(StyleSheetNode item1, StyleSheetNode item2)
    {
      if (item1 == item2)
        return 0;

      int match1 = item1.compareVariants(_locale, 
                                         _direction, 
                                         _agent, 
                                         _mode,
                                         _accProfile);
      
      int match2 = item2.compareVariants(_locale, 
                                         _direction, 
                                         _agent, 
                                         _mode,
                                         _accProfile);

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
    private AccessibilityProfile _accProfile;
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
      return new StyleNode(name, selector, properties, null, null, null);
    }

    // Removes the PropertyNode with the specified name from the
    // List of properties.  Note - we assume that the properties
    // List will contain at most one property with the specified
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
  private final String     _documentVersion;
  private final long       _documentTimestamp;

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
                                                                   null);

  // Error messages
  private static final String _CIRCULAR_INCLUDE_ERROR =
    "Circular dependency detected in style ";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StyleSheetDocument.class);
}
