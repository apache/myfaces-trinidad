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
package org.apache.myfaces.trinidadinternal.style;

import java.util.Map;
import java.util.Iterator;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Arrays;

import org.apache.myfaces.trinidad.util.ArrayMap;

/**
 * The UserStyleSheet is used to specify user-specific style overrides
 * which are merged with the styles defined by a StyleProvider.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/UserStyleSheet.java#0 $) $Date: 10-nov-2005.18:57:59 $
 * @author The Oracle ADF Faces Team
 */
public class UserStyleSheet
{
  /**
   * Convenience method for retrieving a UserStyleSheet from a StyleContext.
   */
  public static UserStyleSheet getUserStyleSheet(StyleContext context)
  {
    // =-=AEW Removed.
    return null;
  }

  /**
   * Creates a UserStyleSheet.
   * @param id A unique string which identifies this UserStyleSheet.
   *   Note that this string is embedded in the name of the generated
   *   style sheet file.  As such, the ID should be suitable for use
   *   in a file system name.  Control characters - and possibly
   *   non-ASCII characters should probably be avoided.
   * @param styles A Map of Style objects, hashed by selector
   * @param namedStyles A Map or names Style objects, hashed by name
   */
  public UserStyleSheet(
    String             id,
    Map<String, Style> styles,
    Map<String, Style> namedStyles
    )
  {
    _init(id, styles, namedStyles);
  }

  /**
   * Creates a UserStyleSheet by merging styles from two different
   * UserStyleSheets.  When styles are defined in both style sheets,
   * the properties in the first style sheet override the same properties
   * in the second style sheet.
   * @param id A unique string which identifies the new UserStyleSheet.
   *   Note that this string is embedded in the name of the generated
   *   style sheet file.  As such, the ID should be suitable for use
   *   in a file system name.  Control characters - and possibly
   *   non-ASCII characters should probably be avoided.
   *           If null, a unique name will be formed by concatenating the
   *           ids from styleSheet1 and styleSheet2.
   * @param styleSheet1 The first style sheet from which styles should
   *           be obtained.  Style properties defined in this style sheet
   *           take precedence over the same properties defined in
   *           styleSheet2.
   * @param styleSheet2 The second style sheet from which styles should
   *           be obtained.
   */
  public UserStyleSheet(
    String id,
    UserStyleSheet styleSheet1,
    UserStyleSheet styleSheet2
    )
  {
    // -= Simon Lessard =- 
    // TODO: Check if synchronization is required
    Hashtable<String, Style> namedStyles = new Hashtable<String, Style>();
    Hashtable<String, Style> styles = new Hashtable<String, Style>();

    // First, put all of the styles from style sheet 2 into our hash tables
    if (styleSheet2 != null)
    {
      // First, do the named styles
      Iterator<Object> e = styleSheet2.getNames();
      if (e != null)
      {
        while (e.hasNext())
        {
          String name = (String)e.next();
          namedStyles.put(name, styleSheet2.getNamedStyle(name));
        }
      }

      // Next do the selector-based styles
      e = styleSheet2.getSelectors();
      if (e != null)
      {
        while (e.hasNext())
        {
          String selector = (String)e.next();
          namedStyles.put(selector, styleSheet2.getStyle(selector));
        }
      }

    }

    // Now, put all of the styles from style sheet 1 into our hash tables,
    // merging any matching styles from style sheet 2.
    if (styleSheet1 != null)
    {
      // First merge the named styles
      Iterator<Object> e = styleSheet1.getNames();
      if (e != null)
      {
        while (e.hasNext())
        {
          String name = (String)e.next();
          _mergeStyle(namedStyles,
                      name,
                      styleSheet1.getNamedStyle(name));
        }
      }

      // Now merge the selector-based styles
      e = styleSheet1.getSelectors();
      if (e != null)
      {
        while (e.hasNext())
        {
          String selector = (String)e.next();
          _mergeStyle(styles,
                      selector,
                      styleSheet1.getStyle(selector));
        }
      }
    }

    // Make sure we've got a valid id
    if (id == null)
    {
      if (styleSheet1 == null)
        id = styleSheet2.getID();
      else if (styleSheet2 == null)
        id = styleSheet1.getID();
      else
        id = styleSheet1.getID() + "-" + styleSheet2.getID();
    }

    // Now, create the UserStyleSheet
    _init(id, styles, namedStyles);
  }

  /**
   * Returns the ID for this UserStyleSheet.
   */
  public String getID()
  {
    return _id;
  }

  /**
   * Returns an Iterator of the String selectors for the
   * Styles of this UserStyleSheet.
   * The Style object corresponding with each selector can be obtained
   * by calling getStyle().
   * @see #getStyle
   */
  public Iterator<Object> getSelectors()
  {
    return ArrayMap.getKeys(_styles);
  }

  /**
   * Returns an Iterator of the String names for the
   * named Styles of this UserStyleSheet.
   * The Style object corresponding with each name can be obtained
   * by calling getNamedStyle().
   * @see #getNamedStyle
   */
  public Iterator<Object> getNames()
  {
    return ArrayMap.getKeys(_namedStyles);
  }

  /**
   * Returns the Style associated with the specified selector.
   */
  public Style getStyle(String selector)
  {
    return (Style)ArrayMap.get(_styles, selector);
  }

  /**
   * Returns the named Style associated with the specified name.
   */
  public Style getNamedStyle(String name)
  {
    return (Style)ArrayMap.get(_namedStyles, name);
  }

  /**
   * Tests for equality.
   */
  @Override
  public boolean equals(Object o)
  {
    if (!(o instanceof UserStyleSheet))
      return false;

    UserStyleSheet styleSheet = (UserStyleSheet)o;
    if (!getID().equals(styleSheet.getID()))
      return false;

    if ((_styles.length != styleSheet._styles.length) ||
        (_namedStyles.length != styleSheet._namedStyles.length))
      return false;

    // Compare the styles
    for (int i = 0; i < _styles.length; i++)
    {
      if (!_styles[i].equals(styleSheet._styles[i]))
        return false;
    }

    // Compare the named styles
    for (int i = 0; i < _namedStyles.length; i++)
    {
      if (!_namedStyles[i].equals(styleSheet._namedStyles[i]))
        return false;
    }

    return true;
  }

  /**
   * Returns the hash code.
   */
  @Override
  public int hashCode()
  {
    return _hashCode;
  }

  // Creates an ArrayMap of Styles, hashed by selector (or name, whatever
  // String key is used in the styles Map).  Oddly enough, the
  // resulting ArrayMap is actually sorted lexicographically by key.  This
  // is done to support fast comparisons between two UserStyleSheet
  // instances.
  private Object[] _createSortedMap(Map<String, Style> styles)
  {
    if ((styles == null) || (styles.size() == 0))
      return _EMPTY_MAP;

    // First, copy all of the style keys into an Array
    int i = 0;
    String keys[] = new String[styles.size()];
    Iterator<String> e = styles.keySet().iterator();

    while (e.hasNext())
      keys[i++] = e.next();

    // Now, sort the array
    Arrays.sort(keys);


    // Now, store the sorted Styles in an ArrayMap
    Object[] map = new Object[keys.length * 2];

    for (i = 0; i < keys.length; i++)
    {
      String key = keys[i];
      Style style = new SortedStyle(styles.get(key));
      int index = i * 2;
      map[index] = key;
      map[index + 1] = style;
    }

    return map;
  }

  // Computes the hash code
  private int _hashCode()
  {
    // This is a bit on the slow side, but it's only called once per instance
    int hashCode = getID().hashCode();

    for (int i = 0; i < _styles.length; i++)
      hashCode ^= _styles[i].hashCode();

    for (int i = 0; i < _namedStyles.length; i++)
      hashCode ^= _namedStyles[i].hashCode();

    return hashCode;
  }

  // Initializes a new UserStyleSheet instance
  private void _init(
    String             id,
    Map<String, Style> styles,
    Map<String, Style> namedStyles)
  {
    assert (id != null);

    _id = id;
    _styles = _createSortedMap(styles);
    _namedStyles = _createSortedMap(namedStyles);

    // Pre-compute the hash code
    _hashCode = _hashCode();
  }

  // Merges the specified style into the styles dictionary.  If
  // a style with the same id already exists in the styles dictionary,
  // the properties of the two styles are merged, with the properties
  // of the new style taking precedence.
  private static void _mergeStyle(
    Map<String, Style> styles,
    String             id,
    Style              style
    )
  {
    Style oldStyle = styles.get(id);
    if (oldStyle == null)
    {
      styles.put(id, style);
      return;
    }

    // If we already had a style with the same ID, merge the properties
    // from the two styles.
    ArrayMap<String, String> properties = 
      new ArrayMap<String, String>(10);

    // First, copy in properties from the old style
    Iterator<Object> names = oldStyle.getPropertyNames();
    while (names.hasNext())
    {
      String name = (String)names.next();
      properties.put(name, oldStyle.getProperty(name));
    }

    // Now, copy in properties from new style, overwriting
    // common properties from the old style
    names = style.getPropertyNames();
    while (names.hasNext())
    {
      String name = (String)names.next();
      properties.put(name, style.getProperty(name));
    }

    // Create and store away the new style
    // =-=ags Hardcoding CSSStyle for now.  Eventually, we're going to
    //        want to use a factory to create styles based on a style type.
    styles.put(id, new CSSStyle(properties));
  }

  // Style implementation which stores properties in a sorted list
  // to support fast comparisons.
  private static class SortedStyle implements Style
  {
    private SortedStyle() {}

    // Creates a SortedStyle, copying the properties from the
    // specified Style.
    public SortedStyle(Style style)
    {
      // First, copy the property names into an array
      // -= Simon Lessard =- 
      // TODO: Check if synchronization is truly required
      Vector<String> v = new Vector<String>();
      Iterator<Object> e = style.getPropertyNames();
      while (e.hasNext())
        v.addElement((String)e.next());

      String[] names = new String[v.size()];
      v.copyInto(names);

      // Sort the array
      Arrays.sort(names);

      // Now, copy the sorted properties into an ArrayMap
      Object[] properties = new Object[names.length * 2];

      for (int i = 0; i < names.length; i++)
      {
        String name = names[i];
        int index = i * 2;
        properties[index] = name;
        properties[index + 1] = style.getProperty(name);
      }

      _properties = properties;
    }

    public Iterator<Object> getPropertyNames()
    {
      return ArrayMap.getKeys(_properties);
    }

    public String getProperty(String name)
    {
      return (String)ArrayMap.get(_properties, name);
    }

    public Object getParsedProperty(ParsedPropertyKey key)
    {
      // This should never get called
      assert false;
      return null;
    }

    public String toInlineString()
    {
      // This should never get called
      assert false;
      return null;
    }

    @Override
    public int hashCode()
    {
      // Hashcode doesn't need to be very fast, as it is
      // only computed once per instance.
      int hashCode = 0;
      for (int i = 0; i < _properties.length; i++)
      {
        if (_properties[i] != null)
          hashCode ^= _properties[i].hashCode();
      }

      return hashCode;
    }

    @Override
    public boolean equals(Object o)
    {
      SortedStyle style = (SortedStyle)o;

      if (_properties.length != style._properties.length)
        return false;

      for (int i = 0; i < _properties.length; i++)
      {
        Object val1 = _properties[i];
        Object val2 = style._properties[i];

        if (val1 == null)
        {
          if (val2 != null)
            return false;
        }
        else if (!val1.equals(val2))
        {
          return false;
        }
      }

      return true;
    }

    private Object[] _properties;
  }

  private String   _id;           // The unique ID for this style sheet
  private Object[] _styles;       // ArrayMap of Styles hashed by selector
  private Object[] _namedStyles;  // ArrayMap of named Styles hashed by name
  private int      _hashCode;     // The pre-computed hash code

  // Empty ArrayMap that we use as a placeholder for null maps
  private static final Object[] _EMPTY_MAP = new Object[0];
}
