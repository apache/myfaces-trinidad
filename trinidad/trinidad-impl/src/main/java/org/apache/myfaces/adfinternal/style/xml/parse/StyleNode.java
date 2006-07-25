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

package org.apache.myfaces.adfinternal.style.xml.parse;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;




/**
 * Private implementation of StyleNode.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/StyleNode.java#0 $) $Date: 10-nov-2005.18:58:11 $
 * @author The Oracle ADF Faces Team
 */
public class StyleNode
{
  // A private constructor that we are temporarily using while
  // resolving the now-deprecated color properties
  StyleNode(
    StyleNode style,
    PropertyNode[] properties
    )
  {
    this(style._name,
         style._selector,
         properties,
         style._compoundProperties,
         style._includedStyles,
         style._includedProperties);
  }

  /**
   * Creates a Style with the specified properties
   */
  public StyleNode(
    String                 name,
    String                 selector,
    PropertyNode[]         properties,
    CompoundPropertyNode[] compoundProperties,
    IncludeStyleNode[]     includedStyles,
    IncludePropertyNode[]  includedProperties
    )
  {
    this(name,
         selector,
         properties,
         compoundProperties,
         includedStyles,
         includedProperties,
         false);
  }

  // Constructor which also specifies allows resetProperties
  // flag to be specified.  This is package-private, since
  // only StyleNodeParser should be calling this.
  StyleNode(
    String                 name,
    String                 selector,
    PropertyNode[]         properties,
    CompoundPropertyNode[] compoundProperties,
    IncludeStyleNode[]     includedStyles,
    IncludePropertyNode[]  includedProperties,
    boolean                resetProperties
    )
  {
    _name = name;
    _selector = selector;

    _resetProperties = resetProperties;

    if (properties != null)
    {
      _properties = new PropertyNode[properties.length];
      System.arraycopy(properties, 0, _properties, 0, properties.length);
    }

    if (compoundProperties != null)
    {
      _compoundProperties =
        new CompoundPropertyNode[compoundProperties.length];
      System.arraycopy(compoundProperties, 0,
                       _compoundProperties, 0,
                       compoundProperties.length);
    }

    if (includedStyles != null)
    {
      _includedStyles = new IncludeStyleNode[includedStyles.length];
      System.arraycopy(includedStyles, 0,
                       _includedStyles, 0,
                       _includedStyles.length);
    }

    if (includedProperties != null)
    {
      _includedProperties = new IncludePropertyNode[includedProperties.length];
      System.arraycopy(includedProperties, 0,
                       _includedProperties, 0,
                       _includedProperties.length);
    }
  }

  /**
   * Implementation of StyleNode.getName().
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Implementation of StyleNode.getSelector().
   */
  public String getSelector()
  {
    return _selector;
  }

  /**
   * Implementation of StyleNode.getProperties().
   */
  public Iterator getProperties()
  {
  if (_properties == null) 
  {
    return (Collections.EMPTY_LIST).iterator();
  }
  else
    return (Arrays.asList(_properties)).iterator();
  }

  /**
   * Returns an Iterator containing Strings and IncludePropertyNodes.
   */
  public Iterator getCompoundProperties()
  {
    if (_compoundProperties == null) 
    {
      return (Collections.EMPTY_LIST).iterator();
    }
    else
      return (Arrays.asList(_compoundProperties)).iterator();
  }

  /**
   * Returns an Iterator of IncludeStyleNodes.
   */
  public Iterator getIncludedStyles()
  {
    if (_includedStyles == null) 
    {
      return (Collections.EMPTY_LIST).iterator();
    }
    else
    return (Arrays.asList(_includedStyles)).iterator();
  }

  /**
   * Returns an Iterator of IncludePropertyNodes.
   */
  public Iterator getIncludedProperties()
  {
    if(_includedProperties == null) 
    {
      return (Collections.EMPTY_LIST).iterator();
    }
    else
      return (Arrays.asList(_includedProperties)).iterator();
  }

  // Just leaving this package-private, since only
  // StyleSheetDocument really needs to know about this.
  // Really, we should have a StyleNodeImpl for this kind
  // of implementation detail.
  boolean __getResetProperties()
  {
    return _resetProperties;
  }

  private String                 _name;
  private String                 _selector;
  private PropertyNode[]         _properties;          // The property nodes
  private CompoundPropertyNode[] _compoundProperties;  // Compound properties
  private IncludeStyleNode[]     _includedStyles;      // Included styles
  private IncludePropertyNode[]  _includedProperties;  // Included properties

  // This flag checks whether the style should inherit properties
  // from equivalent styles defined in earlier style sheets.
  private boolean                _resetProperties;

}
