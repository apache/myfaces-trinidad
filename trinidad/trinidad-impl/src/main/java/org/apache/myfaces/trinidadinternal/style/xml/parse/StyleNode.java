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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;


/**
 * Private implementation of StyleNode.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/StyleNode.java#0 $) $Date: 10-nov-2005.18:58:11 $
 */
public class StyleNode
{

  /**
   * Creates a Style with the specified properties
   */
  public StyleNode(
    String                 name,
    String                 selector,
    PropertyNode[]         properties,
    IncludeStyleNode[]     includedStyles,
    IncludePropertyNode[]  includedProperties,
    Set<String>            inhibitedProperties
    )
  {
    this(name,
         selector,
         properties,
         includedStyles,
         includedProperties,
         inhibitedProperties,
         false);
  }

  // Constructor which also specifies allows resetProperties
  // flag to be specified.  This is package-private, since
  // only StyleNodeParser should be calling this.
  StyleNode(
    String                 name,
    String                 selector,
    PropertyNode[]         properties,
    IncludeStyleNode[]     includedStyles,
    IncludePropertyNode[]  includedProperties,
    Set<String>            inhibitedProperties,
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
    
    _inhibitAll = false;
    if(inhibitedProperties != null)
    {
      _inhibitedProperties = new ArrayList<String>(inhibitedProperties.size());
      for(String property : inhibitedProperties)
      {
        if(_INHIBIT_ALL_VALUE.equalsIgnoreCase(property))
        { // Case insensitivity for "all" value
          _inhibitAll = true;
        }
        else
        {
          _inhibitedProperties.add(property);
        }
      }
      
      if(_inhibitedProperties != null)
      {
        _inhibitedProperties = Collections.unmodifiableList(_inhibitedProperties);
      }
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
   * Returns true if the style node has no properties. 
   */
  public boolean isEmpty()
  {
    if (_properties != null && _properties.length > 0)
      return false;
    if (_includedStyles != null && _includedStyles.length > 0)
      return false;
    if (_includedProperties != null && _includedProperties.length > 0)
      return false;
    return true;
  }

  /**
   * Implementation of StyleNode.getProperties().
   */
  public Iterator<PropertyNode> getProperties()
  {
  if (_properties == null) 
  {
    List<PropertyNode> list = Collections.emptyList();
    return list.iterator();
  }
  else
    return (Arrays.asList(_properties)).iterator();
  }

  /**
   * Returns an Iterator of IncludeStyleNodes.
   */
  public Iterator<IncludeStyleNode> getIncludedStyles()
  {
    if (_includedStyles == null) 
    {
      List<IncludeStyleNode> list = Collections.emptyList();
      return list.iterator();
    }
    else
    {
      return (Arrays.asList(_includedStyles)).iterator();
    }
  }

  /**
   * Returns an Iterator of IncludePropertyNodes.
   */
  public Iterator<IncludePropertyNode> getIncludedProperties()
  {
    if(_includedProperties == null) 
    {
      List<IncludePropertyNode> list = Collections.emptyList();
      return list.iterator();
    }
    else
      return (Arrays.asList(_includedProperties)).iterator();
  }
  
  /**
   * Gets the properties specified by this node's parent that should be
   * ignored. This method will return an empty iterator if 
   * {@link #isInhibitingAll()} returns <code>true</code>
   * 
   * @return an iterator over the properties that should be ignored, an 
   *         empty iterator if all properties should be.
   */
  public Iterator<String> getInhibitedProperties()
  {
    if(_inhibitedProperties == null) 
    {
      List<String> list = Collections.emptyList();
      return list.iterator();
    }
    else
    {
      return _inhibitedProperties.iterator();
    }
  }
  
  /**
   * Determines if this node inhibits all of its inherited properties.
   * 
   * @return <code>true</code> if this node ignores all properties defined 
   *         by its parent, <code>false</code> otherwise.
   */
  public boolean isInhibitingAll()
  {
    return _inhibitAll;
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
  private IncludeStyleNode[]     _includedStyles;      // Included styles
  private IncludePropertyNode[]  _includedProperties;  // Included properties
  private List<String>           _inhibitedProperties; // Inhibited properties
  
  // These flags checks whether the style should inherit properties
  // from equivalent styles defined in earlier style sheets.
  // This is xss-formatted skin files when resetProperties="true".
  private boolean                _resetProperties;
  // This is css-formatted skin files when -tr-inhibit: all.
  private boolean                _inhibitAll;

  private static final String _INHIBIT_ALL_VALUE = "all";
}
