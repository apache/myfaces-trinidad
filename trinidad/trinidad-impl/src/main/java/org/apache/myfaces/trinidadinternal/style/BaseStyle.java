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

import java.io.Serializable;
import java.util.Map;
import java.util.Iterator;


import org.apache.myfaces.trinidad.util.ArrayMap;

/**
 * Base class for Style implementations
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/BaseStyle.java#0 $) $Date: 10-nov-2005.18:57:54 $
 */
abstract public class BaseStyle implements Style, Serializable
{
  /**
   * Creates an empty BaseStyle.
   */
  public BaseStyle()
  {
  }

  /**
   * Creates a BaseStyle with the specified properties
   *
   * @param properties The properties of this style.  The
   *   values must be Strings.
   */
  public BaseStyle(Map<String, String> properties)
  {
    if ((properties != null) && (properties.size() > 0))
    {
      // Initialize the properties array
      int length = properties.size() * 2;
      _properties = new String[length];

      int i = 0;
      for(Map.Entry<String, String> entry : properties.entrySet())
      {
        String key   = entry.getKey();
        String value = entry.getValue();

        // -= Simon Lessard =-
        // FIXME: If key is ever null, NullPointerException will occurs
        _properties[i*2]   = key.toLowerCase();
        _properties[i*2+1] = value;
        i++;
      }
    }
  }

  /**
   * Creates a BaseStyle from an arbitrary Style object.
   */
  public BaseStyle(Style style)
  {
    if ( style != null)
    {

      // First, loop through to get the property count
      int propertyCount = 0;
      Iterator<Object> e = style.getPropertyNames();
      while (e.hasNext())
      {
        e.next();
        propertyCount++;
      }

      if (propertyCount == 0)
        return;

      // Initialize the properties array
      Object properties[] = new Object[propertyCount * 2];

      // Now, loop through to initialize the properties
      int i = 0;
      Iterator<Object> names = style.getPropertyNames();
      while (names.hasNext())
      {
        String name = (String)names.next();
        String value = style.getProperty(name);

        properties[i*2] = name.toLowerCase();
        properties[i*2+1] = value;

        i++;
      }

      _properties = properties;
    }
  }

  /**
   * Returns the names of the properties defined by this style.
   * <p>
   * The property names can be any valid property name.
   */
  public Iterator<Object> getPropertyNames()
  {
    return ArrayMap.getKeys(_properties);
  }

  /**
   * Returns the value of the property with the specified name.
   *
   * @param name The property name for the property to return
   */
  public String getProperty(String name)
  {
    return (String)ArrayMap.get(_properties, name.toLowerCase());
  }


  /**
   * Returns a parsed Java object corresponding to the specified
   * property key.
   */
  public Object getParsedProperty(ParsedPropertyKey key)
    throws PropertyParseException
  {
    Object value = null;

    // For better or worse, we assume that the key is a ParsedPropertyKey.
    // This isn't part of the public API, but we are in cahoots with the
    // Style interface.  Any other type of key is going to cause a
    // ClassCastException.  Note - we could just make Style.getParsedProperty
    // take a ParsedPropertyKey instead of an Object, but I'm not convinced
    // that the ParsedPropertyKey is the way to go just yet.
    int index = key.getKeyIndex();

    if (_parsedProperties != null)
    {
      value = _parsedProperties[index];

      if (value != null)
      {
        if (value == _NULL_VALUE)
          value = null;

        return value;
      }
    }
    else
    {
      // Just allocate the parsed properties array now, we're going to
      // write to it no matter what at this point
      synchronized (this)
      {
         if (_parsedProperties == null)
           _parsedProperties = new Object[_PARSED_PROPERTIES_COUNT];
      }
    }

    // If we don't have a parsed property value already, try to get it.
    try
    {
      value = parseProperty(key);
    }
    catch (PropertyParseException e)
    {
      _parsedProperties[index] = _NULL_VALUE;
      throw e;
    }

    _parsedProperties[index] = (value == null) ? _NULL_VALUE : value;

    return value;
  }

  /**
   * Sets the specified property value.
   */
  public void setProperty(String name, String value)
  {
    // We store all names/values as lowercase string
    name = name.toLowerCase();

    synchronized (this)
    {
      _properties = ArrayMap.remove(_properties, name);

      if (value != null)
        _properties = ArrayMap.put(_properties, name, value);

      // We need to reset to parsed properties if our properties change
      // Really, we could just null out the corresponding parsed property
      // value, but what the heck.
      _parsedProperties = null;
    }
  }

  /**
   * Converts the style to a String suitable for use as an inline style
   * attribute value.
   */
  abstract public String toInlineString();

  /**
   * Parses the property for the specified key.
   * Subclasses should implement this method to perform style sheet
   * language-specific parsing.
   */
  abstract protected Object parseProperty(Object key)
    throws PropertyParseException;

  private Object[] _properties;
  transient private Object[] _parsedProperties;

  // Count of parsed properties defined by Style
  private static final int _PARSED_PROPERTIES_COUNT = 7;

  // Null value used to indicate that the property has been parsed, but
  // was either invalid or null.  We use this placeholder to avoid re-parsing
  // invalid or null values.
  private static final Object _NULL_VALUE = new Object();
}
