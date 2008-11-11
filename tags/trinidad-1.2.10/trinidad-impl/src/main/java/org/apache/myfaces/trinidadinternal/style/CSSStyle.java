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

import java.util.Arrays;
import java.util.Map;
import java.util.Collections;
import java.util.Iterator;

import org.apache.myfaces.trinidadinternal.style.util.CSSUtils;


/**
 * Style implementation for CSS.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/CSSStyle.java#0 $) $Date: 10-nov-2005.18:57:55 $
 */
public class CSSStyle extends BaseStyle
{
  /**
   * Creates an empty CSSStyle.
   */
  public CSSStyle()
  {
  }

  /**
   * Creates an CSSStyle with the specified properties.
   *
   * @param properties The properties of this style.  The
   *   values must be Strings.
   */
  public CSSStyle(Map<String, String> properties)
  {
    super(properties);
  }

  /**
   * Creates a BaseStyle from an arbitrary Style object.
   * All of the properties from the specified Style object are
   * copied into this CSSStyle.
   */
  public CSSStyle(Style style)
  {
    super(style);
  }

  /**
   * Converts the style to a String suitable for use as an inline style
   * attribute value.
   */
  @Override
  public String toInlineString()
  {
    String inline = _inline;

    if (inline != null)
      return inline;

    Iterator<Object> e = getPropertyNames();
    StringBuffer buffer = new StringBuffer(_DEFAULT_BUFFER_SIZE);
    boolean first = true;

    if ( e != null )
    {
      while (e.hasNext())
      {
        if (first)
          first = false;
        else
          buffer.append(";");

        String name = (String)e.next();
        String value = getProperty(name);

        buffer.append(name);
        buffer.append(":");
        buffer.append(value);
      }
    }

    inline = buffer.toString();

    synchronized (this)
    {
      _inline = inline;
    }

    return inline;
  }

  /**
   * Sets the specified property value.
   */
  @Override
  public void setProperty(String name, String value)
  {
    super.setProperty(name, value);

    // Null out our cached inline string
    synchronized (this)
    {
      _inline = null;
    }
  }

  /**
   * Parses the property for the specified key.
   */
  @Override
  protected Object parseProperty(Object key)
    throws PropertyParseException
  {
    Object value = null;

    if (key == Style.BACKGROUND_KEY)
    {
      value = CSSUtils.parseColor(getProperty(_BACKGROUND_NAME));
    }
    else if (key == Style.FOREGROUND_KEY)
    {
      value = CSSUtils.parseColor(getProperty(_FOREGROUND_NAME));
    }
    else if (key == Style.FONT_SIZE_KEY)
    {
      value = CSSUtils.parseFontSize(getProperty(_FONT_SIZE_NAME));
    }
    else if (key == Style.FONT_STYLE_KEY)
    {
      value = CSSUtils.parseFontStyle(getProperty(_FONT_STYLE_NAME));
    }
    else if (key == Style.FONT_WEIGHT_KEY)
    {
      value = CSSUtils.parseFontWeight(getProperty(_FONT_WEIGHT_NAME));
    }
    else if (key == Style.FONT_FAMILIES_KEY)
    {
      String[] families = CSSUtils.parseFontFamilies(getProperty(
                                                     _FONT_FAMILY_NAME));

      if (families != null)
        value = Collections.unmodifiableList(Arrays.asList(families));
    }
    else if (key == Style.TEXT_ANTIALIAS_KEY)
    {
      String antialiased = getProperty(_TEXT_ANTIALIAS_NAME);
      if ((antialiased != null) && "true".equalsIgnoreCase(antialiased))
      {
        value = Boolean.TRUE;
      }
    }

    return value;
  }

  @Override
  public String toString()
  {
    return "CSSStyle[css=" + toInlineString() + "]"; 
  }

  // The cached inline String value
  transient private String _inline;

  private static final String _BACKGROUND_NAME = "background-color";
  private static final String _FOREGROUND_NAME = "color";
  private static final String _FONT_FAMILY_NAME = "font-family";
  private static final String _FONT_SIZE_NAME   = "font-size";
  private static final String _FONT_STYLE_NAME  = "font-style";
  private static final String _FONT_WEIGHT_NAME = "font-weight";
  private static final String _TEXT_ANTIALIAS_NAME = "text-antialias";

  // Default length for inline string buffer
  private static final int _DEFAULT_BUFFER_SIZE = 100;
  private static final long serialVersionUID = 1L;
}
