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

package org.apache.myfaces.adfinternal.style.util;

import java.awt.Color;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.Vector;

import java.util.Collections;

import org.apache.myfaces.adf.util.ArrayMap;

import org.apache.myfaces.adfinternal.util.LRUCache;
import org.apache.myfaces.adfinternal.util.IntegerUtils;


import org.apache.myfaces.adfinternal.style.CSSStyle;
import org.apache.myfaces.adfinternal.style.Style;
import org.apache.myfaces.adfinternal.style.PropertyParseException;

/**
 * CSS-related utilities. I think as we move away from xss, this code will
 * be removed.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/util/CSSUtils.java#0 $) $Date: 10-nov-2005.18:58:49 $
 * @author The Oracle ADF Faces Team
 */
public class CSSUtils
{
  /**
   * Parse an inline style into a CSSStyle object.
   */
  public static CSSStyle parseStyle(String text)
  {
    if ((text == null) || "".equals(text))
      return null;

    // Common mistake: wrapping style attributes in braces.  Drop
    // the braces on their behalf.
    if (text.startsWith("{"))
      text = text.substring(1);
    if (text.endsWith("}"))
      text = text.substring(0, text.length() - 1);

    CSSStyle style = new CSSStyle();
    StringTokenizer tokens = new StringTokenizer(text, ";");
    while (tokens.hasMoreTokens())
    {
      String token = tokens.nextToken();
      int colonPos = token.indexOf(':');
      if (colonPos >= 0)
      {
        String key = token.substring(0, colonPos).trim();
        String value = token.substring(colonPos + 1).trim();
        style.setProperty(key, value);
      }
    }

    return style;
  }


  /**
   * Parses the CSS color value.
   *
   * @param value A CSS color value.
   * @return A Color object which corresponds to the provided value
   * @throws PropertyParseException Thrown if the specified color
   *   value can not be parsed.
   */
  public static Color parseColor(String value)
    throws PropertyParseException
  {
    // First, lose any extra white space
    value = _stripWhitespace(value);

    if ((value == null) || (value.length() == 0))
      return null;

    // Lower-case the value
    value = value.toLowerCase();

    // First check to see if it is a named value
    Color color = (Color)ArrayMap.get(_NAMED_COLORS, value);
    if (color != null)
      return color;

    // Parse #RRGGBB and #RGB values
    if (value.charAt(0) == '#')
    {
      int length = value.length();

      if (length == 7)
      {
        // #RRGGBB
        int rgb = 0;

        try
        {
          rgb = Integer.parseInt(value.substring(1), 16);
        }
        catch (NumberFormatException e)
        {
          throw new PropertyParseException(_INVALID_COLOR + value);
        }

        return _getSharedColor(rgb);
      }
      else if (length == 4)
      {
        // #RGB
        int r = 0;
        int g = 0;
        int b = 0;

        try
        {
          r = Integer.parseInt(value.substring(1, 2), 16);
          g = Integer.parseInt(value.substring(2, 3), 16);
          b = Integer.parseInt(value.substring(3, 4), 16);
        }
        catch (NumberFormatException e)
        {
          throw new PropertyParseException(_INVALID_COLOR + value);
        }

        int rgb = (((r << 20) & 0xf00000) |
                   ((r << 16) & 0x0f0000) |
                   ((g << 12) & 0x00f000) |
                   ((g << 8)  & 0x000f00) |
                   ((b << 4)  & 0x0000f0) |
                    (b & 0x00000f));

        return _getSharedColor(rgb);
      }
      else
      {
        throw new PropertyParseException(_INVALID_COLOR + value);
      }
    }

    // Handle rgb(r, g, b) values
    if (value.startsWith("rgb"))
    {
      int startIndex = value.indexOf('\u0028');  // Start paren
      int endIndex = value.indexOf('\u0029');    // End paren
      if ((startIndex == -1) || (endIndex == -1) || (endIndex <= startIndex))
      {
        throw new PropertyParseException(_INVALID_COLOR + value);
      }

      // Tokenize on whitespace or commas
      StringTokenizer tokens = new StringTokenizer(
                                 value.substring(startIndex + 1, endIndex),
                                 " \t,");

      String redToken = null;
      String blueToken = null;
      String greenToken = null;

      try
      {
        redToken = tokens.nextToken();
        greenToken = tokens.nextToken();
        blueToken = tokens.nextToken();
      }
      catch (NoSuchElementException e)
      {
        throw new PropertyParseException(_INVALID_COLOR + value);
      }

      int red = _parseColorComponent(value, redToken);
      int green = _parseColorComponent(value, greenToken);
      int blue = _parseColorComponent(value, blueToken);

      return _getSharedColor(red, green, blue);
    }

    // Check for system color values.  We can't actually return valid
    // values for these, but we also don't want to throw a
    // PropertyParseException if a system color is specified.
    if (ArrayMap.get(_SYSTEM_COLORS, value) != null)
      return null;

    throw new PropertyParseException(_INVALID_COLOR + value);
  }

  /**
   * Parses a CSS font family value into a list of font family names.
   *
   * @param value A CSS font family value.
   * @return The list of font family names present in the font family property
   * @throws PropertyParseException Thrown if the specified font family
   *   value can not be parsed.
   */
  public static String[] parseFontFamilies(String value)
    throws PropertyParseException
  {
    if ((value == null) || (value.length() == 0))
      return null;

    Vector v = new Vector();
    StringTokenizer tokens = new StringTokenizer(value, ",\"");
    while (tokens.hasMoreTokens())
    {
      String family = _stripWhitespace(tokens.nextToken());

      if ((family != null) && (family.length() > 0))
        v.addElement(family);
    }

    // Copy the results into an array
    String[] families = new String[v.size()];
    v.copyInto(families);

    return families;
  }

  /**
   * Parses a CSS font size.
   *
   * @param value A CSS font size value.  At the moment, all font
   *   sizes must be specified in points (eg. "12pt").
   * @return An Integer font size suitable for use as an AWT Font size
   * @throws PropertyParseException Thrown if the specified font size
   *   value can not be parsed.
   */
  public static Integer parseFontSize(String value)
    throws PropertyParseException
  {
    // First, lose any extra white space
    value = _stripWhitespace(value);

    if ((value == null) || (value.length() == 0))
      return null;

    value = value.toLowerCase();

    // First, check to see if the size is one of the named values
    Integer fontSize = (Integer)ArrayMap.get(_NAMED_FONTS_SIZES, value);
    if (fontSize != null)
      return fontSize;

    if (_isLength(value))
      return parseLength(value);

    if (_isPercentage(value))
      return _parsePercentage(value);

    throw new PropertyParseException(_INVALID_FONT_SIZE + value);
  }

  /**
   * Parses a CSS font style
   *
   * @param value A CSS font style value.
   * @return An integer font style suitable for use as an AWT Font style
   * @throws PropertyParseException Thrown if the specified font style
   *   value can not be parsed.
   */
  public static Object parseFontStyle(String value)
    throws PropertyParseException
  {
    // First, lose any extra white space
    value = _stripWhitespace(value);

    if ((value == null) || (value.length() == 0))
      return null;

    value = value.toLowerCase();

    if (_NORMAL_STYLE.equals(value))
      return Style.PLAIN_FONT_STYLE;

    if (_ITALIC_STYLE.equals(value) || _OBLIQUE_STYLE.equals(value))
      return Style.ITALIC_FONT_STYLE;

    throw new PropertyParseException(_INVALID_FONT_STYLE + value);
  }

  /**
   * Parses a CS font weight
   *
   * @param value A CSS font weight value.  At the moment, all font
   *   weights must be specified as either "bold" or "normal".
   * @return An integer font weight suitable for use as the weight
   *   component of an AWT Font style
   * @throws PropertyParseException Thrown if the specified font weight
   *   value can not be parsed.
   */
  public static Object parseFontWeight(String value)
    throws PropertyParseException
  {
    // First, lose any extra white space
    value = _stripWhitespace(value);

    if ((value == null) || (value.length() == 0))
      return null;

    value = value.toLowerCase();

    if (_NORMAL_WEIGHT.equals(value) || _LIGHTER_WEIGHT.equals(value))
      return Style.PLAIN_FONT_WEIGHT;

    if (_BOLD_WEIGHT.equals(value) || _BOLDER_WEIGHT.equals(value))
      return Style.BOLD_FONT_WEIGHT;

    // Check for sizes 100 - 900
    try
    {
      int weight = Integer.parseInt(value);
      if ((weight >= 100) && (weight <= 900) && ((weight % 100) == 0))
      {
        if (weight >= 600)
          return Style.BOLD_FONT_WEIGHT;

        return Style.PLAIN_FONT_WEIGHT;
      }
    }
    catch (NumberFormatException e)
    {
      ;
    }

    throw new PropertyParseException(_INVALID_FONT_WEIGHT + value);
  }

  /**
   * Parses a CSS length value.
   *
   * @param value A CSS length value.
   * @return An Integer font size representing the length
   * @throws PropertyParseException Thrown if the specified
   *   value can not be parsed.
   */
  public static Integer parseLength(String value)
    throws PropertyParseException
  {
    // First, lose any extra white space
    value = _stripWhitespace(value);

    if (_isLength(value))
      return _parseLength(value);

    throw new PropertyParseException(_INVALID_LENGTH + value);
  }

  /**
   * Converts the specified Color to a valid CSS color value.
   */
  public static String getColorValue(Color color)
  {
    StringBuffer buffer = new StringBuffer(7);
    buffer.append('#');
    buffer.append(_getHexColorComponent(color.getRed()));
    buffer.append(_getHexColorComponent(color.getGreen()));
    buffer.append(_getHexColorComponent(color.getBlue()));

    return buffer.toString();
  }

  // Dinky utility to return a two digit hexidecimal color component
  private static String _getHexColorComponent(int colorComponent)
  {
    String hex = Integer.toString(colorComponent, 16);

    // Make sure hex value is two digits for "#RRGGBB" format
    if (hex.length() == 1)
      hex = "0" + hex;

    return hex;
  }

  // Parses a color component value.  Either 0-255, or 0-100%
  private static int _parseColorComponent(String value, String comp)
    throws PropertyParseException
  {
    if ((comp == null) || (comp.length() == 0))
      return 0;

    int col = 0;

    if (comp.endsWith("%"))
    {
      double percent = 0;

      // Handle percentage units
      try
      {
        percent = Double.parseDouble(comp.substring(0, comp.length() - 1));
      }
      catch (NumberFormatException e)
      {
        throw new PropertyParseException(_INVALID_COLOR + value);
      }

      col = (int)((percent/100.0) * 255);
    }
    else
    {
      try
      {
        col = Integer.parseInt(comp);
      }
      catch (NumberFormatException e)
      {
        throw new PropertyParseException(_INVALID_COLOR + value);
      }
    }

    if (col < 0)
      return 0;

    return (col > 255) ? 255 : col;
  }

  // Tests whether the value is specified in length units
  private static boolean _isLength(String value)
  {
    // Assume we have checked for null already
    assert (value != null);

    return (value.endsWith("in") ||
            value.endsWith("cm") ||
            value.endsWith("mm") ||
            value.endsWith("pt") ||
            value.endsWith("pc") ||
            value.endsWith("em") ||
            value.endsWith("ex") ||
            value.endsWith("px"));
  }

  // Tests whether the value is specified in percentage units
  private static boolean _isPercentage(String value)
  {
    // Assume we have checked for null already
    assert (value != null);

    return value.endsWith("%");
  }

  // Parses length units on an already stripped value
  private static Integer _parseLength(String value)
    throws PropertyParseException
  {
    // Assume we have already checked for length units
    assert (_isLength(value));

    // Parse out the size
    double size = 0;

    try
    {
      size = Double.parseDouble(value.substring(0, value.length() - 2));
    }
    catch (NumberFormatException e)
    {
      throw new PropertyParseException(_INVALID_LENGTH + value);
    }

    // Convert the size to "points".  This conversion isn't really valid,
    // since we don't check the screen resolution.  We just convert using
    // the assuming that 1pt = 1px, which of course isn't really the
    // case, but what are we going to do?
    int points = 0;

    if (value.endsWith("in"))
    {
      points = (int)(72 * size);
    }
    else if (value.endsWith("cm"))
    {
      points = (int)((72 * size)/2.54);
    }
    else if (value.endsWith("mm"))
    {
      points = (int)((72 * size)/25.4);
    }
    else if (value.endsWith("pt"))
    {
      points = (int)size;
    }
    else if (value.endsWith("pc"))
    {
      points = (int)(12 * size);
    }
    else if (value.endsWith("em"))
    {
      // We don't even try to figure out the right em size.  If you don't
      // like it, don't use em units for your image styles.
      points = (int)(12 * size);
    }
    else if (value.endsWith("ex"))
    {
      // We don't even try to figure out the right ex size.  If you don't
      // like it, don't use ex units for your image styles.
      points = (int)(6 * size);

    }
    else if (value.endsWith("px"))
    {
      points = (int)size;
    }
    else
    {
      throw new PropertyParseException(_INVALID_LENGTH + value);
    }

    return IntegerUtils.getInteger(points);
  }

  // Parses length units
  private static Integer _parsePercentage(String value)
    throws PropertyParseException
  {
    // Assume we have already checked for percentage units
    assert (_isPercentage(value));

    double percent = 0;

    try
    {
      percent = Double.parseDouble(value.substring(0, value.length() - 1));
    }
    catch (NumberFormatException e)
    {
      throw new PropertyParseException(_INVALID_PERCENTAGE + value);
    }

    // We just assume the percentage is relative to our base font size - 12pt.
    return IntegerUtils.getInteger((int)((percent/100.0) * 12));
  }

  private static Color _getSharedColor(int rgb)
  {
    Color sharedColor = (Color)_sColorCache.get(new Integer(rgb));

    if (sharedColor == null)
    {
      sharedColor = new Color(rgb);
      _sColorCache.put(new Integer(rgb), sharedColor);
    }

    return sharedColor;
  }

  private static Color _getSharedColor(int r, int g, int b)
  {
    return _getSharedColor(((r << 16) & 0xff0000) |
                           ((g << 8)  & 0x00ff00) |
                            (b        & 0x0000ff));
  }

  // Strips whitespace from start/end of the string
  private static String _stripWhitespace(String str)
  {
    if (str == null)
      return null;

    int length = str.length();
    int startIndex = 0;

    while (startIndex < length)
    {
      if (Character.isWhitespace(str.charAt(startIndex)))
        startIndex++;
      else
        break;
    }

    int endIndex = length;
    while (endIndex > 0)
    {
      if (Character.isWhitespace(str.charAt(endIndex - 1)))
        endIndex--;
      else
        break;
    }

    if ((startIndex == 0) && (endIndex == length))
      return str;

    if (endIndex <= startIndex)
      return null;

    return str.substring(startIndex, endIndex);
  }

  // CSS values
  private static final String _NORMAL_STYLE   = "normal";
  private static final String _ITALIC_STYLE   = "italic";
  private static final String _OBLIQUE_STYLE  = "oblique";
  private static final String _NORMAL_WEIGHT  = "normal";
  private static final String _BOLD_WEIGHT    = "bold";
  private static final String _BOLDER_WEIGHT  = "bolder";
  private static final String _LIGHTER_WEIGHT = "lighter";

  // Warning for invalid colors
  private static final String _INVALID_COLOR       = "Invalid color: ";
  private static final String _INVALID_FONT_SIZE   = "Invalid font size: ";
  private static final String _INVALID_FONT_STYLE  = "Invalid font style: ";
  private static final String _INVALID_FONT_WEIGHT = "Invalid font weight: ";
  private static final String _INVALID_LENGTH      = "Invalid length: ";
  private static final String _INVALID_PERCENTAGE  = "Invalid percentage: ";

  // We keep a cache of shared Color instances, hashed by RGB value, so
  // that we don't end up with one Color instance for each color in each
  // cache key in the Tecate image cache.
  private static final Map _sColorCache = 
    Collections.synchronizedMap(new LRUCache(50));

  // CSS named color values
  private static final Object[] _NAMED_COLORS = new Object[]
  {
    "black",   _getSharedColor(0x000000),
    "white",   _getSharedColor(0xffffff),
    "gray",    _getSharedColor(0x808080),
    "red",     _getSharedColor(0xff0000),
    "green",   _getSharedColor(0x008000),
    "blue",    _getSharedColor(0x0000ff),
    "yellow",  _getSharedColor(0xffff00),
    "aqua",    _getSharedColor(0x00ffff),
    "fuchsia", _getSharedColor(0xff00ff),
    "lime",    _getSharedColor(0x00ff00),
    "maroon",  _getSharedColor(0x800000),
    "navy",    _getSharedColor(0x000080),
    "olive",   _getSharedColor(0x808000),
    "purple",  _getSharedColor(0x800080),
    "silver",  _getSharedColor(0xc0c0c0),
    "teal",    _getSharedColor(0x008080)
  };

  // CSS2 system color names
  private static final Object[] _SYSTEM_COLORS = new Object[]
  {
    "activeborder",         Boolean.TRUE,
    "activecaption",        Boolean.TRUE,
    "appworkspace",         Boolean.TRUE,
    "background",           Boolean.TRUE,
    "buttonface",           Boolean.TRUE,
    "buttonhighlight",      Boolean.TRUE,
    "buttonshadow",         Boolean.TRUE,
    "buttontext",           Boolean.TRUE,
    "captiontext",          Boolean.TRUE,
    "graytext",             Boolean.TRUE,
    "highlight",            Boolean.TRUE,
    "highlighttext",        Boolean.TRUE,
    "inactiveborder",       Boolean.TRUE,
    "inactivecaption",      Boolean.TRUE,
    "inactivecaptiontext",  Boolean.TRUE,
    "infobackground",       Boolean.TRUE,
    "infotext",             Boolean.TRUE,
    "menu",                 Boolean.TRUE,
    "menutext",             Boolean.TRUE,
    "scrollbar",            Boolean.TRUE,
    "threeddarkshadow",     Boolean.TRUE,
    "threedface",           Boolean.TRUE,
    "threedhighlight",      Boolean.TRUE,
    "threedlightshadow",    Boolean.TRUE,
    "threedshadow",         Boolean.TRUE,
    "window",               Boolean.TRUE,
    "windowframe",          Boolean.TRUE,
    "windowtext",           Boolean.TRUE
  };

  // Values for absolute and relative font-size keywords
  private static final Object[] _NAMED_FONTS_SIZES = new Object[]
  {
    // Some comments on the sizes that we have chosen for absolute/relative
    // keywords.  We are using sizes which seem to make sense given that
    // the parsed font-sizes are only used for image generation purposes.
    // So, we could use's IE's odd default values for the absolute size
    // keywords, or we could use Netscape's default values.  Instead, we
    // are using reasonable defaults for image generation.  If you don't
    // like it, don't use absolute size keywords for the image-related
    // styles.  Also, we can't really handle the relative size keywords
    // as they are intended to be used, so we just use some reasonable
    // fixed defaults - it's better than throwing a PropertyParseException.
    // Again, if you don't like these values, don't use these keywords
    // for image-related styles!
    "xx-small", IntegerUtils.getInteger(8),
    "x-small",  IntegerUtils.getInteger(9),
    "small",    IntegerUtils.getInteger(10),
    "medium",   IntegerUtils.getInteger(12),
    "large",    IntegerUtils.getInteger(14),
    "x-large",  IntegerUtils.getInteger(16),
    "xx-large", IntegerUtils.getInteger(18),
    "smaller",  IntegerUtils.getInteger(10),
    "larger",   IntegerUtils.getInteger(14)
  };

}
