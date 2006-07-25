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
package org.apache.myfaces.adfinternal.share.expl;

import java.awt.Color;
import java.lang.reflect.Array;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.util.Date;
import java.util.List;

import org.apache.myfaces.adf.util.ClassLoaderUtils;

import org.apache.myfaces.adfinternal.util.IntegerUtils;

import org.apache.myfaces.adfinternal.share.text.ColorFormat;
import org.apache.myfaces.adfinternal.share.text.RGBColorFormat;

import org.apache.myfaces.adfinternal.share.xml.NamespaceURI;
import org.apache.myfaces.adfinternal.share.xml.XMLUtils;

import org.apache.myfaces.adfinternal.style.Style;
import org.apache.myfaces.adfinternal.style.util.CSSUtils;

/**
 * Coercions is a utility class to coerce values to their target type.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/expl/Coercions.java#0 $) $Date: 10-nov-2005.19:00:11 $
 * @author The Oracle ADF Faces Team
 */
public final class Coercions
{

  /**
   * coerce a String into the specified type
   * @param text the string to be coerced
   * @param type the required type
   */
  public static Object coerce(ExpressionContext context,
                              String text,
                              Class type) throws IllegalArgumentException
  {
    if (type != null)
    {
      if (type == Object.class)
      {
        return text;
      }
      // Turn the type into a Boolean
      else if ((type == Boolean.class) || (type == Boolean.TYPE))
      {
        return toBoolean(text);
      }
      else if ((type == Byte.class) || (type == Byte.TYPE))
      {
        return toByte(text);
      }
      else if ((type == Short.class) || (type == Short.TYPE))
      {
        return toShort(text);
      }
      else if ((type == Integer.class) || (type == Integer.TYPE))
      {
        return toInteger(text);
      }
      else if ((type == Long.class) || (type == Long.TYPE))
      {
        return toLong(text);
      }
      else if ((type == Float.class) || (type == Float.TYPE))
      {
        return toFloat(text);
      }
      else if ((type == Double.class) || (type == Double.TYPE))
      {
        return toDouble(text);
      }
      else if ((type == Character.class) || (type == Character.TYPE))
      {
        Character c = toCharacter(text);

        if (text != null && c == null)
        {
          throw new IllegalArgumentException(
            text + " is not a character");
        }

        return c;
      }
      else if (type == String.class)
      {
        return text;
      }
      else if ((type == int[].class))
      {
        String[] array = XMLUtils.parseNameTokens(text);
        int[] ints = new int[array.length];
        for (int i = 0; i < array.length; i++)
        {
          ints[i] = Integer.parseInt(array[i]);
        }

        return ints;
      }
      else if ((type == Integer[].class))
      {
        String[] array = XMLUtils.parseNameTokens(text);
        Integer[] ints = new Integer[array.length];
        for (int i = 0; i < array.length; i++)
        {
          ints[i] = IntegerUtils.getInteger(Integer.parseInt(array[i]));
        }

        return ints;
      }
      else if ((type == String[].class))
      {
        return XMLUtils.parseNameTokens(text);
      }
      // For Dates, try to parse it as an ISO 8601 Date.
      // If that fails, simply let the string pass through;  for
      // DateFieldBean, this works, and it really should for
      // all date-accepting components.
      else if (type == Date.class)
      {
        try
        {
          return _ISO_DATE_FORMAT.parse(text);
        }
        catch (ParseException pe)
        {
          return text;
        }
      }
      // For Colors, try to parse it as "#FFFFFF" format.
      // If that fails, simply let the string pass through;  for
      // ColorFieldBean, this works, and it really should for
      // all color-accepting components.
      else if (type == Color.class)
      {
        return toColor(text);
      }
      else if ((type == NamespaceURI.class))
      {
        return NamespaceURI.create(context, text, "");
      }
      else if (type == Style.class)
      {
        return CSSUtils.parseStyle(text);
      }
      else if (type == Class.class)
      {
        try
        {
          return ClassLoaderUtils.loadClass(text);
        }
        catch (ClassNotFoundException cnfe)
        {
          throw new IllegalArgumentException("Could not find class " + text);
        }
      }
      else if (type == Object.class)
      {
        return text;
      }
      throw new IllegalArgumentException(text + " cannot be parsed into a " +
                                         type.getName());
    }

    throw new NullPointerException("type is null");
  }

  /**
   * coerces an instance into an instance of the given type
   */
  public static Object coerce(Object value, Class type)
  {
    if (type != null)
    {
      if (value == null)
        return null;

      if (type.isAssignableFrom(value.getClass()))
        return value;

      if (type == Object.class)
      {
        return value;
      }
      // Turn the type into a Boolean
      else if ((type == Boolean.class) || (type == Boolean.TYPE))
      {
        return toBoolean(value);
      }
      else if ((type == Byte.class) || (type == Byte.TYPE))
      {
        return toByte(value);
      }
      else if ((type == Short.class) || (type == Short.TYPE))
      {
        return toShort(value);
      }
      else if ((type == Integer.class) || (type == Integer.TYPE))
      {
        return toInteger(value);
      }
      else if ((type == Long.class) || (type == Long.TYPE))
      {
        return toLong(value);
      }
      else if ((type == Float.class) || (type == Float.TYPE))
      {
        return toFloat(value);
      }
      else if ((type == Double.class) || (type == Double.TYPE))
      {
        return toDouble(value);
      }
      else if ((type == Character.class) || (type == Character.TYPE))
      {
        return toCharacter(value);
      }
      else if (type == String.class)
      {
        return toString(value);
      }
      else if (type.isArray()) // see bug 3234064.
      {
        // we support three types of array coercions.
        // 1. coerce a List into an array.
        // 2. coerce a single value into an array of size 1.
        // 3. coerce an array into an array.

        Class arrayType = type.getComponentType();
        Object res = Array.newInstance(arrayType, 1);

        if (value instanceof List)
        {
          // we probably should coerce each element of this list; however,
          // let's not worry about it until we have a requirement:
          res = ((List) value).toArray((Object[]) res);
        }
        else if (value.getClass().isArray())
        {
          // we probably should coerce each element of this array; however,
          // let's not worry about it until we have a requirement:
          res = value;
        }
        else
        {
          Object arrayValue = coerce(value, arrayType);
          Array.set(res, 0, arrayValue);
        }
        return res;
      }

      throw new IllegalArgumentException("Could not coerce value of type " +
                                         value.getClass() +
                                         " into type " + type.getName());
    }

    throw new NullPointerException("type is null");
  }

  public static Boolean toBoolean(
    Object value)
  {
    if (value != null)
    {
      if (value instanceof Boolean)
      {
        return (Boolean)value;
      }
      else
      {
        return Boolean.valueOf("true".equals(value.toString()));
      }
    }

    return null;
  }

  public static Character toCharacter(
    Object value)
  {
    if (value != null)
    {
      if (value instanceof Character)
      {
        return (Character)value;
      }
      else if (value instanceof String)
      {
        String valueText = value.toString();
        if (valueText.length() != 0)
        {
          return new Character(valueText.charAt(0));
        }
      }
    }

    return null;
  }

  public static Byte toByte(
    Object value)
  {
    if (value != null)
    {
      if (value instanceof Byte)
      {
        return (Byte)value;
      }
      else if (value instanceof Number)
      {
        return new Byte(((Number)value).byteValue());
      }
      else
      {
        byte byteValue = Byte.parseByte(value.toString());
        return new Byte(byteValue);
      }
    }

    return null;
  }

  public static Short toShort(
    Object value)
  {
    if (value != null)
    {
      if (value instanceof Short)
      {
        return (Short)value;
      }
      else if (value instanceof Number)
      {
        return new Short(((Number)value).shortValue());
      }
      else
      {
        short shortValue = Short.parseShort(value.toString());
        return new Short(shortValue);
      }
    }

    return null;
  }

  public static Integer toInteger(
    Object value)
  {
    if (value != null)
    {
      // Turn the type into an Integer.
      //
      // Technically, this is irrelevant for our rendering, since we
      // just turn the things back into strings.  But this does help
      // in two small ways.  First off, you can use the Bean static
      // functions to get at the value without nasty casting.  Second,
      // since we're using shared Integer objects, while these String
      // objects aren't shared at all (unless we intern'd them), this
      // gives us lower memory use.
      //
      if (value instanceof Integer)
      {
        return (Integer)value;
      }
      else if (value instanceof Number)
      {
        return IntegerUtils.getInteger(((Number)value).intValue());
      }
      else
      {
        int intValue = Integer.parseInt(value.toString());
        return IntegerUtils.getInteger(intValue);
      }
    }

    return null;
  }

  public static Long toLong(
    Object value)
  {
    if (value != null)
    {
      if (value instanceof Long)
      {
        return (Long)value;
      }
      else if (value instanceof Number)
      {
        return new Long(((Number)value).longValue());
      }
      else
      {
        long longValue = Long.parseLong(value.toString());
        return new Long(longValue);
      }
    }

    return null;
  }

  public static Float toFloat(
    Object value)
  {
    if (value != null)
    {
      if (value instanceof Float)
      {
        return (Float)value;
      }
      else if (value instanceof Number)
      {
        return new Float(((Number)value).floatValue());
      }
      else
      {
        float floatValue = Float.parseFloat(value.toString());
        return new Float(floatValue);
      }
    }

    return null;
  }

  public static Double toDouble(
    Object value)
  {
    if (value != null)
    {
      if (value instanceof Double)
      {
        return (Double)value;
      }
      else if (value instanceof Number)
      {
        return new Double(((Number)value).doubleValue());
      }
      else
      {
        double doubleValue = Double.parseDouble(value.toString());
        return new Double(doubleValue);
      }
    }

    return null;
  }

  public static String toString(
    Object value)
  {
    if (value != null)
    {
      return value.toString();
    }

    return null;
  }

  public static Color toColor(
    Object value)
  {
    if (value != null)
    {
      if (value instanceof Color)
      {
        return (Color)value;
      }
      else if (value instanceof Number)
      {
        return new Color(((Number)value).intValue(), true);
      }
      else if (value instanceof String)
      {
        try
        {
          String colorString = (String)value;
          // ParsedColor returns original string for toString()
          if ("#trans".equalsIgnoreCase(colorString))
          {
            return new ParsedColor(_TRANSPARENT_COLOR, colorString);
          }
          else if (colorString.length() == 7)  // "#RRGGBB".length() == 7
          {
            return new ParsedColor(_COLOR_FMT.parse(colorString), colorString);
          }
        }
        catch (ParseException e)
        {
          // ignore, iae thrown below for general failure case
          ;
        }
      }

      throw new IllegalArgumentException(value + " cannot be coerced into a " +
                                         "java.awt.Color");
    }

    return null;
  }

  // no instances
  private Coercions()
  {
  }

  // internal Color class that will return the original string
  // value for toString.
  private static final class ParsedColor extends Color
  {
    public ParsedColor(Color color, String toString)
    {
      super(color.getRed(),
            color.getGreen(),
            color.getBlue(),
            color.getAlpha());

      _toString = toString;
    }

    public String toString()
    {
      return _toString;
    }

    private String _toString;
  }

  private static final ColorFormat _COLOR_FMT = new RGBColorFormat("#RRGGBB");
  private static final Color _TRANSPARENT_COLOR = new Color(0,0,0,0);

  // We rely strictly on ISO 8601 formats
  private static DateFormat  _ISO_DATE_FORMAT =
    new SimpleDateFormat("yyyy-MM-dd");
}
