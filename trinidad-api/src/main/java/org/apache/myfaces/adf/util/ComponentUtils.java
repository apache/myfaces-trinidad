/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adf.util;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Utility functions used by the ADF Faces components.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/util/ComponentUtils.java#0 $) $Date: 10-nov-2005.19:08:37 $
 * @author The Oracle ADF Faces Team
 */
public class ComponentUtils
{
  private ComponentUtils()
  {
  }

  /**
   * Utility method for component code that resolves an Object,
   * returning a default value if the value is null.
   */
  public static Object resolveObject(
    Object value, 
    Object defaultValue
    )
  {
    return (value != null)
             ? value
             : defaultValue;
  }


  /**
   * Utility method for component code that transforms Object->boolean.
   */
  public static boolean resolveBoolean(
    Object  value, 
    boolean defaultValue
    )
  {
    if (defaultValue)
      return !Boolean.FALSE.equals(value);
    else
      return Boolean.TRUE.equals(value);
  }

  /**
   * Utility method for component code that transforms Object->boolean.
   */
  public static boolean resolveBoolean(
    Object value
    )
  {
    return Boolean.TRUE.equals(value);
  }


  /**
   * Utility method for component code that transforms an Object
   * (which must be a java.lang.Number) into an int.
   */
  public static int resolveInteger(
    Object value
    )
  {
    return resolveInteger(value, 0);
  }

  /**
   * Utility method for component code that transforms an Object
   * (which must be a java.lang.Number) into an int.
   */
  public static int resolveInteger(
    Object value,
    int     defaultValue
    )
  {
    return (value != null)
             ? ((Number)value).intValue()
             : defaultValue;
  }


  /**
   * Utility method for component code that transforms an Object
   * (which must be a java.lang.Number) into a long.
   */
  public static long resolveLong(
    Object value
    )
  {
    return resolveLong(value, 0);
  }

  /**
   * Utility method for component code that transforms an Object
   * (which must be a java.lang.Number) into a long.
   */
  public static long resolveLong(
    Object value,
    long   defaultValue
    )
  {
    return (value != null)
             ? ((Number)value).longValue()
             : defaultValue;
  }


  /**
   * Utility method for component code that transforms Character->character.
   */
  public static char resolveCharacter(
    Character value
    )
  {
    return resolveCharacter(value, '\u0000');
  }

  /**
   * Utility method for component code that transforms Long->long.
   */
  public static char resolveCharacter(
    Character  value,
    char       defaultValue
    )
  {
    return (value != null)
             ? value.charValue()
             : defaultValue;
  }


  /**
   * Utility method for component code that transforms Object->String.
   */
  public static String resolveString(
    Object  value
    )
  {
    return (value != null)
             ? value.toString()
             : null;
  }


  /**
   * Utility method for component code that transforms Object->String.
   */
  public static String resolveString(
    Object  value,
    String  defaultValue
    )
  {
    return (value != null)
             ? value.toString()
             : defaultValue;
  }
  
  /**
   * Utility method for code that transforms Object->String[]
   */
  public static String[] resolveStringArray(
    Object value)
  {
    return resolveStringArray(value, null);
  }
  
  /**
   * Utility method for code that transforms Object->String[]
   */
  public static String[] resolveStringArray(
    Object value,
    String[] defaultValue)
  {
    return (value != null)
             ? (String[]) value
             : defaultValue;
  }
  
  /**
   * Utility method for code that transforms Object->Date
   */
  public static Date resolveDate(Object value)  
  {
    return resolveDate(value, null);
  }
  
  /**
   * Utility method for code that transforms Object->Date
   */
  public static Date resolveDate(
    Object value,
    Date defaultValue)
  {
    return (value != null)
             ? (Date) value
             : defaultValue;
  }  
  
  public static TimeZone resolveTimeZone(
    Object value
    )
  {
    return resolveTimeZone(value, null);
  }
  
  public static TimeZone resolveTimeZone(
    Object value,
    TimeZone defaultValue
    )
  {
    return (value != null)
             ? (TimeZone) value
             : defaultValue;
  }
  
  public static Locale resolveLocale(
    Object value
    )
  {
    return resolveLocale(value, null);
  }
  
  public static Locale resolveLocale(
    Object value,
    Locale defaultValue
    )
  {
    return (value != null)
             ? (Locale) value
             : defaultValue;
  }
  
  /**
   * Gets the root cause of an exception.
   * Keeps unwrapping the given throwable until the root cause is found.
   */
  public static Throwable unwrap(Throwable t)
  {
    while(true)
    {
      Throwable unwrap = t.getCause();
      if (unwrap == null)
        break;
      t = unwrap;
    }
    return t;
  }
}
