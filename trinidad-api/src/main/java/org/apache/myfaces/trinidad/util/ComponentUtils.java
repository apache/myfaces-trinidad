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
package org.apache.myfaces.trinidad.util;

import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Utility functions used by the Apache Trinidad components.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/util/ComponentUtils.java#0 $) $Date: 10-nov-2005.19:08:37 $
 */
public final class ComponentUtils
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
   * Utility method for component code that transforms an Object
   * (which must be a java.lang.Number) into a double.
   */
  public static double resolveDouble(
    Object value
    )
  {
    return resolveDouble(value, 0);
  }

  /**
   * Utility method for component code that transforms an Object
   * (which must be a java.lang.Number) into a double.
   */
  public static double resolveDouble(
    Object value,
    double   defaultValue
    )
  {
    return (value != null)
             ? ((Number)value).doubleValue()
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
   * Utility method for component code that transforms Character->character.
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
   * Utility method for component code that transforms Object->Number.
   */
  public static Number resolveNumber(
    Object  value
    )
  {
    return resolveNumber(value, null);
  }


  /**
   * Utility method for component code that transforms Object->Number.
   */
  public static Number resolveNumber(
    Object  value,
    Number  defaultValue
    )
  {
    return (value != null)
             ? (Number) value
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


  /**
   * Find a component relative to another.
   * <p>
   * The relative ID must account for NamingContainers. If the component is already inside
   * of a naming container, you can use a single colon to start the search from the root, 
   * or multiple colons to move up through the NamingContainers - "::" will 
   * pop out of the current naming container, ":::" will pop out of two
   * naming containers, etc.
   * </p>
   * 
   * @param from the component to search relative to
   * @param scopedId the relative id path from the 'from' component to the
   *                 component to find
   * @return the component if found, null otherwise
   * @see org.apache.myfaces.trinidad.render.RenderUtils#getRelativeId
   * @see javax.faces.component.UIComponent#findComponent
   */
  public static UIComponent findRelativeComponent(
    UIComponent from,
    String      scopedId)
  {
    if (from == null)
        return null;
    UIComponent originalFrom = from;
    String originalRelativeId = scopedId;
    
    int idLength = scopedId.length();
    // Figure out how many colons
    int colonCount = 0;
    while (colonCount < idLength)
    {
      if (scopedId.charAt(colonCount) != NamingContainer.SEPARATOR_CHAR)
        break;
      colonCount++;
    }

    // colonCount == 0: fully relative
    // colonCount == 1: absolute (still normal findComponent syntax)
    // colonCount > 1: for each extra colon after 1, pop out of
    // the naming container (to the view root, if naming containers run out)
    if (colonCount > 1)
    {
      scopedId = scopedId.substring(colonCount);
      
      // if the component is not a NamingContainer, then we need to 
      // get the component's naming container and set this as the 'from'.
      // this way we'll pop out of the component's 
      // naming container if there is are multiple colons.
      if (!(from instanceof NamingContainer))
      {
        from = _getParentNamingContainer(from);
      }
      
      // pop out of the naming containers if there are multiple colons
      for (int j = 1; j < colonCount; j++)
      {
        from = _getParentNamingContainer(from);
      }
    }

    UIComponent found = from.findComponent(scopedId);
    if (found != null)
      return found;
    else
    {
      // try the old way for backward compatability as far as it differed,
      // which is only if the 'from' was not a NamingContainer.
      if (!(originalFrom instanceof NamingContainer))
        return _findRelativeComponentDeprecated(originalFrom, originalRelativeId);
      else
        return null;
    }
    
  }
  
  // given a component, get its naming container. If the component
  // is a naming container, it will get its naming container.
  // if no parent naming containers exist, it stops at the ViewRoot.
  private static UIComponent _getParentNamingContainer (
    UIComponent from)
  {
    while (from.getParent() != null)
    {
      from = from.getParent();
      if (from instanceof NamingContainer)
        break;
    }
    return from;
  }

   /**
    * Find a component relative to another.
    * This method is the same as the 'old' public findRelativeComponent.
   * This method is around so that the
   * new findRelativeComponent method is backward compatibility.
    * <p>
    * The relative ID must account for NamingContainers. If the component is already inside
    * of a naming container, you can use a single colon to start the search from the root, 
    * or multiple colons to move up through the NamingContainers - "::" will search from 
    * the parent naming container, ":::" will search from the grandparent 
    * naming container, etc.
    * </p>
    * 
    * @param from the component to search relative to
    * @param relativeId the relative path to the component to find
    * @return the component if found, null otherwise
    */
  private static UIComponent _findRelativeComponentDeprecated(
    UIComponent from,
    String      relativeId)
  {  
    UIComponent originalFrom = from;
    String originalRelativeId = relativeId;

    int idLength = relativeId.length();
    // Figure out how many colons
    int colonCount = 0;
    while (colonCount < idLength)
    {
      if (relativeId.charAt(colonCount) != NamingContainer.SEPARATOR_CHAR)
        break;
      colonCount++;
    }

    // colonCount == 0: fully relative
    // colonCount == 1: absolute (still normal findComponent syntax)
    // colonCount > 1: for each extra colon after 1, go up a naming container
    // (to the view root, if naming containers run out)
    if (colonCount > 1)
    {
      relativeId = relativeId.substring(colonCount);
      for (int j = 1; j < colonCount; j++)
      {
        while (from.getParent() != null)
        {
          from = from.getParent();
          if (from instanceof NamingContainer)
            break;
        }
      }
    }

    UIComponent found = from.findComponent(relativeId);
    if (found != null)
    {
      _LOG.warning("DEPRECATED_RELATIVE_ID_SYNTAX", 
        new Object[] {originalRelativeId, originalFrom});
    }
    return found;
  }
  
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(ComponentUtils.class);
}
