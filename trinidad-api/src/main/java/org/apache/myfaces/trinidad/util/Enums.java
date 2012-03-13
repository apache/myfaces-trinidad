/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidad.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.Map;

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Enum-related utilities.
 */
public final class Enums
{
  /**
   * Single abstract method interface for converting Enum
   * constant values to strings.
   */
  public static interface StringProducer<E extends Enum>
  {
    /**
     * Returns a string representation for an enum constant.
     * @param enumValue the enum constant value to convert to a string
     */
    String toString(E enumValue);
  }
  
  /**
   * Returns a map of String -> enum constant for all constants
   * in the provided enum class.
   * 
   * This is intended for use by Enum implementations that wish to
   * provide Enum.valueOf(String)-like functionality, but for String
   * values other than the canonical enum constant names.
   * 
   * The returned Map is not synchronized but can be used in multithreaded
   * environments as long as it is a) not modified and b) safely published.
   * 
   * @param enumClass the enum class from which the map will be created
   * @param keyProducer called for each enum constant to produce the string
   *   key for the map.
   */
  public static <E extends Enum> Map<String, E> createStringKeyMap(
     Class<E>          enumClass,
     StringProducer<E> keyProducer
    )
  {
    E[] enumValues = enumClass.getEnumConstants();
    Map<String, E> stringKeyMap = new ArrayMap<String, E>(enumValues.length);
    
    for (E enumValue : enumValues)
    {
      stringKeyMap.put(keyProducer.toString(enumValue), enumValue);
    }

    return Collections.unmodifiableMap(stringKeyMap);
  }
  
  /**
   * Looks up an Enum value in a String -> Enum map and
   * throws an IllegalArgumentException if a value
   * is not found.
   * 
   * @param stringToEnumMap the map containing String -> enum mappings
   * @param key the string value to look up in the map
   * @param enumClass the target enum class
   * @return a non-null Enum value corresponding to the key.
   * @throws IllegalArgumentException if no value is found.
   */
  public static <T extends Enum> T stringToEnum(
    Map<String, T> stringToEnumMap,
    String         key,
    Class<T>       enumClass
    )
  {
    T enumValue = stringToEnumMap.get(key);
    
    if (enumValue == null)
    {
      String message = _LOG.getMessage("ILLEGAL_ENUM_VALUE", 
                                       new Object[] { enumClass.getName(), key});
      throw new IllegalArgumentException(message);
    }
    
    return enumValue;
  }

  /**
   * Returns a string suitable for use in a regular expression
   * pattern that matches all values of the specified enum.
   * 
   * @param enumClass the enum class from which the pattern
   *   will be derived.
   * @param patternProducer called for each enum constant value
   *   to produce the string representation to use in the pattern.
   */
  public static <E extends Enum> String patternOf(
    Class<E>          enumClass,
    StringProducer<E> patternProducer
    )
  {
    E[] enumValues = enumClass.getEnumConstants();
    StringBuilder builder = new StringBuilder();
    builder.append("(");
    
    Iterator<E> iter = Arrays.asList(enumValues).iterator();

    while (iter.hasNext())
    {
      E enumValue = iter.next();
      String enumPattern = patternProducer.toString(enumValue);
      builder.append(enumPattern);
      
      if (iter.hasNext())
      {
        builder.append("|");
      }
    }

    builder.append(")");
    
    return builder.toString();    
  }
  
  /**
   * Single abstract method interface for parsing enum
   * constant values from strings.
   */
  public static interface EnumParser<E extends Enum>
  {
    /**
     * Returns the enum constant associated with the specified
     * string.
     * @param value the string value to parse
     * @throws IllegalArgumentException if the value does not
     *   correspond to some enum constant.
     */
    E parse(String value) throws IllegalArgumentException;
  }

  /**
   * Parses a collection of strings into a corresponding collection
   * of enum constant values.
   *
   * The returned Collection is not synchronized but can be used in multithreaded
   * environments as long as it is a) not modified and b) safely published.
   * 
   * @param valuesToParse string values to parse
   * @param enumClass target enum class
   * @param defaultValue an enum value to include in the parsed collection if
   *   valueToParse is empty.
   * @param enumParser called for each string value to convert to an enum constant.
   * 
   * @throws EnumParseException if the EnumParser fails to parse one of the values.
   */
  public static <E extends Enum> Collection<E> parseEnumValues(
    Collection<String> valuesToParse,
    Class<E>           enumClass,
    EnumParser<E>      enumParser,
    E                  defaultValue
    ) throws EnumParseException
  {
    Collection<E> enumValues = EnumSet.noneOf(enumClass);
    
    for (String valueToParse : valuesToParse)
    {
      try
      {
        enumValues.add(enumParser.parse(valueToParse));
      }
      catch (IllegalArgumentException e)
      {
        // We translate to an EnumParseException so that the caller can
        // determine which of the values failed to parse (ie. so that the
        // caller can provide a more meaningful error message).
        throw new EnumParseException(e.getMessage(), valueToParse);
      }
    }
    
    if (enumValues.isEmpty() && defaultValue != null)
    {
      enumValues.add(defaultValue);
    }
    
    return enumValues;
  }

  /**
   * Parses a request parameter into a collection of enum constant values.
   *
   * The returned Collection is not synchronized but can be used in multithreaded
   * environments as long as it is a) not modified and b) safely published.
   *
   * @param external the ExternalContext that defines the request parameter values
   * @param paramName the name of the request parameter to parse
   * @param enumClass target enum class
   * @param defaultValue an enum value to include in the parsed collection if
   *   valuesToParse is empty.
   * @param enumParser called for each string value to convert to an enum constant.
   *
   * @throws EnumParseException if the EnumParser fails to parse one of the values.
   */
  public static <E extends Enum> Collection<E> parseEnumRequestParameter(
    ExternalContext external,
    String          paramName,
    Class<E>        enumClass,
    EnumParser<E>   enumParser,
    E               defaultValue
    ) throws EnumParseException
  {
    Collection<String> paramValues = _getRequestParamValues(external, paramName);
    return Enums.parseEnumValues(paramValues, enumClass, enumParser, defaultValue);
  }

  // Returns a non-null collection of values for the specified
  // request parameter.
  private static Collection<String> _getRequestParamValues(
    ExternalContext external,
    String          paramName
    )
  {
    Map<String, String[]> requestParams = external.getRequestParameterValuesMap();
    String[] paramValues = requestParams.get(paramName);
    
    if ((paramValues == null) || (paramValues.length == 0))
    {
      return Collections.emptyList();
    }

    return Arrays.asList(paramValues);
  }

  /**
   * Convenience method for creating String -> enum map where the String keys
   * are produced by calling the displayName() method on each enum constant.
   * 
   * @see #createStringKeyMap
   */
  public static <E extends Enum> Map<String, E> createDisplayNameMap(Class<E> enumClass)
  {
    return createStringKeyMap(enumClass, displayNameStringProducer(enumClass)); 
  }
  
  /**
   * Returns a StringProducer that converts enum constants to String values by
   * calling displayName() on each enum constant.
   * 
   * This implementation is not especially efficient, as it uses method reflection,
   * but should be sufficient for 1-time initialization cases.
   * 
   * @param enumClass the class of the enum for which we want to produce strings.  This
   *   class must provide a public displayName() method.
   *   
   * @throws IllegalArgumentException if the enum class does not provide a publicly
   *   accessible displayName() method.
   */
  public static <E extends Enum> StringProducer<E> displayNameStringProducer(Class<E> enumClass)
    throws IllegalArgumentException
  {
    return methodNameStringProducer(enumClass, "displayName");
  }

  /**
   * Returns an EnumParser that converts String values to enum constants by
   * calling valueOfDisplayName() on the enum class.
   * 
   * @param enumClass the target class to which String values are parsed.  This
   *   class must provide a public static valueOfDisplayName(String) method.
   *
   * @throws IllegalArgumentException if the enum class does not provide a publicly
   *   accessible valueOfDisplayName(String) method.
   */
  public static <E extends Enum> EnumParser<E> displayNameEnumParser(Class<E> enumClass)
  {
    return methodNameEnumParser(enumClass, "valueOfDisplayName");    
  }

  /**
   * Returns a StringProducer uses reflection to produce Strings from
   * enum constant values.
   * 
   * @param enumClass the enum class on which the StringProducer operates
   * @param methodName the name of the method which the StringProducer invokes
   * 
   * @throws IllegalArgumentException if the method specified by the 
   *   methodName argument does not exist.
   */
  public static <E extends Enum> StringProducer<E> methodNameStringProducer(
    Class<E> enumClass,
    String   methodName
    ) throws IllegalArgumentException
  {
    final Method method = _getMethod(enumClass, methodName);
      
    return new StringProducer<E>()
    {
      @Override
      public String toString(E enumValue)
      {
        return (String)_invokeMethod(method, enumValue);
      }
    };
  }
  
  /**
   * Returns an EnumParser that uses reflection to invoke a method
   * on an enum class to translate String values to enum constants.
   *
   * @param enumClass the target enum class
   * @param methodName the name of the method to invoke
   * 
   * @throws IllegalArgumentException if the method specified by the 
   *   methodName argument does not exist.
   */
  public static <E extends Enum> EnumParser<E> methodNameEnumParser(
    final Class<E> enumClass,
    String   methodName
    ) throws IllegalArgumentException
  {
    final Method method = _getMethod(enumClass, methodName, String.class);
    
    return new EnumParser<E>()
    {
      public E parse(String value)
      {
        return (E)_invokeMethod(method, enumClass, value);
      }
    };
  }
  
  // Wrapper for Class.getMethod() that adds exception handling.
  // Throws IllegalArgumentException if the Method cannot be retreived.
  private static Method _getMethod(
    Class<?>    aClass,
    String      methodName,
    Class<?>... parameterTypes
    ) throws IllegalArgumentException
  {
    try
    {
      return aClass.getMethod(methodName, parameterTypes);
    }
    catch (Exception e)
    {
      throw new IllegalArgumentException(e);
    }
  }
  
  // Wrapper for Method.invoke() that adds exception handling.  Checked
  // exceptions are propagated out as runtime exceptions.
  private static Object _invokeMethod(
    Method    method,
    Object    obj,
    Object... args
    )
  {
    try
    {
      return method.invoke(obj, args);
    }
    catch (InvocationTargetException ite)
    {
      Throwable targetException = ite.getTargetException();
      
      // If the method that we invoked threw an IllegalArgumentException,
      // we want to explicitly pass this exception back out since this is
      // significant for callers of EnumParser.parse().
      if (targetException instanceof IllegalArgumentException)
      {
        throw (IllegalArgumentException)targetException;
      }
      else
      {
        throw new RuntimeException(ite);
      }
    }
    catch (Exception e)
    {
      throw new RuntimeException(e);    
    }
  }

  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(Enums.class);
}
