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
package org.apache.myfaces.trinidadinternal.util;

import java.io.UnsupportedEncodingException;

import java.net.URLDecoder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Parsed representation of a query string.
 * 
 * Although this class is thread-safe, the intention is that instances of QueryParam will be confined to
 * the current request thread.  (Also, the Servlet API requires that we expose parameter values as arrays, which
 * are inherently mutable and thus not-thread safe.)
 */
abstract public class QueryParams
{
  /**
   * Parses the specified query string.
   * 
   * @param queryString the query string to parse
   * @param encoding a character encoding suitable for use with URLDecoder.decode().
   * @return the parsed representation of the query string
   * @throws UnsupportedEncodingException if the encoding is not supported
   */
  public static QueryParams parseQueryString(String queryString, String encoding)
    throws UnsupportedEncodingException
  {
    return ((queryString == null) || (queryString.length() == 0))? EmptyQueryParams.getInstance() :
      new NonEmptyQueryParams(_parseNonEmptyQueryString(queryString, encoding));
  }

  /**
   * Returns an immutable map of query parameter name to value. In cases where a particular query parameter
   * appears multiple times in the query string, only the first valeu will appear in the returned map.
   * 
   * Suitable for use as an implementation of ExternalContext.getRequestParameterMap().
   */  
  abstract public Map<String, String> getParameterMap();

  /**
   * Returns an immutable map of query parameter name to (possibly multiple) values.
   * 
   * Suitable for use as an implementation of ExternalContext.getRequestParameterValuesMap().
   */  
  abstract public Map<String, String[]> getParameterValuesMap();

  private static Map<String, String[]> _parseNonEmptyQueryString(String queryString, String encoding)
    throws UnsupportedEncodingException
  {
    assert(queryString != null);
    assert(queryString.length() > 0);

    Map<String, String[]> parameterValuesMap = new HashMap<String, String[]>();

    String[] nameValuePairs = queryString.split(_URL_PARAM_SEPERATOR);
    for (String nameValuePair : nameValuePairs)
    {
      String[] nameAndValue = nameValuePair.split(_URL_NAME_VALUE_PAIR_SEPERATOR);
      String paramName = _decodeParamName(nameAndValue, encoding);
      String paramValue = _decodeParamValue(nameAndValue, encoding);
      __addParameterToMap(parameterValuesMap, paramName, paramValue);
    }

    return parameterValuesMap;
  }

  // Package-private for use by QueryParamsTest.
  static void __addParameterToMap(Map<String, String[]> map, String name, String value)
  {
    String[] oldValues = map.get(name);
    String[] newValues;

    if (oldValues == null)
    {
      newValues = new String[] { value };
    }
    else
    {
      List<String> newValuesList = new ArrayList<String>(Arrays.<String>asList(oldValues));
      newValuesList.add(value);
      newValues = newValuesList.toArray(new String[oldValues.length + 1]);
    }
    
    map.put(name, newValues);
  }

  private static String _decodeParamName(String[] nameAndValue, String encoding)
    throws UnsupportedEncodingException
  {
    assert (nameAndValue.length > 0);
    return URLDecoder.decode(nameAndValue[0], encoding);
  }

  private static String _decodeParamValue(String[] nameAndValue, String encoding)
    throws UnsupportedEncodingException
  {
    return (nameAndValue.length > 1) ? URLDecoder.decode(nameAndValue[1], encoding) : "";
  }

  private final static class NonEmptyQueryParams extends QueryParams
  {
    public NonEmptyQueryParams(Map<String, String[]> parameterValuesMap)
    {
      _parameterValuesMap = Collections.unmodifiableMap(parameterValuesMap);
      _parameterMap = _toSingleValueParameterMap(parameterValuesMap);
    }

    @Override
    public Map<String, String> getParameterMap()
    {
      return _parameterMap;
    }

    @Override
    public Map<String, String[]> getParameterValuesMap()
    {
      return _parameterValuesMap;
    }
    
    private static final Map<String, String> _toSingleValueParameterMap(Map<String, String[]> parameterValuesMap)
    {
      Map<String, String> singleValueMap = new HashMap<String, String>();
      
      for (Map.Entry<String, String[]> multiValueParam : parameterValuesMap.entrySet())
      {
        String[] multiValue = multiValueParam.getValue();
        assert(multiValue != null);
        assert(multiValue.length > 0);
        
        singleValueMap.put(multiValueParam.getKey(), multiValue[0]);
      }

      return Collections.unmodifiableMap(singleValueMap);
    }
    
    private final Map<String, String[]> _parameterValuesMap;
    private final Map<String, String> _parameterMap;
  }

  private static class EmptyQueryParams extends QueryParams
  {
    public static QueryParams getInstance()
    {
      return _INSTANCE;
    }

    @Override
    public Map<String, String> getParameterMap()
    {
      return Collections.emptyMap();
    }

    @Override
    public Map<String, String[]> getParameterValuesMap()
    {
      return Collections.emptyMap();
    }
    
    private EmptyQueryParams()
    {
    }
    
    private static final QueryParams _INSTANCE = new EmptyQueryParams();
  }

  private QueryParams()
  {
  }

  private static final String _URL_PARAM_SEPERATOR = "&";
  private static final String _URL_NAME_VALUE_PAIR_SEPERATOR = "=";
}
