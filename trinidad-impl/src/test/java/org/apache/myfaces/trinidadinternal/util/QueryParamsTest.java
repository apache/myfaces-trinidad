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

import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

/**
 * Unit tests for QueryParams
 */
public class QueryParamsTest
{
  @Test
  public void testNullQueryString()
    throws UnsupportedEncodingException
  {
    _testNullOrEmpty(null);
  }

  @Test
  public void testEmptyQueryString()
    throws UnsupportedEncodingException
  {
    _testNullOrEmpty("");
  }
  
  @Test
  public void testNonEmptyQueryStrings()
    throws UnsupportedEncodingException
  {
    _parseAndCompare("foo=bar", "foo", "bar");
    _parseAndCompare("foo=bar1&foo=bar2", "foo", "bar1", "foo", "bar2");
    _parseAndCompare("a=1&a=2&b=3&b=4&c=5", "a", "1", "a", "2", "b", "3", "b", "4", "c", "5");
    _parseAndCompare("foo", "foo", "");
    _parseAndCompare("foo=", "foo", "");
  }
  
  @Test
  public void testEncodedQueryStrings()
    throws UnsupportedEncodingException
  {
    _parseAndCompare("foo=bar1+bar2", "foo", "bar1 bar2");
    _parseAndCompare("foo+bar=baz", "foo bar", "baz");
    _parseAndCompare("foo=%C3%A9", "foo", "\u00e9");
  }

  @Test(expected=UnsupportedEncodingException.class)
  public void testUnsupportedEncoding()
    throws UnsupportedEncodingException
  {
    QueryParams.parseQueryString("foo=%C3%A9", "UTF-FOO");
  }

  private void _testNullOrEmpty(String nullOrEmpty)
    throws UnsupportedEncodingException
  {
    QueryParams params = QueryParams.parseQueryString(nullOrEmpty, _UTF8);
    Assert.assertNotNull(params);
    Assert.assertTrue(params.getParameterMap().isEmpty());    
  }
  
  private void _parseAndCompare(String queryString, String... expectedNameValues)
    throws UnsupportedEncodingException
  {
    QueryParams params = QueryParams.parseQueryString(queryString, _UTF8);
    Map<String, String[]> expectedParamValuesMap = _toParameterValuesMap(expectedNameValues);
    _compareParamValuesMaps(params.getParameterValuesMap(), expectedParamValuesMap);
    _checkSingleValueParamMap(params.getParameterMap(), expectedParamValuesMap);
  }
  
  private static Map<String, String[]> _toParameterValuesMap(String... nameValues)
  {
    Map<String, String[]> map = new HashMap<String, String[]>();
    
    if ((nameValues.length % 2) != 0)
    {
      throw new IllegalArgumentException("nameValues must contain an even number of entries");
    }
    
    for (int i = 0; i < nameValues.length; i += 2)
    {
      String paramName = nameValues[i];
      String paramValue = nameValues[i + 1];
      QueryParams.__addParameterToMap(map, paramName, paramValue);
    }

    return map;
  }
  
  private static void _compareParamValuesMaps(
    Map<String, String[]> sourceMap,
    Map<String, String[]> targetMap
    )
  {
    Assert.assertNotNull(sourceMap);
    Assert.assertNotNull(targetMap);
    Assert.assertEquals(sourceMap.size(), targetMap.size());
    
    for (Map.Entry<String, String[]> entry : sourceMap.entrySet())
    {
      String[] sourceValues = entry.getValue();
      String[] targetValues = targetMap.get(entry.getKey());
      
      Assert.assertArrayEquals(sourceValues, targetValues);
    }
  }
  
  private static void _checkSingleValueParamMap(
    Map<String, String> singleValueMap,
    Map<String, String[]> multiValueMap
    )
  {
    Assert.assertTrue(singleValueMap.size() == multiValueMap.size());
    
    for (Map.Entry<String, String> param : singleValueMap.entrySet())
    {      
      String[] multiValue = multiValueMap.get(param.getKey());
      Assert.assertNotNull(multiValue);
      Assert.assertTrue(multiValue.length > 0);      
      Assert.assertEquals(param.getValue(), multiValue[0]);
    }
  }

  private static final String _UTF8 = "UTF-8";
}
