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


/**
 * Helper class to build strings for diagnostic log messages which benefit from detailed information,
 *  and in toString() implementations.
 *
 * This class takes care of few niceties like:
 *  1. Adding the identity (classname and hashcode) of an object
 *  2. Adding fields and values of an object, ensuring null value fields are excluded
 *  3. Formatting the string in JSON style
 *  4. Adopts a builder pattern, hence easy to use
 *
 * Note that this class is intended to be used in a single thread and is not thread safe.
 *
 * Example use:
 *
 * <code>
 * public class CustomClass
 * {
 *   private String fieldOne = "HELLO";
 *   private Object fieldTwo = null;
 *   private boolean fieldThree = false;
 *   private List<String> fieldFour = new ArrayList<String>(Arrays.asList("one","two","three"));
 *
 *   public String toString()
 *   {
 *     return
 *       new ToStringHelper(this).
 *       append("fieldOne", fieldOne).
 *       append("fieldTwo", fieldTwo).
 *       append("fieldThree", fieldThree).
 *       append("fieldFour", fieldFour).
 *       toString();
 *   }
 * }
 * </code>
 *
 * Return value of toString() in above example will be
 *  "CustomClass@19621457 {fieldOne: HELLO, fieldThree: false, fieldFour: [one, two, three]}
"
 */
public final class ToStringHelper
{
  public ToStringHelper()
  {
    this(null);
  }

  /**
   * Constructor that takes in the object whose information is to be added to the string
   * @param object The object whose classname and hashcode is to be included in the string
   */
  public ToStringHelper(Object object)
  {
    _builder = new StringBuilder();
    
    if (object != null)
    {
      _builder.append(object.getClass().getName());
      _builder.append("@");
      _builder.append(System.identityHashCode(object));
    }
  }

  /**
   * Appends the field name and value (as returned by 'toString()' of the supplied value object), 
   *  formatting it in JSON style
   * @param fieldName The name of the field to append
   * @param value The value object of the field to append
   * @return The helper object after appending
   */
  public ToStringHelper append(String fieldName, Object value)
  {
    if (fieldName != null)
    {
      String stringValue = null;
      
      if (value != null)
      {
        stringValue = value.toString();
      }
      
      _append(fieldName, stringValue);
    }
    
    return this;
  }
  
  /**
   * Appends the field name and value string, formatting it in JSON style
   * @param fieldName The name of the field to append
   * @param value The value string of the field to append
   * @return The helper object after appending
   */
  private ToStringHelper _append(String fieldName, String value)
  {
    if (value == null)
    {
      if (_STYLE.isNullFieldAppended())
      {
        value = _STYLE.getNullText();
      }
      else
      {
        return this;
      }
    }
    
    _appendNonNullNameValuePair(fieldName, value);
    return this;
  }

  private void _appendNonNullNameValuePair(String fieldName, String value)
  {
    if (fieldName != null && value != null)
    {
      StringBuilder pair = new StringBuilder();
      
      int contentEndIndex = _builder.lastIndexOf(_STYLE.getContentEnd());

      // if we have not added the field-value end marker, add it once
      if (contentEndIndex == -1)
      {
        _builder.append(_SPACE);
        _builder.append(_STYLE.getContentStart());
        _builder.append(_STYLE.getContentEnd());
        contentEndIndex = _builder.length() - 1;
      }
      else
      {
        // we have the end markers, so we have fields already, so just add field separator
        pair.append(_STYLE.getFieldSeparator());
        pair.append(_SPACE);
      }
      
      // add the field-value pair
      pair.append(fieldName);
      pair.append(_STYLE.getFieldNameValueSeparator());
      pair.append(_SPACE);
      pair.append(value);
      
      // insert it just before the end marker
      _builder.insert(contentEndIndex, pair); // BREAKPOINT
    }
  }

  @Override
  public String toString()
  {
    return _builder.toString();
  }
  
  /**
   * Interface defining the formatting style that the ToStringHelper can use
   */
  private interface StringStyle
  {
    // character to append before appending field name-value pairs
    public String getContentStart();

    // character to append after appending field name-value pairs
    public String getContentEnd();
    
    // character to use as the separator between two field name-value pairs
    public String getFieldSeparator();
    
    // character to use as separator between the name and value of the field
    public String getFieldNameValueSeparator();
    
    // tells if we should append null fields
    public boolean isNullFieldAppended();
    
    // tells text to use if the field value is null (i.e. if isNullFieldAppended() returns true)
    public String getNullText();
  }
  
  /**
   * JSON style implementation for default use in ToStringHelper
   */
  private static class JSONStringStyle implements StringStyle
  {

    public String getContentStart()
    {
      return _CONTENT_START;
    }

    public String getContentEnd()
    {
      return _CONTENT_END;
    }

    public String getFieldSeparator()
    {
      return _FIELD_SEPARATOR;
    }

    public String getFieldNameValueSeparator()
    {
      return _FIELD_NAME_VALUE_SEPARATOR;
    }

    public boolean isNullFieldAppended()
    {
      return false;
    }

    public String getNullText()
    {
      return _NULL_TEXT;
    }
    
    private static String _CONTENT_START = "{";
    private static String _CONTENT_END = "}";
    private static String _FIELD_SEPARATOR = ",";
    private static String _FIELD_NAME_VALUE_SEPARATOR = ":";
    private static String _NULL_TEXT = "'null'";
  }
  
  private final StringBuilder _builder;
  private static final StringStyle _STYLE = new JSONStringStyle();
  private static final String _SPACE = " ";
}
