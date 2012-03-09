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
 * Exception thrown by Enums multi-value parsing utilities to indicate that
 * a parsing failure has occurred.
 * 
 * This exception class offers one benefit over the standard exceptions
 * (eg. IllegalArgumentException): it provides access to the String value
 * that triggered the failure, which makes it possible to provide a more
 * detailed error message to the end user.
 * 
 * @see Enums#parseEnumValues
 */
public class EnumParseException extends RuntimeException
{
  /**
   * Creates the EnumParseException
   * 
   * @param message A message describing the failure
   * @param illegalValue the String value that could not be
   *   parsed to an Enum constant value.
   */
  public EnumParseException(String message, String illegalValue)
  {
    super(message);
    
    _illegalValue = illegalValue;
  }

  /**
   * Returns the String value that triggered the EnumParseException.
   */
  public String getIllegalValue()
  {
    return _illegalValue;    
  }

  private final String _illegalValue;
  
  private static final long serialVersionUID = 1L;
}
