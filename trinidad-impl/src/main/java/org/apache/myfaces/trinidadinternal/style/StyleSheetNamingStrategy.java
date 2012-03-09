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
package org.apache.myfaces.trinidadinternal.style;

import java.util.Map;

import org.apache.myfaces.trinidad.util.Enums;

/**
 * Enum used by StyleSheetContext.getStyleSheetNaming() to indicate what
 * type of names should be used for generated style file names.
 */
public enum StyleSheetNamingStrategy
{
  /**
   * Indicates that short names should be used for generated style sheet files.
   */
  SHORT("short"),
  
  /**
   * Indicates that stable (possibly longer) names should be used for generated
   * style sheet files.
   */
  STABLE("stable");
  
  StyleSheetNamingStrategy(String displayName)
  {
    _displayName = displayName;
  }

  /**
   * Returns the display name for this enum constant.
   */
  public String displayName()
  {
    return _displayName;
  }

  /**
   * Performs a reverse lookup of an StyleSheetNames constant based on
   * its display name.
   * 
   * @param displayName the display name of the Accessibility constant to return.
   * @return the non-null Accessibility constant associated with the display name.
   * @throws IllegalArgumentException if displayName does not correspond
   *   to some Accessibility constant.
   * @throws NullPointerException if displayName is null. 
   */
  public static StyleSheetNamingStrategy valueOfDisplayName(String displayName)
  {
    return Enums.stringToEnum(_displayNameMap, displayName, StyleSheetNamingStrategy.class);
  }

  private final String _displayName;
  private static final Map<String, StyleSheetNamingStrategy> _displayNameMap;
    
  static
  {   
    _displayNameMap = Enums.createDisplayNameMap(StyleSheetNamingStrategy.class);
  }
}
