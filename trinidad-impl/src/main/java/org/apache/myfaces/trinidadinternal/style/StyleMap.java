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
package org.apache.myfaces.trinidadinternal.style;

/**
 * The StyleMap is a type-safe map interface for obtaining 
 * Style objects based on a style selector.  Context-specific
 * Style maps can be obtained via the StyleProvider interface.
 * 
 * @see Style
 * @see StyleProvider
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/StyleMap.java#0 $) $Date: 10-nov-2005.18:57:58 $
 */
public interface StyleMap
{
  /**
   * Returns the Style object associated with the specified selector
   *
   * @param context The StyleContext to use when looking up the name.
   *   This should be the same StyleContext instance that is used to 
   *   obtain the StyleMap from the StyleProvider.
   * @param selector The CSS selector string for the desired Style
   */
  public Style getStyleBySelector(StyleContext context, String selector);

  /**
   * Returns the Style object associated with the specified style class
   *
   * @param context The StyleContext to use when looking up the name.
   *   This should be the same StyleContext instance that is used to 
   *   obtain the StyleMap from the StyleProvider.
   * @param styleClass the CSS style class for the desired style
   */
  public Style getStyleByClass(StyleContext context, String styleClass);

  /**
   * Returns the Style object associated with the specified style name
   *
   * @param context The StyleContext to use when looking up the name.
   *   This should be the same StyleContext instance that is used to 
   *   obtain the StyleMap from the StyleProvider.
   * @param name the name for the desired style
   */
  public Style getStyleByName(StyleContext context, String name);
}
