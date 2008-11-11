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

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;

import org.apache.myfaces.trinidad.skin.Icon;


/**
 * The StyleProvider API is used to access context-dependent style
 * information.  Style information is exposed in two ways - as
 * CSS style sheet URIs (via getStyleSheetURI()), or as Style objects
 * (via getStyleMap()).  Both methods take a StyleContext object,
 * which describes the target end user environment.
 *
 * In addition to providing access to style-related information, the
 * StyleProvider also provides access to Icons which are defined within
 * style sheets via the getIcons() API.
 *
 * @see StyleContext
 * @see StyleMap
 * @see Style
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/StyleProvider.java#0 $) $Date: 10-nov-2005.18:57:58 $
 */
public interface StyleProvider
{
  /**
   * Returns the mime type (for example, "text/css") of the styles
   * defined by the StyleProvider.
   */
  public String getContentStyleType(StyleContext context);

  /**
   * Returns a Map which maps style class names to
   * equivalent shorter names.
   * @param context The StyleContext
   *
   * @return A Map which maps the full style class names to
   *   the shorter equivalents.
   */
  public Map<String, String> getShortStyleClasses(StyleContext context);

  /**
   * Returns the URI of the CSS style sheet to use for the
   * end user environment specified via the StyleContext.
   * @param context The context which describes the end user
   *   environment for this request
   * @return A list of CSS style sheet URIs
   */
  public List<String> getStyleSheetURIs(StyleContext context);

  /**
   * Returns a StyleMap object, which can be used to
   * retreive Style objects for the specified context.
   *
   * @param context The context which describes the target end user
   *   environment
   * @return A StyleMap object which exposes the Styles for the
   *  specified context.
   */
  public StyleMap getStyleMap(StyleContext context);

  /**
   * Returns a map of icon names to Icon instances.
   *
   * The returned map is both mutable and threadsafe.  This allows
   * request/context-specific icons to be cached on the icon map.
   *
   * @param context The context which describes the target end user
   *   environment
   * @return A ConcurrentMap which exposes the Icons for the
   *  specified context.
   */
  public ConcurrentMap<String, Icon> getIcons(StyleContext context);

  /**
   * Returns a map of skin property keys to skin property Object instances.
   *
   * The returned map is both mutable and threadsafe.  This allows
   * request/context-specific skin properties to be cached on the skin property map.
   *
   * @param context The context which describes the target end user
   *   environment
   * @return A ConcurrentMap which exposes the skin properties for the
   *  specified context.
   */
  public ConcurrentMap<Object, Object> getSkinProperties(StyleContext context);
}
