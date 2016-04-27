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
package org.apache.myfaces.trinidadinternal.skin.pregen.variant;

import org.apache.myfaces.trinidad.context.AccessibilityProfile;
import org.apache.myfaces.trinidad.context.LocaleContext;

import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig.ContainerType;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig.RequestType;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig.StyleClassType;

/**
 * Represents a single unique combination of variants/properties that
 * influence which styles are selected/included during skin generation.
 */
public final class SkinVariant
{
  /**
   * Creates a unique skin variant.
   */
  public SkinVariant(
    LocaleContext         localeContext,
    int                   platform,
    ApplicationAndVersion appAndVersion,
    AccessibilityProfile  accProfile,
    ContainerType         containerType,
    RequestType           requestType,
    StyleClassType        styleClassType
    )
  {
    _localeContext = localeContext;
    _platform = platform;
    _appAndVersion = appAndVersion;
    _accProfile = accProfile;
    _containerType = containerType;
    _requestType = requestType;
    _styleClassType = styleClassType;
  }
  
  public LocaleContext getLocaleContext()
  {
    return _localeContext;
  }
  
  public int getPlatform()
  {
    return _platform;
  }

  public ApplicationAndVersion getApplicationAndVersion()
  {
     return _appAndVersion;
  }
  
  public AccessibilityProfile getAccessibilityProfile()
  {
    return _accProfile;
  }
  
  public ContainerType getContainerType()
  {
    return _containerType;
  }
  
  public RequestType getRequestType()
  {
    return _requestType;
  }
  
  public StyleClassType getStyleClassType()
  {
    return _styleClassType;
  }

  private final LocaleContext         _localeContext;
  private final int                   _platform;
  private final ApplicationAndVersion _appAndVersion;
  private final AccessibilityProfile  _accProfile;
  private final ContainerType         _containerType;
  private final RequestType           _requestType;
  private final StyleClassType        _styleClassType;
}
