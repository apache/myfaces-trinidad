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
package org.apache.myfaces.trinidadinternal.skin.pregen;

import java.beans.Beans;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig;

/**
 * Skin pregeneration utilities.
 */
public final class SkinPregenerationUtils
{
  /**
   * Pregenerates style sheets for the specified skin.
   *
   * @param context the FacesContext for the current request
   * @param skin the non-null Skin to pregenerate
   * @param config configuration info that determines how pregeneration
   *   is performed.
   */
  public static void pregenerate(
    FacesContext    context,
    Skin            skin,
    PregenConfig    config
    )
  {
    assert(skin != null);
    assert(config != null);

    SkinPregenerator pregenerator = _getSkinPregenerator(context);
    pregenerator.pregenerate(context, skin, config);
  }
  
  private static SkinPregenerator _getSkinPregenerator(FacesContext context)
  {
    if (Beans.isDesignTime())
    {
      return _NOOP_PREGENERATOR;
    }

    return new AllVariantsSkinPregenerator(context);
  }

  private SkinPregenerationUtils()
  {
  }
  
  private static final SkinPregenerator _NOOP_PREGENERATOR =
    new SkinPregenerator()
    {
      @Override
      public void pregenerate(
        FacesContext    context,
        Skin            skin, 
        PregenConfig    config)
      {
      }
    };
}
