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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.util.FileUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig;
import org.apache.myfaces.trinidadinternal.skin.pregen.context.PregenStyleProvider;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;

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
    ) throws IOException
  {
    assert(skin != null);
    assert(config != null);

    SkinPregenerator pregenerator = _getSkinPregenerator();
    String stylesCacheDirectoryPath = _getStylesCacheDirectoryPath(config);
    StyleProvider provider = new PregenStyleProvider(skin, stylesCacheDirectoryPath);

    pregenerator.pregenerate(context, skin, config, provider);
  }
  
  private static SkinPregenerator _getSkinPregenerator()
  {
    if (Beans.isDesignTime())
    {
      return _NOOP_PREGENERATOR;
    }

    return new AllVariantsSkinPregenerator();
  }

  // Returns the subdirectory for skin-specific generated files.
  // Throws an IOException if the directory does not exist/cannot be
  // created or is not writable.
  private static String _getStylesCacheDirectoryPath(PregenConfig config)
    throws IOException
  {
    String targetDirPath = config.getTargetDirectoryPath();
    String subDirPath = TrinidadRenderingConstants.STYLES_CACHE_DIRECTORY;
    String stylesCacheDirPath = targetDirPath + subDirPath;
    
    // Verify that the directory exists and is writable.
    FileUtils.toWritableDirectory(stylesCacheDirPath);
    
    return stylesCacheDirPath;
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
        PregenConfig    config,
        StyleProvider   provider)
      {
      }
    };
}
