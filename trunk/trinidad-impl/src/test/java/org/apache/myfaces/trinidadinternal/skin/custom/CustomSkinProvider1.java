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
package org.apache.myfaces.trinidadinternal.skin.custom;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidad.skin.SkinMetadata;
import org.apache.myfaces.trinidad.skin.SkinProvider;
import org.apache.myfaces.trinidad.skin.SkinVersion;

import javax.faces.context.ExternalContext;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class CustomSkinProvider1 extends SkinProvider
{
  @Override
  public Skin getSkin(ExternalContext context, SkinMetadata skinMetadata)
  {
    String skinId = skinMetadata.getId();
    String family = skinMetadata.getFamily();
    if ("purple.desktop".equals(skinId) || "purple".equals(family))
    {
      if (_purpleSkin == null)
        _purpleSkin = SkinFactory.getFactory().createSkin(context, PURPLE);
      return _purpleSkin;
    }
    if ("blue.desktop".equals(skinId)  || "blue".equals(family))
    {
      if (_blueSkin == null)
        _blueSkin = SkinFactory.getFactory().createSkin(context, BLUE);
      return _blueSkin;
    }
    if ("cyan.desktop".equals(skinId)  || "cyan".equals(family))
    {
      if (_cyanSkin == null)
        _cyanSkin = SkinFactory.getFactory().createSkin(context, NO_CACHE_CYAN);
      return _cyanSkin;
    }
    return null;
  }

  @Override
  public Collection<SkinMetadata> getSkinMetadata(ExternalContext context)
  {
    return Collections.unmodifiableCollection(_METADATA);
  }

  public static final String PURPLE_SKIN_ID        = "purple.desktop";
  public static final String NO_CACHE_CYAN_SKIN_ID = "cyan.desktop";
  public static final String BLUE_SKIN_ID          = "blue.desktop";

  private Skin _purpleSkin;
  private Skin _blueSkin;
  private Skin _cyanSkin;
  private final static SkinMetadata      PURPLE        = new SkinMetadata.Builder()
      .id(PURPLE_SKIN_ID)
      .family("purple")
      .version(new SkinVersion("v1"))
      .baseSkinId("minimal.desktop")
      .styleSheetName("org/apache/myfaces/trinidadinternal/skin/purple/purpleSkin.css")
      .build();
  private final static SkinMetadata      BLUE          = new SkinMetadata.Builder()
      .id(BLUE_SKIN_ID)
      .family("blue")
      .version(new SkinVersion("v1"))
      .baseSkinId(PURPLE_SKIN_ID)
      .styleSheetName("org/apache/myfaces/trinidadinternal/skin/purple/purpleBigFontSkin.css")
      .build();
  private final static SkinMetadata      NO_CACHE_CYAN = new SkinMetadata.Builder()
      .id(NO_CACHE_CYAN_SKIN_ID)
      .family("cyan")
      .version(new SkinVersion("v1"))
      .baseSkinId("beach.desktop")
      .styleSheetName("org/apache/myfaces/trinidadinternal/skin/purple/purpleBigFontSkinThree.css")
      .build();
  private final static Set<SkinMetadata> _METADATA     = new HashSet<SkinMetadata>();

  static
  {
    _METADATA.add(PURPLE);
    _METADATA.add(BLUE);
    _METADATA.add(NO_CACHE_CYAN);
  }

}
