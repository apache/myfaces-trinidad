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

public class CustomSkinProvider2 extends SkinProvider
{
  @Override
  public Skin getSkin(ExternalContext context, SkinMetadata skinMetadata)
  {
    String skinId = skinMetadata.getId();
    String family = skinMetadata.getFamily();

    if ("violet.desktop".equals(skinId) || "violet".equals(family))
    {
      if (_violetSkin == null)
        _violetSkin = SkinFactory.getFactory().createSkin(context, VIOLET);
      return _violetSkin;
    }
    if ("green.desktop".equals(skinId) || "green".equals(family))
    {
      if (_greenSkin == null)
        _greenSkin = SkinFactory.getFactory().createSkin(context, GREEN);
      return _greenSkin;
    }
    return null;
  }

  @Override
  public Collection<SkinMetadata> getSkinMetadata(ExternalContext context)
  {
    return Collections.unmodifiableCollection(_METADATA);
  }

  public static final String VIOLET_SKIN_ID = "violet.desktop";
  public static final String GREEN_SKIN_ID = "green.desktop";

  private Skin _violetSkin;
  private Skin _greenSkin;
  private final static SkinMetadata      VIOLET      = new SkinMetadata.Builder()
      .id(VIOLET_SKIN_ID)
      .family("violet")
      .version(new SkinVersion("v1"))
      .baseSkinId(CustomSkinProvider1.PURPLE_SKIN_ID)
      .styleSheetName("org/apache/myfaces/trinidadinternal/skin/purple/purpleBigFontSkinTwo.css")
      .build();
  private final static SkinMetadata      GREEN       = new SkinMetadata.Builder()
      .id(GREEN_SKIN_ID)
      .family("green")
      .version(new SkinVersion("v1"))
      .baseSkinId("simple.desktop")
      .styleSheetName("org/apache/myfaces/trinidadinternal/skin/purple/purpleBigFontSkinFour.css")
      .build();
  private final static Set<SkinMetadata> _METADATA   = new HashSet<SkinMetadata>();

  static
  {
    _METADATA.add(VIOLET);
    _METADATA.add(GREEN);
  }

}
