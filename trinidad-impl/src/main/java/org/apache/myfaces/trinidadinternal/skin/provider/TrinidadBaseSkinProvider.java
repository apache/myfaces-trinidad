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
package org.apache.myfaces.trinidadinternal.skin.provider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinMetadata;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.CasablancaDesktopSkin;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.CasablancaPdaSkin;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.CasablancaPortletSkin;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.MinimalDesktopSkinExtension;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.MinimalPdaSkinExtension;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.MinimalPortletSkinExtension;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.SimpleDesktopSkin;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.SimplePdaSkin;
import org.apache.myfaces.trinidadinternal.renderkit.core.skin.SimplePortletSkin;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.CASABLANCA_DESKTOP_ID;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.CASABLANCA_PDA_ID;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.CASABLANCA_PORTLET_ID;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.CASABLANCA_SKIN_FAMILY;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.CASABLANCA_STYLE_SHEET_NAME;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.MINIMAL_DESKTOP_ID;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.MINIMAL_DESKTOP_STYLE_SHEET_NAME;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.MINIMAL_PDA_ID;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.MINIMAL_PDA_STYLE_SHEET_NAME;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.MINIMAL_PORTLET_ID;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.MINIMAL_PORTLET_STYLE_SHEET_NAME;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.MINIMAL_SKIN_FAMILY;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.SIMPLE_DESKTOP_ID;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.SIMPLE_DESKTOP_LOCATION;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.SIMPLE_PDA_ID;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.SIMPLE_PDA_LOCATION;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.SIMPLE_PORTLET_ID;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.SIMPLE_PORTLET_LOCATION;
import static org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants.SIMPLE_SKIN_FAMILY;

/**
 * This SkinProvider creates the very base skins such as simple, minimal and casablanca
 */
public class TrinidadBaseSkinProvider extends BaseSkinProvider
{
  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<SkinMetadata> getSkinMetadata(FacesContext context)
  {
    return Collections.unmodifiableCollection(_METADATA);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Skin loadAvailableSkin(FacesContext context, SkinMetadata search)
  {
    // avoid round trips to SkinProviderRegistry for trinidad base skins
    // any base skin used in this provider is also a static skin and can be loaded locally
    // there fore load the base skins locally

    Skin skin = getSkins().get(search);

    if (skin != null)
    {
      return skin;
    }

    Skin loadedSkin = null;
    Skin parentSkin = null;

    if (search == _CASABLANCA_DESKTOP_METADATA)
    {
      parentSkin = _loadBaseSkinAndRegisterIfRequired(context, _SIMPLE_DESKTOP_METADATA);
      loadedSkin =  new CasablancaDesktopSkin(parentSkin);
    }
    else if (search == _CASABLANCA_PDA_METADATA)
    {
      parentSkin = _loadBaseSkinAndRegisterIfRequired(context, _SIMPLE_PDA_METADATA);
      loadedSkin = new CasablancaPdaSkin(parentSkin);
    }
    else if (search == _CASABLANCA_PORTLET_METADATA)
    {
      parentSkin = _loadBaseSkinAndRegisterIfRequired(context, _SIMPLE_PORTLET_METADATA);
      loadedSkin = new CasablancaPortletSkin(parentSkin);
    }
    else if (search == _MINIMAL_DESKTOP_METADATA)
    {
      parentSkin = _loadBaseSkinAndRegisterIfRequired(context, _SIMPLE_DESKTOP_METADATA);
      loadedSkin =  new MinimalDesktopSkinExtension(parentSkin);
    }
    else if (search == _MINIMAL_PDA_METADATA)
    {
      parentSkin = _loadBaseSkinAndRegisterIfRequired(context, _SIMPLE_PDA_METADATA);
      loadedSkin = new MinimalPdaSkinExtension(parentSkin);
    }
    else if (search == _MINIMAL_PORTLET_METADATA)
    {
      parentSkin = _loadBaseSkinAndRegisterIfRequired(context, _SIMPLE_PORTLET_METADATA);
      loadedSkin = new MinimalPortletSkinExtension(parentSkin);
    }
    else if (search == _SIMPLE_DESKTOP_METADATA)
    {
      loadedSkin = new SimpleDesktopSkin();
    }
    else if (search == _SIMPLE_PDA_METADATA)
    {
      loadedSkin = new SimplePdaSkin();
    }
    else if (search == _SIMPLE_PORTLET_METADATA)
    {
      loadedSkin = new SimplePortletSkin();
    }

    return loadedSkin;
  }

  private Skin _loadBaseSkinAndRegisterIfRequired(FacesContext context, SkinMetadata metadata)
  {
    Skin skin = getSkins().get(metadata);

    if (skin == null)
    {
      skin = loadAvailableSkin(context, metadata);
      addSkin(metadata, skin);
    }

    return skin;
  }


  private final static Collection<SkinMetadata> _METADATA;
  private final static SkinMetadata _SIMPLE_DESKTOP_METADATA;
  private final static SkinMetadata _SIMPLE_PDA_METADATA;
  private final static SkinMetadata _SIMPLE_PORTLET_METADATA;
  private final static SkinMetadata _MINIMAL_DESKTOP_METADATA;
  private final static SkinMetadata _MINIMAL_PORTLET_METADATA;
  private final static SkinMetadata _MINIMAL_PDA_METADATA;
  private final static SkinMetadata _CASABLANCA_DESKTOP_METADATA;
  private final static SkinMetadata _CASABLANCA_PDA_METADATA;
  private final static SkinMetadata _CASABLANCA_PORTLET_METADATA;

  static {
    _SIMPLE_DESKTOP_METADATA = new SkinMetadata.Builder().id(SIMPLE_DESKTOP_ID).family(SIMPLE_SKIN_FAMILY)
      .renderKitId(SkinMetadata.RenderKitId.DESKTOP).styleSheetName(SIMPLE_DESKTOP_LOCATION).build();
    _SIMPLE_PDA_METADATA = new SkinMetadata.Builder().id(SIMPLE_PDA_ID).family(SIMPLE_SKIN_FAMILY)
      .renderKitId(SkinMetadata.RenderKitId.PDA).styleSheetName(SIMPLE_PDA_LOCATION).build();
    _SIMPLE_PORTLET_METADATA = new SkinMetadata.Builder().id(SIMPLE_PORTLET_ID).family(SIMPLE_SKIN_FAMILY)
      .renderKitId(SkinMetadata.RenderKitId.PORTLET).styleSheetName(SIMPLE_PORTLET_LOCATION).build();
    _MINIMAL_DESKTOP_METADATA= new SkinMetadata.Builder().id(MINIMAL_DESKTOP_ID).family(MINIMAL_SKIN_FAMILY)
      .renderKitId(SkinMetadata.RenderKitId.DESKTOP).baseSkinId(SIMPLE_DESKTOP_ID)
      .styleSheetName(MINIMAL_DESKTOP_STYLE_SHEET_NAME).build();
    _MINIMAL_PDA_METADATA = new SkinMetadata.Builder().id(MINIMAL_PDA_ID).family(MINIMAL_SKIN_FAMILY)
      .renderKitId(SkinMetadata.RenderKitId.PDA).baseSkinId(SIMPLE_PDA_ID)
      .styleSheetName(MINIMAL_PDA_STYLE_SHEET_NAME).build();
    _MINIMAL_PORTLET_METADATA = new SkinMetadata.Builder().id(MINIMAL_PORTLET_ID).family(MINIMAL_SKIN_FAMILY)
      .renderKitId(SkinMetadata.RenderKitId.PORTLET).baseSkinId(SIMPLE_PORTLET_ID)
      .styleSheetName(MINIMAL_PORTLET_STYLE_SHEET_NAME).build();
    _CASABLANCA_DESKTOP_METADATA = new SkinMetadata.Builder().id(CASABLANCA_DESKTOP_ID).family(CASABLANCA_SKIN_FAMILY)
      .renderKitId(SkinMetadata.RenderKitId.DESKTOP).baseSkinId(SIMPLE_DESKTOP_ID)
      .styleSheetName(CASABLANCA_STYLE_SHEET_NAME).build();
    _CASABLANCA_PDA_METADATA = new SkinMetadata.Builder().id(CASABLANCA_PDA_ID).family(CASABLANCA_SKIN_FAMILY)
      .renderKitId(SkinMetadata.RenderKitId.PDA).baseSkinId(SIMPLE_PDA_ID)
      .styleSheetName(CASABLANCA_STYLE_SHEET_NAME).build();
    _CASABLANCA_PORTLET_METADATA = new SkinMetadata.Builder().id(CASABLANCA_PORTLET_ID).family(CASABLANCA_SKIN_FAMILY)
      .renderKitId(SkinMetadata.RenderKitId.PORTLET).baseSkinId(SIMPLE_PORTLET_ID).build();

    _METADATA = new ArrayList<SkinMetadata>(9);
    _METADATA.add(_SIMPLE_DESKTOP_METADATA);
    _METADATA.add(_SIMPLE_PDA_METADATA);
    _METADATA.add(_SIMPLE_PORTLET_METADATA);
    _METADATA.add(_MINIMAL_DESKTOP_METADATA);
    _METADATA.add(_MINIMAL_PDA_METADATA);
    _METADATA.add(_MINIMAL_PORTLET_METADATA);
    _METADATA.add(_CASABLANCA_DESKTOP_METADATA);
    _METADATA.add(_CASABLANCA_PDA_METADATA);
    _METADATA.add(_CASABLANCA_PORTLET_METADATA);
  }
}