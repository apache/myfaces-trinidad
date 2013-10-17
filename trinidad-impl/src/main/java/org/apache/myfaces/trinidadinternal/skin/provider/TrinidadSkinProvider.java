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
import java.util.List;
import java.util.Map;

import javax.el.ValueExpression;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinAddition;
import org.apache.myfaces.trinidad.skin.SkinMetadata;
import org.apache.myfaces.trinidad.skin.SkinProvider;
import org.apache.myfaces.trinidad.skin.SkinVersion;
import org.apache.myfaces.trinidadinternal.config.LazyValueExpression;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants;
import org.apache.myfaces.trinidadinternal.skin.SkinExtension;
import org.apache.myfaces.trinidadinternal.skin.SkinUtils;
import org.apache.myfaces.trinidadinternal.skin.parse.SkinsNode;

/**
 * This is the Trinidad's SkinProvider for loading skins from trinidad-skins.xml. This provider reads and caches
 * trinidad-skins.xml from various sources like META-INF, WEB-INF and SkinResourceLoader API during its initialization.
 * Subsequently the Skin objects are created lazily using the cached metadata, only when it is first requested for.
 */
public final class TrinidadSkinProvider extends BaseSkinProvider
{
  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<SkinMetadata> getSkinMetadata(FacesContext context)
  {
    // already initialized to unmodifiableCollection
    return _skinMetadata;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Skin loadAvailableSkin(FacesContext context, SkinMetadata skinMetadata)
  {
    SkinMetadata matchingNode = null;

    for (SkinMetadata node : _skinMetadata)
    {
      if (skinMetadata.getId().equals(node.getId()))
      {
        matchingNode = node;
        break;
      }
    }

    if (matchingNode == null)
    {
      // This cannot happen because base class checks for availability before it calls for the skin to be loaded
      if (_LOG.isSevere())
        _LOG.severe("SP_LOADING_UNKNOWN_SKIN", new Object[] {skinMetadata.getId()});

      throw new NullPointerException(_LOG.getMessage("SP_LOADING_UNKNOWN_SKIN", new Object[] {skinMetadata.getId()}));
    }

    String id =  matchingNode.getId();
    String family =  matchingNode.getFamily();
    String renderKitId =  matchingNode.getRenderKitId();
    Skin baseSkin = null;
    String baseSkinId = matchingNode.getBaseSkinId();
    SkinProvider provider = SkinUtils.getSkinProvider(context);

    if (provider != null && baseSkinId != null)
      baseSkin = provider.getSkin(context, new SkinMetadata.Builder().id(baseSkinId).build());

    // if there is no base skin then use the default base skin for the renderKit
    if (baseSkin == null)
      baseSkin = _getDefaultBaseSkin(provider, renderKitId);

    if (id == null)
      throw new NullPointerException(_LOG.getMessage("NULL_SKIN_ID"));

    if (family == null)
      throw new NullPointerException("Null family");

    if (_LOG.isFine())
      _LOG.fine("Creating skin extension for : " + skinMetadata);

    // features object itself cannot be null
    Skin loadedSkin = new SkinExtension(baseSkin, matchingNode);

    if (_skinAdditionNodes != null)
      for (SkinAddition addition : _skinAdditionNodes)
      {
        String additionSkinId = addition.getSkinId();

        if (id.equals(additionSkinId))
        {
          if (_LOG.isFine())
            _LOG.fine("Adding skin addition : " + addition);

          loadedSkin.addSkinAddition(addition);
        }

        if (baseSkinId.equals(additionSkinId)
          && !_hasSkinAddition(baseSkin.getSkinAdditions(), addition))
        {
            if (_LOG.isFine())
              _LOG.fine("Adding parent skin addition : " + addition);

            baseSkin.addSkinAddition(addition);
        }
      }

    return loadedSkin;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void initialize(FacesContext context)
  {
    if (context == null || context.getExternalContext() == null)
    {
      return;
    }

    if (_skinMetadata == null)
    {
      if (_LOG.isFine())
        _LOG.fine("init provider.");

      // only now do initialization
      ExternalContext extCtxt = context.getExternalContext();
      List<SkinsNode> skinsNodes = SkinUtils.buildSkinsNodes(extCtxt);
      List<SkinMetadata> skinNodes = new ArrayList<SkinMetadata>();
      List<SkinAddition> skinAdditionNodes = new ArrayList<SkinAddition>();

      for (SkinsNode node : skinsNodes)
      {
        if  (node != null)
        {
          skinNodes.addAll(node.getSkinNodes());
          skinAdditionNodes.addAll(node.getSkinAdditionNodes());
        }
      }

      if (skinNodes.isEmpty())
        _skinMetadata = Collections.emptyList();
      else
        _skinMetadata = Collections.unmodifiableList(skinNodes);

      if (skinAdditionNodes.isEmpty())
        _skinAdditionNodes = Collections.emptyList();
      else
        _skinAdditionNodes = Collections.unmodifiableList(skinAdditionNodes);

      if (_LOG.isFine())
      {
        _LOG.fine("trindiad-skins loaded: " + _skinMetadata.size());
        _LOG.fine("trindiad-skins additions loaded: " + _skinAdditionNodes.size());
      }
    }
  }

  private boolean _hasSkinAddition(List<SkinAddition> additions, SkinAddition search)
  {
    if (search == null)
      return false;

    // here we check only stylesheet name as an addition with a stylesheet name needs to added only once
    for (SkinAddition addn : additions)
      if (addn != null && search.getStyleSheetName().equals(addn.getStyleSheetName()))
        return true;

    return false;
  }

  private static Skin _getDefaultBaseSkin(SkinProvider provider, String renderKitId)
  {
    String baseSkinId;

    if (TrinidadRenderingConstants.APACHE_TRINIDAD_PDA.equals(renderKitId))
      baseSkinId =  TrinidadRenderingConstants.SIMPLE_PDA_ID;
    else if (TrinidadRenderingConstants.APACHE_TRINIDAD_PORTLET.equals(renderKitId))
      baseSkinId =  TrinidadRenderingConstants.SIMPLE_PORTLET_ID;
    else
      baseSkinId = TrinidadRenderingConstants.SIMPLE_DESKTOP_ID;

    Skin baseSkin = provider.getSkin(null, new SkinMetadata.Builder().id(baseSkinId).build());

    // this will never be null because we are asking for the default simple skin
    assert (baseSkin != null);
    return baseSkin;
  }

  private List<SkinMetadata> _skinMetadata;
  private List<SkinAddition> _skinAdditionNodes;

  private final static TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TrinidadSkinProvider.class);
}