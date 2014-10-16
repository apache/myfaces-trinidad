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

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinAddition;
import org.apache.myfaces.trinidad.skin.SkinMetadata;
import org.apache.myfaces.trinidad.skin.SkinProvider;
import org.apache.myfaces.trinidadinternal.skin.SkinExtension;
import org.apache.myfaces.trinidadinternal.skin.SkinUtils;
import org.apache.myfaces.trinidadinternal.skin.parse.SkinsNode;

/**
 * This is the Trinidad's SkinProvider for loading skins from trinidad-skins.xml. This provider
 * reads and caches trinidad-skins.xml from various sources like META-INF, WEB-INF and
 * SkinResourceLoader API during its initialization. Subsequently the Skin objects are created
 * lazily using the cached metadata, only when it is first requested for.
 */
public final class TrinidadSkinProvider extends BaseSkinProvider
{
  /**
   * Key for the TrinidadSkinProvider stored in ExternalContext
   */
  public static final String TRINDIAD_SKIN_PROVIDER_KEY =
    "org.apache.myfaces.trinidad.skin.TRINIDAD_SKIN_PROVIDER_INSTANCE";

  /**
   * static factory method to get hold of a TrinidadSkinProvider object This can be used for easy
   * creation of Skin object without having to implement the abstract class
   *
   * @param ec
   * @return
   */
  public static TrinidadSkinProvider getCurrentInstance(ExternalContext ec)
  {
    if (ec == null)
      throw new NullPointerException("ExternalContext is passed as null");

    TrinidadSkinProvider trinidadSkinProvider =
      (TrinidadSkinProvider) ec.getApplicationMap().get(TRINDIAD_SKIN_PROVIDER_KEY);
    return trinidadSkinProvider;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<SkinMetadata> getSkinMetadata(ExternalContext context)
  {
    initialize(context);
    return _skinMetadata;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Skin loadAvailableSkin(ExternalContext context, SkinMetadata skinMetadata)
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
      // This cannot happen because base class checks for availability before it calls for the
      // skin to be loaded
      String message = _LOG.getMessage("SP_LOADING_UNKNOWN_SKIN", skinMetadata.getId());
      _LOG.severe(message);
      throw new IllegalArgumentException(message);
    }

    String id = matchingNode.getId();
    String family = matchingNode.getFamily();
    String renderKitId = matchingNode.getRenderKitId();
    Skin baseSkin = null;
    String baseSkinId = matchingNode.getBaseSkinId();
    SkinProvider provider = SkinProvider.getCurrentInstance(context);

    if (provider != null && baseSkinId != null)
      baseSkin = provider.getSkin(context, new SkinMetadata.Builder().id(baseSkinId).build());

    // if there is no base skin then use the default base skin for the renderKit
    if (baseSkin == null)
    {
      baseSkin = SkinUtils.getDefaultSkinForRenderKitId(provider, context, renderKitId);
    }

    if (id == null)
      throw new NullPointerException(_LOG.getMessage("NULL_SKIN_ID"));

    if (family == null)
      throw new NullPointerException("Null family");

    _LOG.fine("Creating skin extension for skin metadata {0}", skinMetadata);
    return new SkinExtension(baseSkin, matchingNode, true);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void initialize(ExternalContext extCtxt)
  {
    if (extCtxt == null)
    {
      throw new NullPointerException("ExternalContext is passed as null");
    }

    if (_skinMetadata == null)
    {
      if (_LOG.isFine())
        _LOG.fine("init provider.");

      // only now do initialization
      List<SkinsNode> skinsNodes = SkinUtils.buildSkinsNodes(extCtxt);
      List<SkinMetadata> skinNodes = new ArrayList<SkinMetadata>();
      List<SkinAddition> skinAdditionNodes = new ArrayList<SkinAddition>();

      for (SkinsNode node : skinsNodes)
      {
        if (node != null)
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
        _LOG.fine("Number of skin metadata loaded from trinidad-skins.xml: {0}", _skinMetadata.size());
        _LOG.fine("Number of skin additions loaded from trinidad-skins.xml: {0}", _skinAdditionNodes.size());
      }
    }
  }

  /**
   * used to ensure that the skin and its base skin additions are added correctly
   * This is called at the exit point of SkinProviderRegistry
   * @param skin
   */
  void ensureSkinAdditions(Skin skin)
  {
    if (_skinAdditionNodes == null || _skinAdditionNodes.isEmpty())
      return;

    for (SkinAddition addition : _skinAdditionNodes)
    {
      // skin additions in _skinAdditionNodes will not be null
      _checkAndAddInHierarchy(skin, addition);
    }
  }

  private void _checkAndAddInHierarchy(Skin skin, SkinAddition addition)
  {
    // exit condition for the recursive call
    if (skin == null)
      return;

    String skinId = addition.getSkinId();

    if (skinId != null && skinId.equals(skin.getId()))
    {
      skin.addSkinAddition(addition);
    }

    _checkAndAddInHierarchy(skin.getBaseSkin(), addition);
  }

  private List<SkinMetadata> _skinMetadata;
  private List<SkinAddition> _skinAdditionNodes;

  private final static TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(TrinidadSkinProvider.class);
}

