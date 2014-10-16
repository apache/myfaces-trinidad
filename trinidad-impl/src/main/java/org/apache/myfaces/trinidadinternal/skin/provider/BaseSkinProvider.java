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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinMetadata;
import org.apache.myfaces.trinidad.skin.SkinProvider;
import org.apache.myfaces.trinidad.skin.SkinVersion;

/**
 * This is the common base class for Trinidad SkinProviders. This class abstracts out some common
 * code that is useful across Trinidad internal SkinProviders. One such example worth mentioning is
 * the code to finding a Skin match for a given SkinMetadata search criteria.
 *
 * @See TrinidadBaseSkinProvider
 * @See TrinidadSkinProvider
 * @See ExternalSkinProvider
 */
abstract class BaseSkinProvider extends SkinProvider
{
  public BaseSkinProvider()
  {
    initSkins();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Skin getSkin(ExternalContext context, SkinMetadata skinMetadata)
  {
    synchronized (this)
    {
      return _getMatchingSkin(context, skinMetadata);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Collection<SkinMetadata> getSkinMetadata(ExternalContext context)
  {
    synchronized (this)
    {
      return Collections.unmodifiableCollection(_skins.keySet());
    }
  }

  /**
   * add method used to register a skin with the provider.
   *
   * @param metadata
   * @param skin
   * @return Any previously registered skins existing with the same metadata
   */
  public Skin addSkin(SkinMetadata metadata, Skin skin)
  {
    if (metadata == null || skin == null)
    {
      _LOG.warning("CANNOT_ADD_SKIN");
      return null;
    }

    if (_LOG.isFine())
      _LOG.fine("Adding skin to cache [metadata: {0}, skin: {1}]", new Object[] {metadata, skin});

    synchronized (this)
    {
      return _skins.put(metadata, skin);
    }
  }

  /**
   * template method to be implemented by subclasses with logic to load a skin that base class knows
   * as available with the current skin provider being asked to load the skin
   *
   * @param context
   * @param skinMetadata
   */
  protected abstract Skin loadAvailableSkin(ExternalContext context, SkinMetadata skinMetadata);

  /**
   * getter for the skins, only used by ExternalSkinProvider.reload to cache and restore the skins
   * if reload fails
   *
   * @return
   */
  protected Map<SkinMetadata, Skin> getSkins()
  {
    synchronized (this)
    {
      return _skins;
    }
  }

  /**
   * setter for the skins, only used by ExternalSkinProvider.reload to cache and restore the skins
   * if reload fails
   *
   * @param skins
   */
  protected void setSkins(Map<SkinMetadata, Skin> skins)
  {
    synchronized (this)
    {
      // do not allow a null to be set.
      if (skins == null)
        skins = new HashMap<SkinMetadata, Skin>();

      _skins = skins;
    }
  }

  /**
   * initialize a new HashMap for the skins, used by constructor and ExternalSkinProvider.reload
   */
  protected void initSkins()
  {
    synchronized (this)
    {
      _skins = new HashMap<SkinMetadata, Skin>();
    }
  }

  /**
   * Hook to do any kind of initialization before loading a skin or skin metadata sub classes can
   * choose to implement
   *
   * @param context
   */
  protected void initialize(ExternalContext context)
  {
  }

  /**
   * one point method for searching skins
   *
   * @param context
   * @param searchMetadata to search for
   * @return matching skin
   */
  private final Skin _getMatchingSkin(ExternalContext context, SkinMetadata searchMetadata)
  {
    if (searchMetadata == null)
      throw new NullPointerException("SkinMetadata passed for search is null");

    // find a skin that is already loaded using the exact search metadata passed.
    Skin availableSkin = _skins.get(searchMetadata);
    if (availableSkin != null)
      return availableSkin;

    // there is no skin already available, so find a skin which matches the search criteria
    // verify that either id or family is set for the search
    if (null == searchMetadata.getId() && null == searchMetadata.getFamily())
    {
      if (_LOG.isWarning())
        _LOG.warning("SP_CANNOT_FIND_SKIN_WITHOUT_FAMILY_OR_ID");

      throw new NullPointerException(_LOG.getMessage("SP_CANNOT_FIND_SKIN_WITHOUT_FAMILY_OR_ID"));
    }

    initialize(context);

    if (_LOG.isFine())
      _LOG.fine("SP_FINDING_SKIN_FOR", new Object[]{searchMetadata.toString()});

    // first check if a skin matching the requirement is present in this provider

    SkinMetadata availableMetadata = _findSkinMetadata(context, searchMetadata);
    if (availableMetadata == null)
    {
      if (_LOG.isFine())
        _LOG.fine(this + " Cannot find skin in this provider: " + searchMetadata);

      return null;
    }

    // find a skin that is already loaded
    // this is different from doing it at the beginning of the method
    // because the metadata with which the skin is added into _skins
    // may not have matched completely, since searchMetadata can provide
    // only certain conditions.
    availableSkin = _skins.get(availableMetadata);
    if (availableSkin != null)
      return availableSkin;

    // now we know that there is a skin available but not loaded
    // so load the skin
    availableSkin = loadAvailableSkin(context, availableMetadata);
    assert (availableSkin != null);

    _skins.put(availableMetadata, availableSkin);

    return availableSkin;
  }

  /**
   * find if there is a skin with the search condition supported by the current SkinProvider
   *
   * @param search
   * @return
   */
  private SkinMetadata _findSkinMetadata(ExternalContext context, SkinMetadata search)
  {
    SkinMetadata matchingSkinMetadata = null;
    String skinId = search.getId();

    Collection<SkinMetadata> skinMetadata = getSkinMetadata(context);

    if (skinMetadata == null || skinMetadata.isEmpty())
      return null;

    // search is with either id or (family, renderkit, version)
    // we have ensure that either id or family is passed in the search
    if (skinId != null)
    {
      // Id is available then search using ID
      for (SkinMetadata m : skinMetadata)
        if (skinId.equals(m.getId()))
        {
          matchingSkinMetadata = m;
        }

      if (matchingSkinMetadata == null)
      {
        if (_LOG.isFine())
        {
          _LOG.fine("SP_CANNOT_FIND_MATCHING_SKIN_ID", new Object[]{skinId});
        }
      }
    }
    else
    {
      // search using family, renderkit, version
      String family = search.getFamily();

      // we need at least the family to go on with the search
      if (family == null)
        return null;

      // renderkit id cannot be null, we initialize it to APACHE_TRINIDAD_DESKTOP
      String renderKit = search.getRenderKitId();

      // version cannot be null as we initialize it to SkinVersion.EMPTY_SKIN_VERSION
      SkinVersion version = search.getVersion();

      List<SkinMetadata> familyRenderKitMatches = new ArrayList<SkinMetadata>(2);

      // search using family and renderkit id
      for (SkinMetadata m : skinMetadata)
        if (family.equalsIgnoreCase(m.getFamily()) &&
              renderKit.equalsIgnoreCase(m.getRenderKitId()))
          familyRenderKitMatches.add(m);

      if (familyRenderKitMatches.isEmpty())
      {
        // if we get here, that means we couldn't find an exact
        // family/renderKitId match, so return the simple skin
        // that matches the renderKitId.
        if (_LOG.isFine())
        {
          _LOG.fine("SP_CANNOT_FIND_MATCHING_SKIN", new Object[]{family, renderKit});
        }

        return null;
      }

      // at this point we know we have something in the familyRenderKitMatches
      // which is a list of matching family and renderKitId skins. Now match the version
      // to find the best matched skin.
      String versionName = version.getName();
      boolean versionIsDefault = version.isDefault();
      boolean foundMatchingSkin = false;

      // if the user didn't ask for the 'default' version, then look for the exact match
      if (!versionIsDefault)
      {
        matchingSkinMetadata = _findSkinMetadataForVersionName(familyRenderKitMatches, version);
      }

      // matchingSkinMetadata will be null if an exact version match (family+renderKitId+version)
      // was not found;
      // or if user was asking for a default version
      // we can have an exact version match if the user asks for null version,
      // and we find a skin with no
      // version set.
      if (matchingSkinMetadata == null || versionIsDefault)
      {
        // at this point either user is looking for default skin
        // or did not find the exact version match specified
        // so we find the default skin
        matchingSkinMetadata = _findSkinMetadataWithDefaultVersion(familyRenderKitMatches);

        if (matchingSkinMetadata == null)
        {
          // if we fail to find a default skin then
          // get the latest skin in the matchingSkinList if there is no skin marked default.
          if (_LOG.isFine())
            _LOG.fine(this + " did not find default / version skinMetadata. so getting leaf skin");

          matchingSkinMetadata = _findLeafSkinMetadata(familyRenderKitMatches);
        }
        else if ((matchingSkinMetadata != null) && versionIsDefault)
        {
          // found the default skin the user wanted
          if (_LOG.isFine())
            _LOG.fine(this + " found default skinMetadata");

          foundMatchingSkin = true;
        }
      }

      // log messages
      if (foundMatchingSkin)
      {
        if (_LOG.isFine())
          _LOG.fine("GET_SKIN_FOUND_SKIN_VERSION",
                    new Object[]{family, version, matchingSkinMetadata.getId()});
      }
      else if (matchingSkinMetadata != null)
      {
        if (_LOG.isFine())
        {
          if ("".equals(versionName))
          {
            _LOG.fine("GET_SKIN_CANNOT_FIND_NO_VERSION",
                      new Object[]{family, matchingSkinMetadata.getId()});
          }
          else
          {
            _LOG.fine("GET_SKIN_CANNOT_FIND_SKIN_VERSION",
                      new Object[]{family, version, matchingSkinMetadata.getId()});
          }
        }
      }

      if (matchingSkinMetadata == null)
      {
        matchingSkinMetadata = familyRenderKitMatches.get(familyRenderKitMatches.size() - 1);
      }
    }


    if (matchingSkinMetadata != null)
      if (_LOG.isFine())
        _LOG.fine(this + " found matching metadata: " + matchingSkinMetadata);

    return matchingSkinMetadata;
  }

  /**
   * find a skin with version passed
   *
   * @param skins
   * @param version
   * @return
   */
  private SkinMetadata _findSkinMetadataForVersionName(Collection<SkinMetadata> skins,
                                                       SkinVersion version)
  {
    if (version == null)
      throw new IllegalArgumentException("skin version cannot be null");

    if (version.getName() == null || version.getName().isEmpty())
      return null;

    for (SkinMetadata metadata : skins)
    {
      // metadata cannot be null and also version inside it cannot be null
      if (version.getName().equals(metadata.getVersion().getName()))
      {
        if (_LOG.isFine())
          _LOG.fine("Found version match skinMetadata: " + metadata);

        return metadata;
      }
    }

    return null;
  }

  /**
   * Latest skin is the one which is last in the family hierarchy eg: fusion-v1 -> fusion-v2 ->
   * fusion-v3 Among this fusion-v3 is the latest. So we look for a skin that is not extended by any
   * other skin in the family.
   *
   * @param skins
   * @return
   */
  private SkinMetadata _findLeafSkinMetadata(Collection<SkinMetadata> skins)
  {
    List<SkinMetadata> leafSkinMetadata = new ArrayList<SkinMetadata>();
    List<String> parentIds = new ArrayList<String>();

    // collect parents skins among the list
    for (SkinMetadata metadata : skins)
    {
      String parentId = metadata.getBaseSkinId();
      if (parentId != null)
        parentIds.add(metadata.getBaseSkinId());
    }

    // find leaf skins, which is not in parent list
    for (SkinMetadata metadata : skins)
    {
      String skinId = metadata.getId();
      if (skinId != null && !parentIds.contains(skinId))
      {
        leafSkinMetadata.add(metadata);
      }
    }

    // if there are no leaves, return null
    // this is a rare case, almost impossible since there will be
    // at least one skin which is not parent of another
    // but let us cover the corner case if any
    if (leafSkinMetadata.isEmpty())
      return null;

    // if there is one leaf skin return that
    // if there are many, return the last one among the leaves
    return leafSkinMetadata.get(leafSkinMetadata.size() - 1);
  }

  /**
   * find a skin that has its SkinVersion set to 'default', if it exists.
   *
   * @param matchingSkinList A list of Skins that we will look through to find the 'default'.
   * @return Skin with SkinVersion isDefault true, otherwise, null.
   */
  private SkinMetadata _findSkinMetadataWithDefaultVersion(
    Collection<SkinMetadata> matchingSkinList)
  {
    for (SkinMetadata metadata : matchingSkinList)
    {
      SkinVersion skinVersion = metadata.getVersion();

      if (skinVersion != null && skinVersion.isDefault())
      {
        if (_LOG.isFine())
          _LOG.fine("Found default skinMetadata: " + metadata);

        return metadata;
      }
    }

    return null;
  }

  private Map<SkinMetadata, Skin> _skins;
  private final static TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(BaseSkinProvider.class);
}