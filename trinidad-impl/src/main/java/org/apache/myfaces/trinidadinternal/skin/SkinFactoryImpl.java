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
package org.apache.myfaces.trinidadinternal.skin;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidad.skin.SkinFeatures;
import org.apache.myfaces.trinidad.skin.SkinMetadata;
import org.apache.myfaces.trinidad.skin.SkinProvider;
import org.apache.myfaces.trinidad.skin.SkinVersion;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.skin.provider.ExternalSkinProvider;


/**
 * Factory for creating Skin objects.
 * To create and manage skins external to skin framework, use SkinProvider.
 * @see org.apache.myfaces.trinidad.skin.SkinProvider
 */
public class SkinFactoryImpl extends SkinFactory
{

  /**
   * Constructor registers default and custom skins
   */
  public SkinFactoryImpl()
  {
    super();
  }

  /**
   * @inheritDoc
   */
  @Override
  public Skin createSkin(FacesContext context, SkinMetadata baseSkinMetadata, SkinMetadata skinMetadata)
  {
    if (context == null ||  baseSkinMetadata == null || skinMetadata == null)
      throw new NullPointerException(_LOG.getMessage("NULL_FC_SKIN_BASE_SKIN_METADATA"));

    if (baseSkinMetadata.getId() != null && !baseSkinMetadata.getId().equals(skinMetadata.getBaseSkinId()))
      throw new IllegalArgumentException(_LOG.getMessage("INVALID_BASE_SKIN_ID"));

    Skin baseSkin = SkinProvider.getCurrentInstance(context.getExternalContext()).getSkin(context, baseSkinMetadata);

    if (baseSkin == null)
      throw new IllegalArgumentException(_LOG.getMessage("INVALID_BASE_SKIN"));

    return new SkinExtension(baseSkin, skinMetadata);
  }

  /**
   * @inheritDoc
   */
  @Override
  public Skin createSkin(FacesContext context, SkinMetadata skinMetadata)
  {
    if (context == null || skinMetadata == null)
      throw new NullPointerException(_LOG.getMessage("NULL_FC_SKIN_METADATA"));

    if (skinMetadata.getBaseSkinId() == null)
      throw new NullPointerException(_LOG.getMessage("NULL_BASE_SKIN_ID"));

    SkinMetadata baseSkinMetadata = new SkinMetadata.Builder().id(skinMetadata.getBaseSkinId()).build();

    return createSkin(context, baseSkinMetadata, skinMetadata);
  }

   /**
    * <p>Register the specified {@link Skin} instance, associated with
    * the specified <code>skinId</code>, to be supported by this
    * {@link SkinFactory}, replacing any previously registered
    * {@link Skin} for this identifier.</p>
    * 
    * <p>A warning will be logged if a previously registered {@link Skin} was replaced, since it could produce
    * inconsistent results if the application cached the previously registered Skin.</p>
    *
    * @param skinId Identifier of the {@link Skin} to register
    * @param skin {@link Skin} instance that we are registering
    * @deprecated use SkinProvider SPI to deal with externals skins
    */
  @Override
  @Deprecated
  public  void addSkin(
    String skinId,
    Skin   skin)
  {
    if (skinId == null || skin == null)
    {
      _LOG.warning("CANNOT_ADD_SKIN");
      return;
    }

    SkinMetadata.Builder builder = new SkinMetadata.Builder().id(skinId)
            .family(skin.getFamily()).version(skin.getVersion())
            .renderKitId(SkinMetadata.RenderKitId.fromId(skin.getRenderKitId()));

    if (skin.getBaseSkin() != null)
      builder.baseSkinId(skin.getBaseSkin().getId());

    if (skin.getSkinFeatures() != null)
      builder.features(new SkinFeatures(skin.getSkinFeatures()));

    Skin previousValue = _getExternalSkinProvider(null).addSkin(builder.build(), skin);

    if (previousValue != null)
      _LOG.warning("DUPLICATE_ADD_SKIN_TO_SKIN_FACTORY", skinId);
  }


  /**
   * given the skinId, pass back the Skin.
   * @param context FacesContext. If not available, pass in null.
   * @param skinId
   * @return Skin that is in this SkinFactory and has the skinId.
   * @deprecated use SkinProvider to query skins
   */
  @Deprecated
  @Override
  public Skin getSkin(
    FacesContext context,
    String       skinId)
  {

    if (skinId == null)
    {
      _LOG.warning("CANNOT_GET_SKIN_WITH_NULL_SKINID");
      return null;
    }

    return SkinUtils.getSkinProvider(context).getSkin(context, new SkinMetadata.Builder().id(skinId).build());
  }

  /**
   * given the skinFamily and renderKitId, pass back the Skin.
   * @param context FacesContext for the request currently being
   * processed, or <code>null</code> if none is available.
   * @param family skin family of the requested {@link Skin} instance
   * @param renderKitId RenderKit identifier of the requested:
   * XhtmlConstants.APACHE_TRINIDAD_DESKTOP, XhtmlConstants.APACHE_TRINIDAD_PDA, or
   * XhtmlConstants.APACHE_TRINIDAD_PORTLET
   *  {@link Skin} instance
   * @deprecated use SkinProvider to query skins
   */
  @Deprecated
  @Override
  public Skin getSkin(
    FacesContext context,
    String       family,
    String       renderKitId)
  {
    return getSkin(context, family, renderKitId, null);
  }

  /**
   * Given the skin family, renderKitId, and version, return the best matched skin.
   * The skin picking logic is:
   * If an exact family, renderKitId, and version (including null or "") is found, return that skin
   * Else if the user asks for version "default", return the skin with family and renderKitId with version marked
   * default. If version wasn't default and does not match any version for the skins with family and renderKitId, 
   * then return the 'default' skin if there is one marked or return the last entry in the list
   * of matching family/renderKitId skins.
   * @param context
   * @param family
   * @param renderKitId
   * @param version The version of the skin you want to return. This can be 
   *                "default", or a version name (e.g., "v1"), or null or ""
   *                (if you want the skin that does not have a version set).
   * @return the best matched Skin given the family, renderKitId, and version.
   * @deprecated use SkinProvider to query skins
   */
  @Deprecated
  @Override
  public Skin getSkin(
    FacesContext context,
    String       family,
    String       renderKitId,
    String       version)
  {
    // By setting the version to the empty string if version is null, we can
    // get the skin that has a matching family and renderkit and has no skin version.
    // (A Skin with no version returns SkinVersion.EMPTY_SKIN_VERSION for skin.getVersion(),
    // and getName will be "")
    if (version == null)
        version = "";
  
    // given a skinFamily and a renderKitId, figure out the skinId.
    // If we don't have an exact match, use the simple skin that matches the
    // renderKitId (simple.desktop or simple.pda)
    if (family == null)
     throw new NullPointerException("Null skin family");

    Skin matchingSkin = SkinUtils.getSkinProvider(context).getSkin(context, new SkinMetadata.Builder().
      family(family).version(new SkinVersion(version)).renderKitId(SkinMetadata.RenderKitId.fromId(renderKitId)).build());

    return (matchingSkin == null) ? null : new RequestSkinWrapper(matchingSkin);
  }

  /**
   * @inheritDoc
   */
  @Deprecated
  @Override
  public Iterator<String> getSkinIds()
  {
    Collection<SkinMetadata> metadatas = _getExternalSkinProvider(null).getSkinMetadata(null);
    Set<String> ids = new HashSet<String>();

    for (SkinMetadata metadata : metadatas)
      ids.add(metadata.getId());

    return ids.iterator();
  }

  /**
   * @inheritDoc
   */
  @Deprecated
  @Override
  public void reload()
  {
    _getExternalSkinProvider(null).reload();
  }

  private ExternalSkinProvider _getExternalSkinProvider(FacesContext context)
  {
    if (context == null)
      context = FacesContext.getCurrentInstance();

    if (context == null)
      throw new NullPointerException("Cannot retrieve FacesContext. FacesContext is null.");

    return ExternalSkinProvider.getCurrentInstance(context.getExternalContext());
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SkinFactoryImpl.class);

}
