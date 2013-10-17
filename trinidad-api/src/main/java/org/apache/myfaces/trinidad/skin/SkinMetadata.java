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
package org.apache.myfaces.trinidad.skin;

import java.util.HashMap;
import java.util.Map;

import javax.el.ValueExpression;

/**
 * SkinMetadata encapsulates information about a skin. SkinMetadata is instantiated using the nested Builder class.
 * <pre>
 * {@code
 * SkinMetadata mySkinMetadata = new SkinMetadata.Builder().baseSkinId("alta-v1.desktop").id("mySkin.desktop")
 *                               .family("mySkin").renderKitId(SkinMetadata.RenderKit.DESKTOP)
 *                              .styleSheetName("my/css/location.css").version(new SkinVersion("v1")).build();
 * }
 * </pre>
 * SkinMetadata is used mainly in two ways:
 * First, as a representation of a Skin.
 * Second, as a means to query Skins.
 * <p/>
 * SkinProvider#getSkinMetadata returns a collection of SkinMetadata.
 * The collection returned from this method can be built by the SkinProvider implementations beforehand, so that
 * it can publish the information about the skins that it supports to callers. Creation of SkinMetadata is a far
 * less costly process than creating a Skin. SkinMetadata created for this purpose should contain all relevant
 * information.
 * SkinProviders can also use SkinMetadata objects to create the actual Skin objects using SkinFactory#createSkin
 * API as well. In this case, SkinMetadata should be well formed with all relevant information so that SkinFactory
 * can create the skin.
 * <p/>
 * SkinMetadata is used as query object to retrieve Skin from a SkinProvider#getSkin. Querying Skins are based on
 * id, family, version and renderkit. For querying Skins, user should set either id or family as mandatory.
 * Other information (even if present) are not relevant while searching for a skin.
 * <p/>
 * @see SkinProvider
 * @see Builder
 * @see Skin
 * @see SkinFeatures
 * @see CustomMetadata
 */
public final class SkinMetadata
{
  private SkinMetadata(Builder builder)
  {
    this._id = builder._id;
    this._family = builder._family;

    if (builder._version == null)
      this._version = SkinVersion.EMPTY_SKIN_VERSION;
    else
      this._version = builder._version;

    if (builder._renderKitId == null)
      this._renderKitId = RenderKitId.DESKTOP;
    else
      this._renderKitId = builder._renderKitId;

    this._baseSkinId = builder._baseSkinId;
    this._styleSheetName = builder._styleSheetName;
    this._resourceBundleName = builder._resourceBundleName;
    this._translationSource = builder._translationSource;
    this._features = builder._features;
    this._metadata = builder._metadata;

  }

  /**
   * @return id of the Skin
   */
  public String getId()
  {
    return _id;
  }

  /**
   * @return family which the Skin belongs to
   */
  public String getFamily()
  {
    return _family;
  }

  /**
   * @return renderKit {@link RenderKit} id of the Skin
   */
  public String getRenderKitId()
  {
    return _renderKitId.id();
  }

  /**
   * @return version {@link SkinVersion} of the Skin
   */
  public SkinVersion getVersion()
  {
    return _version;
  }

  /**
   * @return styleSheetName for the Skin. This points to the actual Skin file
   * and is loaded using NameResolver
   * {@link org.apache.myfaces.trinidad.share.io.NameResolver} implementation.
   */
  public String getStyleSheetName()
  {
    return _styleSheetName;
  }

  /**
   * @return resource bundle name for the Skin which contains the translations.
   * If both translationSource and resourceBundleName is provided,
   * resourceBundleName takes the preceedence.
   */
  public String getResourceBundleName()
  {
    return _resourceBundleName;
  }

  /**
   * @return translation source for the Skin, alternate way to specify the translations.
   * If both translationSource and resourceBundleName is provided,
   * resourceBundleName takes the preceedence.
   */
  public ValueExpression getTranslationSource()
  {
    return _translationSource;
  }

  /**
   * @return id of the base Skin for this Skin
   */
  public String getBaseSkinId()
  {
    return _baseSkinId;
  }

  /**
   * @return features {@link SkinFeatures} for this Skin
   */
  public SkinFeatures getFeatures()
  {
    return _features;
  }

  /**
   * @return metadata {@link CustomMetadata} for this Skin
   */
  public CustomMetadata getMetadata()
  {
    return _metadata;
  }


  @Override
  public boolean equals(Object o)
  {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    SkinMetadata that = (SkinMetadata) o;

    if (_id != null ? !_id.equals(that._id) : that._id != null) return false;
    if (_family != null ? !_family.equals(that._family) : that._family != null) return false;
    if (_renderKitId != null ? !_renderKitId.equals(that._renderKitId) : that._renderKitId != null) return false;
    if (_resourceBundleName != null ? !_resourceBundleName.equals(that._resourceBundleName) : that._resourceBundleName != null) return false;
    if (_styleSheetName != null ? !_styleSheetName.equals(that._styleSheetName) : that._styleSheetName != null) return false;
    if (_translationSource != null ? !_translationSource.equals(that._translationSource) : that._translationSource != null) return false;
    if (_version != null ? !_version.equals(that._version) : that._version != null) return false;
    if (_baseSkinId != null ? !_baseSkinId.equals(that._baseSkinId) : that._baseSkinId != null) return false;
    if (_features != null ? !_features.equals(that._features) : that._features != null) return false;
    if (_metadata != null ? !_metadata.equals(that._metadata) : that._metadata != null) return false;

    return true;
  }

  @Override
  public int hashCode()
  {
    int result = 17;
    result = 37 * result + (null == _id ? 0 : _id.hashCode());
    result = 37 * result + (null == _family ? 0 : _family.hashCode());
    result = 37 * result + (null == _renderKitId ? 0 : _renderKitId.hashCode());
    result = 37 * result + (null == _version ? 0 : _version.hashCode());
    result = 37 * result + (null == _resourceBundleName ? 0 : _resourceBundleName.hashCode());
    result = 37 * result + (null == _translationSource ? 0 : _translationSource.hashCode());
    result = 37 * result + (null == _styleSheetName ? 0 : _styleSheetName.hashCode());
    result = 37 * result + (null == _baseSkinId ? 0 : _baseSkinId.hashCode());
    result = 37 * result + (null == _features ? 0 : _features.hashCode());
    result = 37 * result + (null == _metadata ? 0 : _metadata.hashCode());
    return result;
  }

  @Override
  public String toString()
  {
    StringBuilder toString = new StringBuilder("SkinMetadata [");

    if (_id != null)
      toString.append("Id: ").append(_id).append(", ");
    if (_family != null)
      toString.append("Family: ").append(_family).append(", ");
    if (_renderKitId != null)
      toString.append("RenderKitId: ").append(_renderKitId).append(", ");
    if (_version != SkinVersion.EMPTY_SKIN_VERSION)
      toString.append("Version: ").append(_version).append(", ");
    if (_resourceBundleName != null)
      toString.append("ResourceBundleName: ").append(_resourceBundleName).append(", ");
    if (_styleSheetName != null)
      toString.append("StyleSheetName: ").append(_styleSheetName).append(", ");
    if (_translationSource != null)
      toString.append("TranslationSource: ").append(_translationSource).append(", ");
    if (_baseSkinId != null)
      toString.append("BaseSkinId: ").append(_baseSkinId).append(", ");
    if (_features != null && !_features.isEmpty())
      toString.append(_features).append(", ");
    if (_metadata != null && !_metadata.isEmpty())
      toString.append(_metadata).append(", ");
    toString.append(']') ;

    return toString.toString();
  }

  /**
   * Enumeration representing RenderKit
   */
  public enum RenderKitId
  {
    /**
     * Renderkit Id for DESKTOP
     */
    DESKTOP("org.apache.myfaces.trinidad.desktop"),

    /**
     * Renderkit Id for PDA
     */
    PDA("org.apache.myfaces.trinidad.pda"),

    /**
     * Renderkit Id for PORTLET
     */
    PORTLET("portlet");

    private RenderKitId(String id)
    {
      if (id == null) throw new NullPointerException();

      _id = id;
    }

    /**
     * @return the id of this RenderKit.
     */
    public String id()
    {
      return _id;
    }

    @Override
    public String toString()
    {
      return _id;
    }

    /**
     * Returns the RenderKit instance or <code>null</code> if no id matches or id is passed as null.
     * @param id of RenderKit to return
     * @return RenderKit with the specified id
     * @throws IllegalArgumentException if there is no enum with the specified name.
     */
    public static RenderKitId fromId(String id)
    {
      if (id == null)
        return null;

      RenderKitId renderKit = ID_TO_RENDER_KIT.get(id);

      if (renderKit == null)
        throw new IllegalArgumentException();

      return renderKit;
    }

    private static final Map<String, RenderKitId> ID_TO_RENDER_KIT = new HashMap<String, RenderKitId>();

    static
    {
      RenderKitId[] instances = RenderKitId.class.getEnumConstants();

      for (int i = 0; i < instances.length; i++)
      {
        ID_TO_RENDER_KIT.put(instances[i].toString(), instances[i]);
      }
    }

    private final String _id;
  }

  /**
   * convenience builder class for SkinMetadata
   * SkinMetadata object can be created using this:
   * <pre>
   * {@code
   * SkinMetadata mySkinMetadata = new SkinMetadata.Builder().baseSkinId("alta-v1.desktop").id("mySkin.desktop")
   *                               .family("mySkin").renderKitId(SkinMetadata.RenderKit.DESKTOP)
   *                              .styleSheetName("my/css/location.css").version(new SkinVersion("v1")).build();
   * }
   * </pre>
   */
  public static class Builder
  {

    public Builder()
    {
      this._features = new SkinFeatures();
      this._metadata = new CustomMetadata();
    }

    /**
     * Mandatory while creating a Skin using SkinFactory#createSkin
     * can be used while querying Skin using SkinProvider#getSkin
     * either this or family is typically set while querying Skin
     * @param id
     * @return
     */
    public Builder id(String id)
    {
      this._id = id;
      return this;
    }

    /**
     * Mandatory while creating a Skin using SkinFactory#createSkin
     * can be used while querying Skin using SkinProvider#getSkin
     * either this or id is typically set while querying Skin
     * @param family
     * @return
     */
    public Builder family(String family)
    {
      this._family = family;
      return this;
    }

    /**
     * Recommended to be set while creating a Skin using SkinFactory#createSkin
     * can be used while querying Skin using SkinProvider#getSkin
     * @param renderKitId
     * @return
     */
    public Builder renderKitId(RenderKitId renderKitId)
    {
      this._renderKitId = renderKitId;
      return this;
    }

    /**
     * Mandatory while creating a Skin using SkinFactory#createSkin
     * Not used while querying Skin using SkinProvider#getSkin
     * @param styleSheetName
     * @return
     */
    public Builder styleSheetName(String styleSheetName)
    {
      this._styleSheetName = styleSheetName;
      return this;
    }

    /**
     * Recommended to be set while creating a Skin using SkinFactory#createSkin
     * Not used while querying Skin using SkinProvider#getSkin
     * @param resourceBundleName
     * @return
     */
    public Builder resourceBundleName(String resourceBundleName)
    {
      this._resourceBundleName = resourceBundleName;
      return this;
    }

    /**
     * Recommended to be set while creating a Skin using SkinFactory#createSkin
     * Not used while querying Skin using SkinProvider#getSkin
     * @param translationSource
     * @return
     */
    public Builder translationSource(ValueExpression translationSource)
    {
      this._translationSource = translationSource;
      return this;
    }

    /**
     * Mandatory while creating a Skin using SkinFactory#createSkin
     * Not used while querying Skin using SkinProvider#getSkin
     * @param baseSkinId
     * @return
     */
    public Builder baseSkinId(String baseSkinId)
    {
      this._baseSkinId = baseSkinId;
      return this;
    }

    /**
     * Recommended to be set while creating a Skin using SkinFactory#createSkin
     * can be used while querying Skin using SkinProvider#getSkin
     * @param version
     * @return
     */
    public Builder version(SkinVersion version)
    {
      this._version = version;
      return this;
    }

    /**
     * Recommended to be set (if applicable) while creating a Skin using SkinFactory#createSkin
     * Not used while querying Skin using SkinProvider#getSkin
     * @param features
     * @return
     */
    public Builder features(SkinFeatures features)
    {
      // this is intialized to an empty object in the constructor.
      // we always let it be an empty object, not null
      if (features != null)
        this._features = features;

      return this;
    }

    /**
     * Recommended to be set (if applicable) while creating a Skin using SkinFactory#createSkin
     * Not used while querying Skin using SkinProvider#getSkin
     * @param metadata
     * @return
     */
    public Builder metadata(CustomMetadata metadata)
    {
      // this is intialized to an empty object in the constructor.
      // we always let it be an empty object, not null
      if (metadata != null)
        this._metadata = metadata;

      return this;
    }

    /**
     * call this method after you have set all the information that you wanted to
     * @return
     */
    public SkinMetadata build()
    {
      return new SkinMetadata(this);
    }

    private String _id;
    private String _family;
    private RenderKitId _renderKitId;
    private String _styleSheetName;
    private String _resourceBundleName;
    private ValueExpression _translationSource;
    private String _baseSkinId;
    private SkinVersion _version;
    private SkinFeatures _features;
    private CustomMetadata _metadata;
  }

  private String _id;
  private String _family;
  private RenderKitId _renderKitId;
  private String _baseSkinId;
  private String _styleSheetName;
  private String _resourceBundleName;
  private ValueExpression _translationSource;
  private SkinVersion _version;
  private SkinFeatures _features;
  private CustomMetadata _metadata;
}