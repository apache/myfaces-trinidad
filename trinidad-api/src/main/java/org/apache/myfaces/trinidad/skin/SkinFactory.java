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

import java.util.Iterator;
import java.util.Map;
import java.util.WeakHashMap;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * All methods in this class dealing with skin management activities such as loading, creating,
 * managing and disposing skins are deprecated. It is recommended to use SkinProvider instead of
 * SkinFactory for the deprecated APIs. SkinProvider introduces lazy / on-demand loading of skins as
 * opposed to eager loading done by SkinFactory. SkinProvider also introduces the flexibility to
 * create and manage external skin repositories. Thus we have better manageability of Skins and
 * clear separation of external skins and trinidad provided / supported skins.
 * <p/>
 * SkinFactory exists today to provide SkinProvider implementers an easy way to create a Skin object
 * without having to implement Skin interface.This is done using SkinFactory#createSkin API. Skin
 * object created using this API can be used by the SkinProvider implementation's
 * SkinProvider#getSkin methods. SkinProvider implementation should take care of managing the skin
 * and destroy it when required.
 * <p/>
 * Creating a Skin using this SkinFactory#createSkin API is different from adding a Skin into
 * SkinFactory using SkinFactory#addSkin. With SkinFactory#createSkin the responsibility of managing
 * the Skin and returning is in response to SkinProvider#getSkin calls is with the SkinProvider and
 * with SkinFactory#addSkin, this responsibility is with the SkinFactory.
 *
 * @see SkinProvider
 * @see Skin
 * @see SkinMetadata
 */
abstract public class SkinFactory
{
  /**
   * Retrieve the current SkinFactory.
   */
  static public SkinFactory getFactory()
  {
    synchronized (_FACTORIES)
    {
      return _FACTORIES.get(_getClassLoader());
    }
  }

  /**
   * Store the current SkinFactory.
   */
  static public void setFactory(SkinFactory factory)
  {
    synchronized (_FACTORIES)
    {
      ClassLoader cl = _getClassLoader();
      if (_FACTORIES.get(cl) != null)
      {
        throw new IllegalStateException(_LOG.getMessage(
          "FACTORY_ALREADY_AVAILABlE_FOR_THIS_CLASS_LOADER"));
      }

      _FACTORIES.put(cl, factory);
    }
  }

  /**
   * Creates a Skin based on the supplied base skin information and skinMetadata information. This
   * API can be used by SkinProvider implementers to create Skin objects which their SkinProvider
   * implementation supports. As a minimum, the user of this API should pass information to obtain
   * the base skin through 'baseSkinMetadata' and provide information such as id, family, renderkit
   * etc for the new skin through 'skinMetadata'.
   *
   * @param externalContext  valid ExternalContext
   * @param baseSkinMetadata {@link SkinMetadata} metadata to find the base skin for the new skin to
   *                         be created. The user should pass enough information to pick the base
   *                         skin with id / family / version / renderkit. Base skin is obtained
   *                         using SkinProvider#getSkin API.
   * @param skinMetadata     {@link SkinMetadata} metadata to create new Skin. The user should pass
   *                         all information such as id, family, version, renderkit, styleSheetName,
   *                         features, metadata etc. for the new skin.
   * @return a new skin {@link Skin} object created using the skinMetadata {@link SkinMetadata}
   * supplied
   * @throws IllegalArgumentException if the baseSkinId contained in the supplied skinMetadata did
   *                                  not match the id of the baseSkin obtained using
   *                                  baseSkinMetadata
   * @throws ClassCastException       if SkinProvider does not provide as matching base skin as per
   *                                  metadata passed in baseSkinMetadata
   */
  public Skin createSkin(
    ExternalContext externalContext,
    SkinMetadata baseSkinMetadata,
    SkinMetadata skinMetadata)
  {
    throw new UnsupportedOperationException(_LOG.getMessage("SKIN_FACTORY_NO_CREATE_SKIN_SUPPORT",
                                                            this));
  }

  /**
   * Creates a Skin based on the supplied base skin information and skinMetadata information. This
   * API can be used by SkinProvider implementers to create Skin objects which their SkinProvider
   * implementation supports. As a minimum, the user of this API should pass information such as
   * baseSkinId, id, family, renderkit etc for the new skin through 'skinMetadata'.
   *
   * @param externalContext valid ExternalContext
   * @param skinMetadata    {@link SkinMetadata} metadata to create new Skin. The user should
   *                        pass all information such as baseSkinId, id, family, version, renderkit,
   *                        styleSheetName, features, metadata etc. for the new skin. baseSkinId is
   *                        mandatory, since it is used to query the base skin for the new skin
   *                        using SkinProvider#getSkin API.
   * @return a new skin {@link Skin} object created using the skinMetadata {@link SkinMetadata}
   * supplied
   * @throws IllegalArgumentException if the baseSkinId contained in the supplied skinMetadata did
   *                                  not match the id of the baseSkin obtained from
   *                                  SkinProvider#getSkin
   */
  public Skin createSkin(ExternalContext externalContext, SkinMetadata skinMetadata)
  {
    throw new UnsupportedOperationException(_LOG.getMessage("SKIN_FACTORY_NO_CREATE_SKIN_SUPPORT",
                                                            this));
  }

  /**
   * Reloads the skins that was registered with this factory. Subclassers can choose to provide the
   * implementation.
   *
   * @deprecated use SkinProvider SPI to deal with externals skins this functionality will now be
   * replaced by having the SkinProvider implementations reloading skins themselves as and when
   * required.
   */
  @Deprecated
  public void reload()
  {
    _LOG.warning("SKIN_FACTORY_NO_RELOAD_SUPPORT", this);
  }

  /**
   * <p>Register the specified {@link Skin} instance, associated with the specified
   * <code>skinId</code>, to be supported by this {@link SkinFactory}, replacing any previously
   * registered {@link Skin} for this identifier.</p>
   *
   * @param skinId Identifier of the {@link Skin} to register
   * @param skin   {@link Skin} instance that we are registering
   * @deprecated use SkinProvider SPI to deal with externals skins Implementing SkinProvider and
   * exposing skins using SkinProvider#getSkin() method is recommended than using this API to make
   * skins available at runtime.
   */
  @Deprecated
  public abstract void addSkin(String skinId, Skin skin);


  /**
   * <p>Return a {@link Skin} instance for the specified skinId. If there is no registered {@link
   * Skin} for the specified identifier, return <code>null</code>.  The set of available skin
   * identifiers is available via the <code>getSkinIds()</code> method.</p>
   *
   * @param context FacesContext for the request currently being processed, or <code>null</code> if
   *                none is available.
   * @param skinId  Skin identifier of the requested {@link Skin} instance
   * @deprecated use SkinProvider#getSkin method to query skins
   */
  @Deprecated
  public abstract Skin getSkin(FacesContext context, String skinId);

  /**
   * <p>Return a {@link Skin} instance for the specified skinFamily and renderKitId. If there is no
   * registered {@link Skin} for the specified identifier, return <code>null</code>.  The set of
   * available skin identifiers is available via the <code>getSkinIds()</code> method.</p>
   *
   * @param context     FacesContext for the request currently being processed, or <code>null</code>
   *                    if none is available.
   * @param family      family of the requested {@link Skin} instance
   * @param renderKitId RenderKit identifier of the requested {@link Skin} instance
   * @deprecated use SkinProvider#getSkin method to query skins
   */
  @Deprecated
  public abstract Skin getSkin(
    FacesContext context,
    String family,
    String renderKitId);



  /**
   * <p>Return a {@link Skin} instance for the specified skinFamily and renderKitId, and skin
   * version. The best matched skin is returned. If there is no registered {@link Skin} for the
   * specified identifier, return <code>null</code>.  The set of available skin identifiers is
   * available via the <code>getSkinIds()</code> method.</p>
   *
   * @param context     FacesContext for the request currently being processed, or <code>null</code>
   *                    if none is available.
   * @param family      family of the requested {@link Skin} instance
   * @param renderKitId RenderKit identifier of the requested {@link Skin} instance
   * @param version     A string that denotes the skin version name. It can be "default" or the name
   *                    of the version (see the Skin's SkinVersion#getName) or null which returns
   *                    the skin with no version set.
   * @deprecated use SkinProvider#getSkin method to query skins
   */
  @Deprecated
  public abstract Skin getSkin(
    FacesContext context,
    String family,
    String renderKitId,
    String version);

  /**
   * <p>Return an <code>Iterator</code> over the set of skin identifiers registered with this
   * factory. </p>
   *
   * @deprecated use SkinProvider#getSkinMetadata to get the list of skins supported
   */
  @Deprecated
  public abstract Iterator<String> getSkinIds();


  static private ClassLoader _getClassLoader()
  {
    return Thread.currentThread().getContextClassLoader();
  }

  private static final Map<ClassLoader, SkinFactory> _FACTORIES =
    new WeakHashMap<ClassLoader, SkinFactory>();
  private static final TrinidadLogger                _LOG       =
    TrinidadLogger.createTrinidadLogger(SkinFactory.class);
}
