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

import java.util.Collection;
import java.util.Collections;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

/**
 * SkinProvider SPI is used to create, load, manage, and dispose skins. SkinProvider introduces lazy / on-demand
 * loading of skins as opposed to eager loading done by SkinFactory. SkinProvider also introduces the flexibility
 * to create and manage external skin repositories. Thus we have better manageability of Skins and clear separation of
 * external skins and trinidad provided / supported skins.
 * <p/>
 * For each such skin repository externally maintained, the user should register their SkinProvider as an SPI.
 * Classes implementing this abstraction should be listed in the jar's /META-INF/services folder
 * inside of a text file named "org.apache.myfaces.trinidad.skin.SkinProvider".
 * These services will then be run by trinidad skin framework and be used for all SkinProvider#getSkin or
 * SkinProvider#getSkinMetadata calls.
 * <p/>
 * A SkinProvider implementation shall return a Skin object in response to SkinProvider#getSkin API call,
 * if a Skin matching the skinMetadata requested is available in the particular implementation.
 * The SkinProvider implementer can make use of SkinFactory#createSkin API to create a Skin object
 * instead of implementing the abstract Skin APIs. For this, information about the new skin and its base skin
 * has to be passed to SkinFactory#createSkin. See SkinFactory#createSkin documentation for details.
 * SkinProvider#getSkin API implementation can decide to return a cached skin or a new skin object for
 * subsequent request for the same Skin. It can also choose not to return a skin for a criteria which it served
 * before (if the implementation finds that the Skin is now deleted or discontinued).
 * Thus the SkinProvider implementation needs to manage the lifecycle of the Skin by creating, modifying
 * and destroying it.
 *
 * If a Skin supported by the implementation is modified (by the Skin file change or any other change
 * in Skin properties), the SkinProvider implementation needs to convey this to trinidad skinning framework
 * so that the Skin can be reloaded. This can be done using Skin.setDirty() method.
 * Calling this method sets the Skin as dirty and trinidad skinning framework will reload the skin and
 * regenerate the css for the skin.
 * <p/>
 * SkinProvider implementation can optionally publish a list of supported skins by returning a
 * Collection of SkinMetadata objects from SkinProvider#getSkinMetadata API. The collection can be built by the
 * SkinProvider implementations beforehand. This also helps in publishing the information about the skins
 * that it intends to support. See SkinMetadata documentation for details.
 * SkinMetadata created for this method should be done without creating the actual Skin objects.
 * Thus the implementation will support lazy loading of Skins.
 * @see SkinFactory
 * @see SkinMetadata
 */
public abstract class SkinProvider
{
  /**
   * Returns the Skin for the given skin metadata
   *
   * The returned skin is not cached by the framework and thus it
   * is the responsibility of the provider to return the same instance
   * of the skin should it desire so.
   * @param context
   * @param skinMetadata   search criteria object containing the information of skin to be queried
   *                       id, family, renderKit, version are the information used from skin metadata
   *                       to perform search for the skin requested.
   *                       Other fields do not participate in the search.
   * @return Skin matching the search criteria
   */
  public abstract Skin getSkin(FacesContext context, SkinMetadata skinMetadata);

  /**
   * A provider can optionally return information about some or all of the skins it provides.
   * Typically this information is generated without loading the skins
   * as this is a quick way to find out about the Skins in the provider and also thereby
   * supporting lazy loading of Skins.
   * SkinMetadata for a Skin should contain only features and metadata that it
   * explicitly supports. It should not contain the features and metadata that
   * belongs only to its parent skins.
   * @param context
   * @return
   */
  public Collection<SkinMetadata> getSkinMetadata(FacesContext context)
  {
    return Collections.emptyList();
  }

  /**
   * static factory method to get hold of the current SkinProvider instance.
   * This can be used to get a SkinProvider to query for a skin or metadata
   * @param ec
   * @return
   */
  public static SkinProvider getCurrentInstance(ExternalContext ec)
  {
    if (ec == null)
      throw new NullPointerException("ExternalContext is passed as null");

    SkinProvider sp = (SkinProvider) ec.getApplicationMap().get(SKIN_PROVIDER_INSTANCE_KEY);

    return sp;
  }

  /**
   * Key for the SkinProvider stored in ExternalContext
   */
  public static final String SKIN_PROVIDER_INSTANCE_KEY = "org.apache.myfaces.trinidad.skin.SKIN_PROVIDER_INSTANCE";
}