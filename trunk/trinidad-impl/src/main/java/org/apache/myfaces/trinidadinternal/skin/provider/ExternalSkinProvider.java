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

import java.util.Map;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidad.skin.SkinMetadata;
import org.apache.myfaces.trinidadinternal.config.GlobalConfiguratorImpl;

/**
 * ExternalSkinProvider serves to maintain backward compatibility with legacy SkinFactory users.
 * Before we introduced SkinProvider SPI, users can register skins in the lifecycle of the
 * Configurator using reloadSkins API. With SkinProvider SPI, this method of registering skins is
 * deprecated. This provider is used to support the existing users who use SkinFactory to register /
 * reload skins. This provider manages the skins added to the SkinFactory and provides methods to
 * perform the SkinFactory operations.
 * <p/>
 * Essentially SkinFactory methods will call this provider for methods like addSkin, getSkinIds etc
 * which interact with the skins added to the factory.
 */
public class ExternalSkinProvider extends BaseSkinProvider
{
  /**
   * Key for the ExternalSkinProvider stored in ExternalContext
   */
  public static final String EXTERNAL_SKIN_PROVIDER_KEY =
    "org.apache.myfaces.trinidad.skin.EXTERNAL_SKIN_PROVIDER_INSTANCE";

  /**
   * {@inheritDoc}
   */
  @Override
  public Skin getSkin(ExternalContext context, SkinMetadata skinMetadata)
  {
    return super.getSkin(context, skinMetadata);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Skin loadAvailableSkin(ExternalContext context, SkinMetadata skinMetadata)
  {
    // this case will never rise with this provider.
    // any skin supported by this provider is added using
    // addSkins API. So the parent class will have all the skins
    // supported by this provider and there is no need to load it
    return null;
  }

  /**
   * called from SkinFactory to reload the skins that are registered with SkinFactory With this
   * provider, the skins registered with SkinFactory are managed in this provider
   */
  public final void reload()
  {
    synchronized (this)
    {
      FacesContext fc = FacesContext.getCurrentInstance();

      if (fc != null)
      {
        SkinFactory factory = SkinFactory.getFactory();
        _LOG.fine("Reloading skins begin");

        // backup the old skins to help in recovery if need be
        Map<SkinMetadata, Skin> oldSkins = getSkins();
        initSkins();

        try
        {
          // give chance for configurator services to attach any skins that was not defined
          // trinidad-skins.xml
          GlobalConfiguratorImpl.getInstance().reloadSkins(fc.getExternalContext(), factory);
        }
        catch (Exception e)
        {
          _LOG.severe("SKIN_RELOAD_FAILURE", e);
          setSkins(oldSkins);
        }
        finally
        {
          _LOG.fine("Reloading skins complete");
        }
      }
    }
  }

  /**
   * static factory method to get hold of a ExternalSkinProvider object This can be used for easy
   * creation of Skin object without having to implement the abstract class
   *
   * @param ec
   * @return
   */
  public static ExternalSkinProvider getCurrentInstance(ExternalContext ec)
  {
    if (ec == null)
      throw new NullPointerException("ExternalContext is passed as null");

    ExternalSkinProvider esp =
      (ExternalSkinProvider) ec.getApplicationMap().get(EXTERNAL_SKIN_PROVIDER_KEY);
    return esp;
  }

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(ExternalSkinProvider.class);
}