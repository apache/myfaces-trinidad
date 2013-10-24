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
import org.apache.myfaces.trinidadinternal.skin.SkinUtils;

/**
 * ExternalSkinProvider serves to maintain backward compatibility with legacy SkinFactory users.
 * Before we introduced SkinProvider SPI, users can register skins in the lifecycle of
 * the Configurator using reloadSkins API.
 * With SkinProvider SPI, this method of registering skins is deprecated. This provider is used to
 * support the existing users who use SkinFactory to register / reload skins. This provider manages
 * the skins added to the SkinFactory and provides methods to perform the SkinFactory operations.
 *
 * Essentially SkinFactory methods will call this provider for methods like addSkin, getSkinIds etc
 * which interact with the skins added to the factory.
 */
public class ExternalSkinProvider extends BaseSkinProvider
{
  /**
   * {@inheritDoc}
   */
  @Override
  public Skin getSkin(FacesContext context, SkinMetadata skinMetadata)
  {
    synchronized (this)
    {
      Skin skin = super.getSkin(context, skinMetadata);

      // ensure that the skin's and its parent's skin additions are added before we return.
      // see _ensureSkinAdditions method documentation for more information on why we need to do this.
      if (skin != null)
        _ensureSkinAdditions(context, skin);

      return skin;
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Skin loadAvailableSkin(FacesContext context, SkinMetadata skinMetadata)
  {
    // this case will never rise with this provider.
    // any skin supported by this provider is added using
    // addSkins API. So the parent class will have all the skins
    // supported by this provider and there is no need to load it
    return null;
  }

  /**
   * called from SkinFactory to reload the skins that are registered with SkinFactory
   * With this provider, the skins registered with SkinFactory are managed in this provider
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
          // give chance for configurator services to attach any skins that was not defined trinidad-skins.xml
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
   * static factory method to get hold of a ExternalSkinProvider object
   * This can be used for easy creation of Skin object without having to
   * implement the abstract class
   * @param ec
   * @return
   */
  public static ExternalSkinProvider getCurrentInstance(ExternalContext ec)
  {
    if (ec == null)
      throw new NullPointerException("ExternalContext is passed as null");

    ExternalSkinProvider esp = (ExternalSkinProvider) ec.getApplicationMap().get(EXTERNAL_SKIN_PROVIDER_KEY);
    return esp;
  }

  /**
   * {@link org.apache.myfaces.trinidad.config.Configurator} allows access to SkinFactory in its init() and
   * reloadSkin() API. These are now deprecated. But for existing use cases where user registers Skins during init() of
   * Configurator, they obtains their base skin by doing something like SkinFactory.getSkin(null, "simple.desktop");
   * Then custom skin is created using the simple skin as base and put it into SkinFactory using SkinFactory.addSkin method.
   * All skins registered using SkinFactory.addSkin method lands up here in ExternalSkinProvider.
   * Such skins will not have the skin additions registered in trinidad-skins.xml because
   * TrinidadSkinProvider was not able to kick in and provide this information. So we need to ensure that
   * skins returned from ExternalSkinProvider the skin additions for that skin and its base skins are added, before we
   * return them to the caller.
   * @param context
   * @param skin
   * @return
   */
  private Skin _ensureSkinAdditions(FacesContext context, Skin skin)
  {
    // It is possible to optimize here by keeping track of skins which already went through
    // this process. Such optimization will avoid repeating this operation for skins which already
    // went through this. However, this is a corner case as there are not many skins are registered
    // like this. Moreover we are deprecating SkinFactory.addSkin and it may be removed
    // in a later release, so there is no point in adding premature optimization.
    TrinidadSkinProvider trinidadSkinProvider = SkinUtils.getTrinidadSkinProvider(context);
    trinidadSkinProvider.ensureSkinAdditions(skin);
    return skin;
  }

  /**
   * Key for the ExternalSkinProvider stored in ExternalContext
   */
  public static final String EXTERNAL_SKIN_PROVIDER_KEY =
    "org.apache.myfaces.trinidad.skin.EXTERNAL_SKIN_PROVIDER_INSTANCE";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ExternalSkinProvider.class);
}