/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.skin;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidad.skin.SkinVersion;
import org.apache.myfaces.trinidadinternal.config.GlobalConfiguratorImpl;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;


/**
 * Factory for creating Skin objects.
 *
 */
public class SkinFactoryImpl extends SkinFactory
{

  /**
   * Constructor registers default and custom skins
   */
  public SkinFactoryImpl()
  {
    super();
    _initalizeSkins();

  }
  
   /**
    * <p>Register the specified {@link Skin} instance, associated with
    * the specified <code>skinId</code>, to be supported by this
    * {@link SkinFactory}, replacing any previously registered
    * {@link Skin} for this identifier.</p>
    * 
    * @param skinId Identifier of the {@link Skin} to register
    * @param skin {@link Skin} instance that we are registering
    */
  @Override
  public  void addSkin(
    String skinId,
    Skin   skin)
  {
    if (skinId == null || skin == null)
    {
      _LOG.warning("CANNOT_ADD_SKIN");
      return;
    }

    synchronized (this)
    {
      // we could be called when 
      // 1. skin is in READY state -> allow skin addition
      // 2. _reloadIfDirty() is in call stack (in which case we will be in RELOADABLE state) -> allow skin addition
      // 3. _reloadIfDirty() is not in call stack, we could possibly be in DIRTY state -> do not allow skin addition
      if (_skinsState == SkinsState.DIRTY)
      {
        throw new IllegalStateException(_LOG.getMessage("SKIN_ADDITION_ATEMPT_WHEN_FACTORY_DIRTY"));
      }
      
      _skins.put(skinId, skin);
    }
  }


  /**
   * given the skinId, pass back the Skin.
   * @param context FacesContext. If not available, pass in null.
   * @param skinId
   * @return Skin that is in this SkinFactory and has the skinId.
   */
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

    Skin skin = null;
    synchronized (this)
    {
      // we can be called at deploy time, so, it is possible that faces context is null
      if (context != null)
      {
        _reloadIfDirty(context.getExternalContext());
      }

      if (_skins.containsKey(skinId))
      {
        skin = _skins.get(skinId);
      }
    }

    return skin;
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
   */
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
   */
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
    
    // we are in JSF lifecycle when this method gets called, so this time we will have the faces context
    _reloadIfDirty(context.getExternalContext());

    // default render-kit-id, if needed.
    if (renderKitId == null)
      renderKitId = XhtmlConstants.APACHE_TRINIDAD_DESKTOP;

    // loop through each skin in the SkinFactory
    // and see if the family and the renderKitId match
    Skin matchingSkin = null;
    List<Skin> matchingSkinList = new ArrayList<Skin>();

    for(Skin skin : _skins.values())
    {
      if (family.equalsIgnoreCase(skin.getFamily()) &&
          renderKitId.equalsIgnoreCase(skin.getRenderKitId()))
      {
        // exact family+renderKitId match!
        matchingSkinList.add(skin);
      }
    }
    

    if (matchingSkinList.isEmpty())
    {
      // if we get here, that means we couldn't find an exact
      // family/renderKitId match, so return the simple skin
      // that matches the renderkitid.
       if (_LOG.isWarning())
       {
         _LOG.warning("CANNOT_FIND_MATCHING_SKIN", new Object[]{family, renderKitId});
       }

      if (renderKitId.equals(XhtmlConstants.APACHE_TRINIDAD_PORTLET))
        matchingSkin = getSkin(context, _SIMPLE_PORTLET);
      else if (renderKitId.equals(XhtmlConstants.APACHE_TRINIDAD_PDA))
        matchingSkin = getSkin(context, _SIMPLE_PDA);
      else
        matchingSkin = getSkin(context, _SIMPLE_DESKTOP);
    }
    else
    {
      // at this point we know we have something in the matchingSkinList
      // which is a list of matching family and renderKitId skins. Now match the version
      // to find the best matched skin.
        boolean foundMatchingSkin = false;
        boolean versionIsDefault = (_DEFAULT.compareToIgnoreCase(version) == 0);
      // if the user didn't ask for the 'default' version, then look for the exact match
        if (!versionIsDefault)
        {
          for (Skin skin : matchingSkinList)
          {
            SkinVersion skinVersion = skin.getVersion();
            if (skinVersion != null)
            {
              String name = skinVersion.getName(); 
              if (version.equals(name))
              {
                matchingSkin = skin;
                break;
              }
            }
          }          
        }
      // matchingSkin will be null if an exact version match (family+renderKitId+exact version) was not found;
      // we can have an exact version match if the user asks for null version, and we find a skin with no
      // version set.
        if (matchingSkin == null || versionIsDefault)
        {
          // find skin with version= default
          matchingSkin = _getDefaultVersionSkin(matchingSkinList);

          if (matchingSkin == null)
          {
            // get the last skin in the matchingSkinList if there is no skin marked default.
            matchingSkin = matchingSkinList.get(matchingSkinList.size() -1);
          }
          else if ((matchingSkin != null) && versionIsDefault)
          {
            // found the default skin the user wanted
            foundMatchingSkin = true;
          }
        } // end matchingSkin == null || versionIsDefault 
        else
        {
          foundMatchingSkin = true;
        }
        // log messages
        if (foundMatchingSkin)
        {
          if (_LOG.isFine())
            _LOG.fine("GET_SKIN_FOUND_SKIN_VERSION", 
                      new Object[]{family, version, matchingSkin.getId()}); 
        }
        else
        {
          if(_LOG.isWarning())
        {
          if ("".equals(version))
          {
              _LOG.warning("GET_SKIN_CANNOT_FIND_NO_VERSION", 
                           new Object[]{family, matchingSkin.getId()});  
          }
          else
          { 
            _LOG.warning("GET_SKIN_CANNOT_FIND_SKIN_VERSION", 
                         new Object[]{family, version, matchingSkin.getId()}); 
        }
        }
      }
    }
    
    // If we've got a matching skin, wrap it in a RequestSkinWrapper
    // to provide access to request-specific state.
    return (matchingSkin == null) ? null : new RequestSkinWrapper(matchingSkin); 
  }

  @Override
  public Iterator<String> getSkinIds()
  {
    return (_skins.keySet().iterator());
  }

  @Override
  public void reload()
  {
    synchronized (this)
    {
      // just mark it dirty, we will do the reload on next request
      _skinsState = SkinsState.DIRTY;
    }
  }
  
  private void _reloadIfDirty(ExternalContext context)
  {
    synchronized (this)
    {
      // dont try to reload if we are done with or in process of reloading
      if (_skinsState == SkinsState.DIRTY)
      {
        _LOG.fine("Reloading skins begin");
        _skinsState = SkinsState.RELOADING;
        
        // backup the old skins to help in recovery if need be
        Map<String, Skin> oldSkins = _skins;
        
        _initalizeSkins();
        
        try
        {
          // give chance for configurator services to attach any skins that was not defined trinidad-skins.xml
          GlobalConfiguratorImpl.getInstance().reloadSkins(context, this);
        }
        catch (Exception e)
        {
          _LOG.severe("SKIN_RELOAD_FAILURE", e);
          
          // recover to the skins before the reload attempt
          _skins = oldSkins;
        }
        finally
        {
          _skinsState = SkinsState.READY;
          _LOG.fine("Reloading skins complete");
        }
      }
    }
  }

  /**
   * Given a list of Skins, find the one that has its SkinVersion set to 'default', if it exists.
   * @param matchingSkinList A list of Skins that we will look through to find the 'default'.
   * @return Skin with SkinVersion isDefault true, otherwise, null.
   */
  private Skin _getDefaultVersionSkin(List<Skin> matchingSkinList)
  {
    Skin matchingSkin = null;
    for (Skin skin : matchingSkinList)
    {
      SkinVersion skinVersion = skin.getVersion();
      if (skinVersion != null)
      {
        if (skinVersion.isDefault())
        {
          matchingSkin = skin;
          break;
        }
      }
    }
    return matchingSkin;
  }

  private void _initalizeSkins()
  {
    _skins = new LinkedHashMap<String, Skin>();
  }

  /**
   * State of the skins attached to this factory
   */
  private enum SkinsState
  {
    /**
     * Ready to be used
     */
    READY,
    /**
     * Marked dirty, reload pending
     */
    DIRTY,
    /**
     * Reload in progress
     */
    RELOADING
  }

  // Stores all the Skins in this SkinFactory
  private Map<String, Skin> _skins = null;
  private SkinsState _skinsState = SkinsState.READY;
  
  static private final String _SIMPLE_PDA = "simple.pda";
  static private final String _SIMPLE_DESKTOP = "simple.desktop";
  static private final String _SIMPLE_PORTLET = "simple.portlet";
  static private final String _DEFAULT = "default";
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SkinFactoryImpl.class);

}
