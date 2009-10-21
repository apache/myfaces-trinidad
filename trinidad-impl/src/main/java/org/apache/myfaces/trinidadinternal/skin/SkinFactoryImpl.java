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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants;


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
    _skins = new HashMap<String, Skin>();

  }

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

    synchronized (_skins)
    {
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
    synchronized (_skins)
    {
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

    // given a skinFamily and a renderKitId, figure out the skinId.
    // If we don't have an exact match, use the simple skin that matches the
    // renderKitId (simple.desktop or simple.pda)
   if (family == null)
     throw new NullPointerException("Null skin family");

    // default render-kit-id, if needed.
    if (renderKitId == null)
      renderKitId = TrinidadRenderingConstants.APACHE_TRINIDAD_DESKTOP;

    // loop through each skin in the SkinFactory
    // and see if the family and the renderKitId match
    Skin matchingSkin = null;

    for(Skin skin : _skins.values())
    {
      if (family.equalsIgnoreCase(skin.getFamily()) &&
          renderKitId.equalsIgnoreCase(skin.getRenderKitId()))
      {
        // exact family+renderKitId match!
        matchingSkin = skin;
        break;
      }
    }

    if (matchingSkin == null)
    {
      // if we get here, that means we couldn't find an exact
      // family/renderKitId match, so return the simple skin
      // that matches the renderkitid.
       if (_LOG.isWarning())
       {
         _LOG.warning("CANNOT_FIND_MATCHING_SKIN", new Object[]{family, renderKitId});
       }

      // if we get here, that means we couldn't find an exact
      // family/renderKitId match, so return the simple skin
      // that matches the renderkitid.

      if (renderKitId.equals(TrinidadRenderingConstants.APACHE_TRINIDAD_PORTLET))
        matchingSkin = getSkin(context, _SIMPLE_PORTLET);
      else if (renderKitId.equals(TrinidadRenderingConstants.APACHE_TRINIDAD_PDA))
        matchingSkin = getSkin(context, _SIMPLE_PDA);
      else
        matchingSkin = getSkin(context, _SIMPLE_DESKTOP);
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


  // Stores all the Skins in this SkinFactory
  private Map<String, Skin> _skins = null;

  static private final String _SIMPLE_PDA = "simple.pda";
  static private final String _SIMPLE_DESKTOP = "simple.desktop";
  static private final String _SIMPLE_PORTLET = "simple.portlet";
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SkinFactoryImpl.class);

}
