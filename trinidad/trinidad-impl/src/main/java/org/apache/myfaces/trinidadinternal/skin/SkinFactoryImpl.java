/*
 * Copyright  2004-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidadinternal.skin;

import java.util.HashMap;
import java.util.Iterator;
import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;



/**
 * Factory for creating Skin objects.
 *
 * @author The Oracle ADF Faces Team
 */
public class SkinFactoryImpl extends SkinFactory
{

  /**
   * Constructor registers default and custom skins
   */
  public SkinFactoryImpl()
  {
    super();
    _skins = new HashMap();

  }

  public  void addSkin(
    String skinId,
    Skin   skin)
  {
    if (skinId == null || skin == null)
    {
      _LOG.warning("Can't add Skin with null skinId or null skin");
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
  public Skin getSkin(
    FacesContext context,
    String       skinId)
  {

    if (skinId == null)
    {
      _LOG.warning("Can't get Skin with null skinId");
      return null;
    }

    Skin skin = null;
    synchronized (_skins)
    {
      if (_skins.containsKey(skinId))
      {
        skin = (Skin) _skins.get(skinId);
      }
    }

    return skin;
  }

  /**
   * given the skinFamily and renderKitId, pass back the Skin.
   * @param context FacesContext for the request currently being
   * processed, or <code>null</code> if none is available.
   * @param family skin family of the requested {@link Skin} instance
   * @param renderKitId RenderKit identifier of the requested
   *  {@link Skin} instance
   */
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

    // if there isn't a specific renderKitId specified, get the skin
    // with the default render kit.
    if (renderKitId == null) renderKitId = _RENDER_KIT_ID_CORE;

    // loop through each skin in the SkinFactory
    // and see if the family and the renderKitId match
    Iterator skinIterator = getSkinIds();

    Skin skin = null;

    while (skinIterator.hasNext())
    {
      String skinId = (String)skinIterator.next();
      skin = getSkin(context, skinId);

      if (family.equalsIgnoreCase(skin.getFamily()) &&
          renderKitId.equalsIgnoreCase(skin.getRenderKitId()))
      {
        // exact family+renderKitId match!
        return skin;
      }
    }

    // if we get here, that means we couldn't find an exact
    // family/renderKitId match, so return the simple skin
    // that matches the renderkitid.
     if (_LOG.isWarning())
     {
       _LOG.warning("Can't find a skin that matches family " + family + 
                    " and renderkit " + renderKitId + ", so we will" +
                    " use the simple skin");
     }

    if (renderKitId.equals(_RENDER_KIT_ID_PDA))
    {
      return getSkin(context, _SIMPLE_PDA);
    }
    else
    {
      return getSkin(context, _SIMPLE_DESKTOP);
    }

  }

  public Iterator getSkinIds()
  {
    return (_skins.keySet().iterator());
  }


  // Stores all the Skins in this SkinFactory
  private HashMap _skins = null;

  static private final String _RENDER_KIT_ID_CORE = "org.apache.myfaces.trinidad.desktop";
  static private final String _RENDER_KIT_ID_PDA = "org.apache.myfaces.trinidad.pda";
  static private final String _SIMPLE_PDA = "simple.pda";
  static private final String _SIMPLE_DESKTOP = "simple.desktop";

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SkinFactoryImpl.class);

}
