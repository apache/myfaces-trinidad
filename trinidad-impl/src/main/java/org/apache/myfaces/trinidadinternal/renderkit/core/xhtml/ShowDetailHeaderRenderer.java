/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CoreShowDetailHeader;

import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderer;

public class ShowDetailHeaderRenderer
  extends PanelHeaderRenderer
{
  public ShowDetailHeaderRenderer()
  {
    super(CoreShowDetailHeader.TYPE);
  }

  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _detailRenderer = new DetailRenderer(type);
    _disclosedKey = type.findKey("disclosed");
  }

  public void decode(FacesContext context, UIComponent component)
  {
    // Delegate decoding to the showDetail renderer
    _detailRenderer.decode(context, component);
  }


  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    // map the resource value keys that are used in showDetail and header
    // to the keys we need to use in this renderer.
    Map originalResourceKeyMap = arc.getSkinResourceKeyMap();
    try
    {

      arc.setSkinResourceKeyMap(_RESOURCE_KEY_MAP);
      super.encodeAll(context, arc, component, bean);
    }
    finally
    {
      //restore original map
      arc.setSkinResourceKeyMap(originalResourceKeyMap);
    }
  }

  protected void renderIcon(FacesContext context, RenderingContext arc, 
                            UIComponent component, FacesBean bean, String messageType)
    throws IOException
  {
    delegateRenderer(context, arc, component, bean, _detailRenderer);
  }

  protected String getMessageType(FacesBean bean)
  {
    // Not currently supported
    return null;
  }

  protected boolean getDisclosed(FacesBean bean)
  {
    Object o = bean.getProperty(_disclosedKey);
    if (o == null)
      o = _disclosedKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected boolean shouldRenderChildren(FacesBean bean)
  {
    return getDisclosed(bean);
  }


  private class DetailRenderer extends ShowDetailRenderer
  {
    public DetailRenderer(FacesBean.Type type)  
    {
      super(type);
    }
    
    protected void renderId(FacesContext context, UIComponent component)
      throws IOException
    {
    }

    protected void renderAllAttributes(FacesContext context, 
                                       RenderingContext arc, 
                                       FacesBean bean)
      throws IOException
    {
    }

    protected boolean renderAsInline()
    {
      return true;
    }

    protected String getDisclosureText(RenderingContext arc, 
                                       FacesBean bean, boolean disclosed)
    {
      return null;
    }
  }
  
  private CoreRenderer _detailRenderer;
  private PropertyKey  _disclosedKey;
  
  private static final Map _RESOURCE_KEY_MAP  =  new HashMap();
  static
  {
    _RESOURCE_KEY_MAP.put("af_showDetail.DISCLOSED" ,
                          "af_showDetailHeader.DISCLOSED");
    _RESOURCE_KEY_MAP.put("af_showDetail.UNDISCLOSED" ,
                          "af_showDetailHeader.UNDISCLOSED");
    _RESOURCE_KEY_MAP.put("af_showDetail.DISCLOSED_TIP" ,
                          "af_showDetailHeader.DISCLOSED_TIP");
    _RESOURCE_KEY_MAP.put("af_showDetail.UNDISCLOSED_TIP" ,
                          "af_showDetailHeader.UNDISCLOSED_TIP");
    _RESOURCE_KEY_MAP.put(XhtmlConstants.AF_PANEL_HEADER_STYLE_CLASS,
                          XhtmlConstants.AF_SHOW_DETAIL_HEADER_STYLE_CLASS);
    _RESOURCE_KEY_MAP.put(XhtmlConstants.AF_SHOW_DETAIL_DISCLOSED_ICON_NAME,
                          XhtmlConstants.AF_SHOW_DETAIL_HEADER_SD_DISCLOSED_ICON_NAME);
    _RESOURCE_KEY_MAP.put(XhtmlConstants.AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME,
                          XhtmlConstants.AF_SHOW_DETAIL_HEADER_SD_UNDISCLOSED_ICON_NAME);
  }
}
