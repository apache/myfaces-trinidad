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
package org.apache.myfaces.adfinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.PropertyKey;
import org.apache.myfaces.adf.component.core.layout.CoreShowDetailItem;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adf.event.DisclosureEvent;
import org.apache.myfaces.adf.context.AdfFacesContext;

public class ShowDetailItemRenderer extends XhtmlRenderer
{
  public ShowDetailItemRenderer()
  {
    this(CoreShowDetailItem.TYPE);
  }
  
  protected ShowDetailItemRenderer(FacesBean.Type type)
  {
    super(type);
  }
  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _disclosedKey = type.findKey("disclosed");
  }

  public void decode(FacesContext context, UIComponent component)
  {
    Map parameters =  context.getExternalContext().getRequestParameterMap();
    Object event = parameters.get(XhtmlConstants.EVENT_PARAM);
    if (XhtmlConstants.HIDE_EVENT.equals(event) ||
        XhtmlConstants.SHOW_EVENT.equals(event))
    {
      Object source = parameters.get(XhtmlConstants.SOURCE_PARAM);
      String id = component.getClientId(context);
      
      if (id.equals(source))
      {
        boolean isDisclosed = XhtmlConstants.SHOW_EVENT.equals(event);
        (new DisclosureEvent(component, isDisclosed)).queue();
        AdfFacesContext.getCurrentInstance().addPartialTarget(component);
      }
    }
  }


  public boolean getRendersChildren()
  {
    return true;
  }

  protected boolean getDisclosed(FacesBean bean)
  {
    Object o = bean.getProperty(_disclosedKey);
    if (o == null)
      o = _disclosedKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected void encodeAll(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("span", component);
    renderId(context, component);
    renderAllAttributes(context, arc, bean);
    
    if (getDisclosed(bean))
    {
      encodeAllChildren(context, component);
    }
    rw.endElement("span");
  }
  
  private PropertyKey _disclosedKey;
}
