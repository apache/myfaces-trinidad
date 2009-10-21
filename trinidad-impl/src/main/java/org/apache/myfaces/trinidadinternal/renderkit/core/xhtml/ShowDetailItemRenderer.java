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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXShowOne;
import org.apache.myfaces.trinidad.component.core.layout.CoreShowDetailItem;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.event.DisclosureEvent;
import org.apache.myfaces.trinidad.context.RequestContext;

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
  
  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _disclosedKey = type.findKey("disclosed");
  }

  @SuppressWarnings("unchecked")
  @Override
  public void decode(FacesContext context, UIComponent component)
  {
    Map<String, String> parameters = 
      context.getExternalContext().getRequestParameterMap();
    
    Object event = parameters.get(TrinidadRenderingConstants.EVENT_PARAM);
    if (TrinidadRenderingConstants.HIDE_EVENT.equals(event) ||
        TrinidadRenderingConstants.SHOW_EVENT.equals(event))
    {
      Object source = parameters.get(TrinidadRenderingConstants.SOURCE_PARAM);
      String id = component.getClientId(context);
      
      if (id.equals(source))
      {
        boolean isDisclosed = TrinidadRenderingConstants.SHOW_EVENT.equals(event);
        (new DisclosureEvent(component, isDisclosed)).queue();
        // Add ourselves as a PPR target - except, if we're in a 
        // ShowOne, then really the whole parent has to get
        // repainted
        UIComponent pprComponent;
        if (component.getParent() instanceof UIXShowOne)
          pprComponent = component.getParent();
        else
          pprComponent = component;

        RequestContext.getCurrentInstance().addPartialTarget(pprComponent);
      }
    }
  }

  @Override
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

  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
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
