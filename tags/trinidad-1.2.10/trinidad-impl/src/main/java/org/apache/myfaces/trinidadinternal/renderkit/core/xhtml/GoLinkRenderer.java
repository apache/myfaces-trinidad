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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.nav.CoreGoLink;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;

public class GoLinkRenderer extends XhtmlRenderer
{
  public GoLinkRenderer()
  {
    this(CoreGoLink.TYPE);
  }

  protected GoLinkRenderer(FacesBean.Type type)
  {
    super(type);
  }
  
  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _accessKeyKey = type.findKey("accessKey");
    _destinationKey = type.findKey("destination");
    _disabledKey = type.findKey("disabled");
    _onblurKey  = type.findKey("onblur");
    _onfocusKey = type.findKey("onfocus");
    _targetFrameKey = type.findKey("targetFrame");
    _textKey = type.findKey("text");
  }

  @Override
  protected void encodeBegin(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("a", comp);

    if (getDisabled(bean) || !supportsNavigation(arc))
    {
      renderId(context, comp);
      renderStyleAttributes(context, arc, bean);
    }
    else
    {
      renderId(context, comp);
      renderAllAttributes(context, arc, bean);

      // If we have an onclick handler, always provide a destination
      String destination = getDestination(bean);
      if ((destination == null) && hasOnclick(bean))
      {
        destination = "#";
      }

      renderEncodedActionURI(context, "href", destination);

      if (!Boolean.FALSE.equals(
              arc.getAgent().getCapabilities().get(TrinidadAgent.CAP_TARGET)))
      {
        rw.writeAttribute("target", getTargetFrame(bean), null);
      }
    }

    char accessKey;
    if (supportsAccessKeys(arc))
    {
      accessKey = getAccessKey(bean);
      if (accessKey != CHAR_UNDEFINED)
      {
        rw.writeAttribute("accesskey",
                          Character.valueOf(accessKey),
                          "accessKey");
      }                   
    }
    else
    {
      accessKey = CHAR_UNDEFINED;
    }
    
    AccessKeyUtils.renderAccessKeyText(context,
                                       getText(bean),
                                       accessKey,
                                       SkinSelectors.AF_LINKACCESSKEY_STYLE_CLASS);
  }

  @Override
  public void encodeEnd(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.endElement("a");
  }

  /**
   * Renders the client ID as both "id" and "name"
   */
  @Override
  protected void renderId(
    FacesContext context,
    UIComponent  component) throws IOException
  {
    if (shouldRenderId(context, component))
    {
      String clientId = getClientId(context, component);
      // For links, these are actually URI attributes
      context.getResponseWriter().writeURIAttribute("id", clientId, "id");
      context.getResponseWriter().writeURIAttribute("name", clientId, "id");
    }
  }

  @Override
  protected void renderEventHandlers(
    FacesContext context,
    FacesBean    bean) throws IOException
  {
    super.renderEventHandlers(context, bean);
    ResponseWriter rw = context.getResponseWriter();
    rw.writeAttribute("onblur", getOnblur(bean),  "onblur");
    rw.writeAttribute("onfocus", getOnfocus(bean),  "onfocus");
  }

  
  protected char getAccessKey(FacesBean bean)
  {
    return toChar(bean.getProperty(_accessKeyKey));
  }

  protected String getDestination(FacesBean bean)
  {
    return toResourceUri(FacesContext.getCurrentInstance(), bean.getProperty(_destinationKey));
  }

  protected boolean getDisabled(FacesBean bean)
  {
    Object o = bean.getProperty(_disabledKey);
    if (o == null)
      o = _disabledKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  /**
   * Returns true if the bean has onclick;  provided so
   * subclasses that always have onclick can override.
   */ 
  protected boolean hasOnclick(FacesBean bean)
  {
    return getOnclick(bean) != null;
  }

  protected String getOnblur(FacesBean bean)
  {
    if (_onblurKey == null)
      return null;

    return toString(bean.getProperty(_onblurKey));
  }

  protected String getOnfocus(FacesBean bean)
  {
    if (_onfocusKey == null)
      return null;

    return toString(bean.getProperty(_onfocusKey));
  }

  protected String getTargetFrame(FacesBean bean)
  {
    return toString(bean.getProperty(_targetFrameKey));
  }

  protected String getText(FacesBean bean)
  {
    return toString(bean.getProperty(_textKey));
  }

  @Override
  protected String getDefaultStyleClass(FacesBean bean)
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    if (((CoreRenderingContext) arc).isDefaultLinkStyleDisabled())
      return null;

    if (getDisabled(bean))
      return SkinSelectors.LINK_DISABLED_STYLE_CLASS;
    else
      return SkinSelectors.LINK_STYLE_CLASS;
  }

  private PropertyKey _accessKeyKey;
  private PropertyKey _destinationKey;
  private PropertyKey _disabledKey;
  private PropertyKey _onblurKey;
  private PropertyKey _onfocusKey;
  private PropertyKey _targetFrameKey;
  private PropertyKey _textKey;
}
