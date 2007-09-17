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
import java.util.Collections;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.output.CoreStatusIndicator;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Icon;

public class StatusIndicatorRenderer extends XhtmlRenderer
{
  public StatusIndicatorRenderer()
  {
    super(CoreStatusIndicator.TYPE);
  }

  @Override
  protected void encodeBegin(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    String clientId = getClientId(context, comp);
    
    rw.startElement(XhtmlConstants.SPAN_ELEMENT, comp);
    renderId(context, comp);
    renderAllAttributes(context, rc, bean);
    rw.startElement(XhtmlConstants.SPAN_ELEMENT, null);
    rw.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, clientId + "::ready", null);
    renderReadyContent(context, rc, comp, bean);
    rw.endElement(XhtmlConstants.SPAN_ELEMENT);
    
    rw.startElement(XhtmlConstants.SPAN_ELEMENT, null);
    rw.writeAttribute(XhtmlConstants.ID_ATTRIBUTE, clientId + "::busy", null);
    rw.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE, "display:none", null);
    renderBusyContent(context, rc, comp, bean);
    rw.endElement(XhtmlConstants.SPAN_ELEMENT);
      
    rw.startElement(XhtmlConstants.SCRIPT_ELEMENT, null);
    renderScriptTypeAttribute(context, rc);
    rw.writeText("TrStatusIndicator._register(\"" + clientId + "\");", null);
    rw.endElement(XhtmlConstants.SCRIPT_ELEMENT);

    rw.endElement(XhtmlConstants.SPAN_ELEMENT);
  }

  @Override
  protected String getDefaultStyleClass(FacesBean bean)
  {
    return SkinSelectors.AF_STATUS_INDICATOR_STYLE;
  }
  
  protected void renderBusyContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean) throws IOException
  {
    renderDefaultContent(context, 
                         rc, 
                         comp,
                         SkinSelectors.AF_STATUS_INDICATOR_BUSY_ICON,
                         "af_statusIndicator.BUSY",
                         "busy",
                         SkinSelectors.AF_STATUS_INDICATOR_BUSY_STYLE);
  }
  
  protected void renderReadyContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    FacesBean        bean) throws IOException
  {
    renderDefaultContent(context, 
                         rc, 
                         comp,
                         SkinSelectors.AF_STATUS_INDICATOR_READY_ICON,
                         "af_statusIndicator.READY",
                         "ready",
                         SkinSelectors.AF_STATUS_INDICATOR_READY_STYLE);
  }
  
  private void renderDefaultContent(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      comp,
    String           iconName,
    String           iconDesc,
    String           facetName,
    String           facetStyleClass) throws IOException
  {

    ResponseWriter rw = context.getResponseWriter();

    // Render the icon
    Icon icon = rc.getIcon(iconName);
    if (icon != null && !icon.isNull())
    {
      Map<String, String> attrs = 
        Collections.singletonMap(Icon.SHORT_DESC_KEY, 
                                 rc.getTranslatedString(iconDesc));
      
      icon.renderIcon(context, rc, attrs);
    }
    
    // Render the facet
    UIComponent facet = getFacet(comp, facetName);
    if (facet != null)
    {
      rw.startElement(XhtmlConstants.SPAN_ELEMENT, null);
      renderStyleClass(context, rc, facetStyleClass);
      encodeChild(context, facet);
      rw.endElement(XhtmlConstants.SPAN_ELEMENT);
    }
  }
}
