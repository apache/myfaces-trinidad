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
import org.apache.myfaces.trinidad.component.core.output.CoreStatusIndicator;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ArrayMap;

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
    FacesContext        context,
    RenderingContext    rc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    Icon busyIcon = rc.getIcon(SkinSelectors.AF_STATUS_INDICATOR_BUSY_ICON);
    Icon readyIcon = rc.getIcon(SkinSelectors.AF_STATUS_INDICATOR_READY_ICON);

    if (busyIcon == null || readyIcon == null)
    {
      _LOG.warning("STATUS_INDICATOR_MISSING_ICONS");
      return;
    }

    ResponseWriter rw = context.getResponseWriter();

    rw.startElement("span", comp);
    renderId(context, comp);
    renderAllAttributes(context, rc, bean);
    String clientId = getClientId(context, comp);

    Map<String, Object> attrs = new ArrayMap<String, Object>(3);

    attrs.put(Icon.ID_KEY, clientId + "::ready");
    attrs.put(Icon.SHORT_DESC_KEY,
              rc.getTranslatedString("af_statusIndicator.READY"));
    readyIcon.renderIcon(context, rc, attrs);

    attrs.put(Icon.INLINE_STYLE_KEY, "display:none");
    attrs.put(Icon.ID_KEY, clientId + "::busy");
    attrs.put(Icon.SHORT_DESC_KEY,
              rc.getTranslatedString("af_statusIndicator.BUSY"));
    busyIcon.renderIcon(context, rc, attrs);
      
    
    rw.startElement("script", null);
    renderScriptTypeAttribute(context, rc);

    rw.writeText("TrStatusIndicator._register(\"", null);
    rw.writeText(clientId, null);
    rw.writeText("\");", null);
    
    rw.endElement("script");

    rw.endElement("span");
  }

  protected String getDefaultStyleClass(FacesBean bean)
  {
    return SkinSelectors.AF_STATUS_INDICATOR_STYLE;
  }

  private Map<String, Object> _getNodeAttributeMap(
    FacesContext        context,
    UIComponent         comp,
    FacesBean           bean,
    boolean             embed)
  {
    Map<String, Object> attrs = null;
    attrs = new ArrayMap<String, Object>(1);

    attrs.put(Icon.STYLE_CLASS_KEY, getStyleClass(bean));
    attrs.put(Icon.ID_KEY, getClientId(context, comp));

    return attrs;
  }

  private PropertyKey _nameKey;

  private static String _ICON_NAME_PREFIX = "AF";
  private static String _ICON_NAME_SUFFIX = "Icon";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StatusIndicatorRenderer.class);
}
