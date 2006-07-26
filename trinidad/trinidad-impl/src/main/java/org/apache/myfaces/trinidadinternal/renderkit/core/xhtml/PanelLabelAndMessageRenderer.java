/*
 * Copyright  2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelLabelAndMessage;

import org.apache.myfaces.trinidadinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.trinidadinternal.util.MessageUtils;


/**
 */
public class PanelLabelAndMessageRenderer extends LabelAndMessageRenderer
{
  public PanelLabelAndMessageRenderer()
  {
    super(CorePanelLabelAndMessage.TYPE);
  }
  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _forKey = type.findKey("for");
    _labelInlineStyleKey = type.findKey("labelStyle");
  }    

  protected boolean labelShowRequired(FacesBean bean)
  {
    // Simpler algorithm for panelLabelAndMessage
    return getShowRequired(bean);
  }
  
 
  protected String getRootStyleClass(FacesBean bean)
  {
    return "af|panelLabelAndMessage";
  }
  
  protected String getLabelFor(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean)
  {
    String forValue = getFor(bean);

    return MessageUtils.getClientIdFor(context,
                                       component,
                                       forValue);
  }

  protected void renderFieldCellContents(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    // The structure of this part of the DOM looks like this:
    // +------------------+-----------+
    // | indexed children | end facet |
    // +------------------+-----------+
    // | help facet                   |
    // +------------------------------+
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("table", component);
    OutputUtils.renderLayoutTableAttributes(context, arc, "0", null/*width*/);

    UIComponent end = getFacet(component, CorePanelLabelAndMessage.END_FACET);
    UIComponent help = getFacet(component, CorePanelLabelAndMessage.HELP_FACET);

    short helpColSpan = 1;

    // Build the main row:
    rw.startElement("tr", null);
    rw.startElement("td", null);
    encodeAllChildren(context, component);
    rw.endElement("td");
    if (end != null)
    {
      helpColSpan = 2;
      rw.startElement("td", null);
      // =-= mcc TODO apply className for "af|panelLabelAndMessage::end-facet"
      // renderStyleClass(context, arc, ...);
      encodeChild(context, end);
      rw.endElement("td");
    }

    // Build the help row:
    if (help != null)
    {
      rw.endElement("tr");
      rw.startElement("tr", null);
      rw.startElement("td", null);
      rw.writeAttribute("colspan", helpColSpan, null);
      // =-= mcc TODO apply className for "af|panelLabelAndMessage::help-facet"
      // renderStyleClass(context, arc, ...);
      encodeChild(context, help);
      rw.endElement("td");
    }

    rw.endElement("tr");
    rw.endElement("table");
  }

  protected boolean hasMessage(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean)
  {
    return false;
  }

  protected String getFor(FacesBean bean)
  {
    return toString(bean.getProperty(_forKey));
  }

  protected String getLabelInlineStyleKey(FacesBean bean)
  {
    return toString(bean.getProperty(_labelInlineStyleKey));
  }

  private PropertyKey _forKey;
  private PropertyKey _labelInlineStyleKey;
}
