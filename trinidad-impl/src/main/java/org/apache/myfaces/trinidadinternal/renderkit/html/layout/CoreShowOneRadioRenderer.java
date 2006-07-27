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
package org.apache.myfaces.trinidadinternal.renderkit.html.layout;

import java.io.IOException;

import java.util.ListIterator;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.component.UIXShowDetail;

import org.apache.myfaces.trinidadinternal.renderkit.RenderUtils;
import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafConstants;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;

/**
 * Renderer for ShowOneRadio
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/html/layout/CoreShowOneRadioRenderer.java#0 $) $Date: 10-nov-2005.19:01:13 $
 * @author The Oracle ADF Faces Team
 */
public class CoreShowOneRadioRenderer extends ShowOneListRendererBase
{
  /**
   *  {@inheritDoc}
   */
  protected void renderListDisplay(FacesContext context,
                                   UIComponent component,
                                   String disclosedChildId)
    throws IOException
  {
    _LOG.finest("CoreShowOneRadioRenderer.renderRadioFacet: " +
                "disclosedChildId: {0}" + disclosedChildId);

    // This renders the select controls alongwith javascript onchange handler.
    UIXRenderingContext rCtx = getRenderingContext(context, component);

    String compId = component.getClientId(context);

    URLEncoder encoder = rCtx.getURLEncoder();
    ResponseWriter out = context.getResponseWriter();

    // draw table to contain the select UI control
    out.startElement("table", component);
    out.writeAttribute("id", compId + _RADIO_TABLE_SUFFIEX_ID_CONST, null);
    out.writeAttribute("border", "0", null);
    out.writeAttribute("cellspacing", "0", null);
    out.writeAttribute("cellpadding", "0", null);

    if (!XhtmlLafRenderer.isInaccessibleMode(rCtx))
    {
      out.writeAttribute("summary", "", null);
    }

    out.startElement("tr", component);

    String label = (String)component.getAttributes().get("label");

    out.startElement("td", component);
    out.writeAttribute("align", "left", null);
    out.writeAttribute("nowrap", Boolean.TRUE, null);
    out.startElement("span", component);

    XhtmlLafRenderer.
      renderStyleClassAttribute(rCtx,
                                XhtmlLafConstants.AF_LABEL_TEXT_STYLE_CLASS);

    out.writeText(encoder.encodeParameter(label), null);

    out.endElement("span");
    out.endElement("td");

    // Render filler / separator between label and select control
    renderSpacerTD(out, component, getLabelControlSeparatorSize());

    _renderRadioItemsInTD(context,
                          component,
                          out,
                          rCtx,
                          compId,
                          disclosedChildId);

    out.endElement("tr");
    out.endElement("table");
  }

  /**
   *  Generates markup for rendering HTML radio controls.
   *
   *  Each radio control is corresponding to a rendered UIXShowDetail child.
   *  The disable showDetail children are shown as disabled radio buttons.
   */
  private void _renderRadioItemsInTD(FacesContext context,
                                     UIComponent component,
                                     ResponseWriter out,
                                     UIXRenderingContext rCtx,
                                     String compId,
                                     String disclosedChildId)
    throws IOException
  {
    out.startElement("td", component);
    out.writeAttribute("valign", "top", null);
    out.writeAttribute("nowrap", Boolean.TRUE, null);

    String formName = RenderUtils.getFormId(context, component);

    // each of the radio buttons would occupy a td in a tr
    // so there will be as many rows as the number of children and each row
    // in turn will have only one td - to contain the radio button
    out.startElement("table", component);
    out.writeAttribute("id", compId + disclosedChildId, null);
    out.writeAttribute("summary", "", null);
    out.writeAttribute("border", "0", null);
    out.writeAttribute("cellspacing", "0", null);
    out.writeAttribute("cellpadding", "0", null);

    URLEncoder encoder = rCtx.getURLEncoder();

    ListIterator children = component.getChildren().listIterator();
    while (children.hasNext())
    {
      UIComponent child = (UIComponent) children.next();
      if (! (child instanceof UIXShowDetail) )
      {
        continue;
        // Can't do any thing with non-showDetail children.
      }
      UIXShowDetail detailItem = (UIXShowDetail) child;

      String childClientId = child.getClientId(context);
      out.startElement("tr", component);
      out.startElement("td", component);

      boolean isRTL = BaseLafUtils.isRightToLeft(rCtx);
      if (isRTL)
      {
        out.writeAttribute("align", "right", null);
      }
      else
      {
        out.writeAttribute("align", "left", null);
      }
      out.writeAttribute("valign", "top", null);
      out.writeAttribute("nowrap", Boolean.TRUE, null);

      out.startElement("span", component);
      out.writeAttribute("id",
                         childClientId + _RADIO_SPAN_SUFFIEX_ID_CONST,
                         null);

      Boolean disabledObj =
        (Boolean) detailItem.getAttributes().get(
          UIConstants.DISABLED_ATTR.getAttributeName());
      boolean disabled = false; // by default is enabled.
      if (disabledObj != null)
      {
        disabled = disabledObj.booleanValue();
      }

      if (! disclosedChildId.equals(childClientId) && (! disabled) )
      {
        boolean isImmediate = detailItem.isImmediate();
        String submitJS = _getRadioSubmitJS(component,
                                            rCtx,
                                            formName,
                                            compId,
                                            childClientId,
                                            isImmediate);
        out.writeAttribute("onclick", submitJS, null);
      }

      // render the radio button now
      out.startElement("input", component);
      out.writeAttribute("id", childClientId, null);
      out.writeAttribute("value", childClientId, null);
      out.writeAttribute("name", compId, null);

      if (disabled)
      {
        out.writeAttribute("disabled", Boolean.TRUE, null);
      }

      out.writeAttribute("type", "radio", null);
      if (disclosedChildId.equals(childClientId) )
      {
        out.writeAttribute("checked", Boolean.TRUE, null);
      }
      out.endElement("input");

      out.startElement("label", component);
      out.writeAttribute("for", childClientId, null);

      Character accessChar =
        (Character) detailItem.getAttributes().get("accessKey");
      if (accessChar != null)
      {
        out.writeAttribute("accessKey", accessChar.toString(), null);
      }

      out.startElement("span", component);

      String radioSpanClass = getFieldTextClass();
      if (disabled)
      {
        radioSpanClass = XhtmlLafConstants.AF_FIELD_TEXT_DISABLED_STYLE_CLASS;
      }
      XhtmlLafRenderer.renderStyleClassAttribute(rCtx,
                                                 radioSpanClass);

      writeLabel(out,
                 detailItem,
                 encoder,
                 (String) detailItem.getAttributes().get("text"));

      out.endElement("span");
      out.endElement("label");
      out.endElement("span");
      out.endElement("td");
      out.endElement("tr");
    }

    out.endElement("table");
    out.endElement("td");
  }

  /**
   *  Gets onclick javascript to be associated with radio onlcick event.
   *
   *  Checks if component is contained within a form, if not, returns null.
   *  Further, checks if PPR is supported and returns a script to be called
   *  for this case else returns a script where PPR is not required.
   */
  private String _getRadioSubmitJS(UIComponent component,
                                   UIXRenderingContext rCtx,
                                   String formName,
                                   String compId,
                                   String detailChildId,
                                   boolean isImmediate)
  {
    if (formName == null)
    {
      _LOG.warning("Page contains no form, it will not work properly");
      return null;
    }

    String validate = "1";
    if (isImmediate)
    {
      validate = "0";
    }

    // Check if PPR enabled, do a _submitPartialChange, else do a formSubmit.
    String onClickHandler = "";
    boolean pprEnabled = elementSupportsPartial(rCtx, compId);
    if (pprEnabled)
    {
      String encodedPartialTargets =
        ShowOneUtils.getEncodedPartialTargets(component, compId);
      StringBuffer jsBuff = new StringBuffer(220);
      jsBuff.append("_submitPartialChange('")
            .append(formName)
            .append("',")
            .append(validate)
            .append(", {partialTargets:'")
            .append(encodedPartialTargets)
            .append("', event:'show',source:'")
            .append(detailChildId)
            .append("'});return true;");

      onClickHandler = jsBuff.toString();
    }
    else
    {
      StringBuffer jsBuff = new StringBuffer(135);
      jsBuff.append("submitForm('")
            .append(formName)
            .append("',")
            .append(validate)
            .append(",{event:'show',source:'")
            .append(detailChildId)
            .append("'});return true;");
      onClickHandler = jsBuff.toString();
    }
    return onClickHandler;
  }

  private static final String _RADIO_TABLE_SUFFIEX_ID_CONST = "_sor_tbl";
  private static final String _RADIO_SPAN_SUFFIEX_ID_CONST =
    _RADIO_TABLE_SUFFIEX_ID_CONST + "_span";

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(CoreShowOneRadioRenderer.class);
}
