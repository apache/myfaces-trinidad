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
package org.apache.myfaces.adfinternal.ui.laf.base.pda;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafRenderer;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;

import java.io.IOException;

/**
 */
public class TipRenderer extends XhtmlLafRenderer
{
  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);
  }

  protected void prerender(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.prerender(context, node);

    ResponseWriter writer = context.getResponseWriter();
    // tip icon
    writer.startElement("span", null);
    renderIcon(context, "tip.gif", "15", "13");
    writer.endElement("span");

    // tip label
    writer.startElement("span", null);
    renderStyleClassAttribute(context, TIP_LABEL_STYLE_CLASS);
    writer.writeAttribute("nowrap", Boolean.TRUE, null);

    if (isRightToLeft(context))
    {
      writer.writeText(NBSP_STRING, null);
      writer.writeText(getTranslatedValue(context, "af_panelTip.TIP"), null);
    }
    else
    {
      writer.writeText(getTranslatedValue(context, "af_panelTip.TIP"), null);
      writer.writeText(NBSP_STRING, null);
    }
    writer.endElement("span");
    // content
    writer.startElement("span", null);
    renderStyleClassAttribute(context, TIP_TEXT_STYLE_CLASS);
  }

  protected void postrender(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.endElement("span");
    super.postrender(context, node);
  }


  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    return "div";
  }

}
