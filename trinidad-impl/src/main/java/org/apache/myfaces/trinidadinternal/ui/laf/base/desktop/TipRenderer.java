/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * Renders the page or section level tip UI element.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/TipRenderer.java#0 $) $Date: 10-nov-2005.18:56:21 $
 * @author The Oracle ADF Faces Team
 */
public class TipRenderer extends HtmlLafRenderer
{


  protected void prerender(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.prerender(context, node);
    
    ResponseWriter writer = context.getResponseWriter();
    
     // tip label
    writer.startElement("span", null);
    renderStyleClassAttribute(context, TIP_LABEL_STYLE_CLASS);

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
