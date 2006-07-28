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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * Renderer for rendering spacers.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/SpacerRenderer.java#0 $) $Date: 10-nov-2005.18:54:13 $
 * @author The Oracle ADF Faces Team
 */
public class SpacerRenderer extends XhtmlLafRenderer
{
  public void render(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object width = node.getAttributeValue(context, WIDTH_ATTR);
    // If width not specified, avoid the t.gif way, use block-level element.
    if (width == null)
    {
      renderVerticalSpacer(context, node.getAttributeValue(context, 
                                                           HEIGHT_ATTR));
    }
    else
    {
      // The span is written out here because the writer needs to see the UIComponent.
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("span", node.getUIComponent());
      renderSpacer(context,
                   node.getAttributeValue(context, WIDTH_ATTR),
                   node.getAttributeValue(context, HEIGHT_ATTR),
                   getID(context, node));
      writer.endElement("span");
    }                 
  }
}
