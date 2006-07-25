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
package org.apache.myfaces.adfinternal.ui.laf.base.xhtml;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;

/**
 * Renders a frame.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/FrameRenderer.java#0 $) $Date: 10-nov-2005.18:53:54 $
 * @author The Oracle ADF Faces Team
 */
public class FrameRenderer extends XhtmlLafRenderer
{
  public boolean isSupportedNode(
    RenderingContext context,
    UINode           node
    )
  {
    // only supported if advanced frames are supported
    return supportsFrames(context);
  }

  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    ResponseWriter writer = context.getResponseWriter();

    writer.writeAttribute("frameborder", "0", null);
    writer.writeAttribute("marginwidth",
                          node.getAttributeValue(context, MARGIN_WIDTH_ATTR),
						              null);
    writer.writeAttribute("marginheight",
                          node.getAttributeValue(context, MARGIN_HEIGHT_ATTR),
						              null);
    writer.writeAttribute("noresize", Boolean.TRUE, null);

    Object source = node.getAttributeValue(context, SOURCE_ATTR);
    renderEncodedURIAttribute(context, "src", source);

    Object longDesc = node.getAttributeValue(context, LONG_DESC_URL_ATTR);
    renderEncodedURIAttribute(context, "longdesc", longDesc);

    writer.writeAttribute("scrolling",
                          node.getAttributeValue(context, SCROLLING_ATTR),
						  null);
  }


  public void render(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    UINode parent = context.getRenderedAncestorNode(1);

    //
    // complain if our parent isn't a FrameBorderLayout
    //
    if ((parent == null) ||
        !isEqualMarlinName(parent, FRAME_BORDER_LAYOUT_NAME))
    {
      _LOG.warning("Frames must appear inside FrameBorderLayouts");
    }
    else
    {
      super.render(context, node);
    }
  }

  protected void renderContent(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    // don't render any children
  }


  /**
   * Override to return the id and then anme, in that order
   */
  protected Object getID(
    RenderingContext context,
    UINode           node
    )
  {
    return getIDOrName(context, node);
  }


  /**
   * Override to render both the id and the name
   */
  protected void renderID(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderNameAndID(context, node);
  }

  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    return "frame";
  }

  private static final ADFLogger _LOG = ADFLogger.createADFLogger(FrameRenderer.class);
}
