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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/FrameBorderLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:54:23 $
 * @author The Oracle ADF Faces Team
 */
public class FrameBorderLayoutRenderer extends
                        org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.FrameBorderLayoutRenderer
{
  
  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
  }

  public boolean isSupportedNode(
    RenderingContext context,
    UINode           node
    )
  {
    // only supported if advanced frames are supported
    //return supportsFrames(context);
    return true;
  }

  /**
   * overwrites the superclass method so that no children are written except
   * for the named ones.
   */
  protected void renderContent(
    RenderingContext context,
    UINode           node
    )
    throws IOException
  {
    String leftName       = LEFT_CHILD;
    String innerLeftName  = INNER_LEFT_CHILD;
    String rightName      = RIGHT_CHILD;
    String innerRightName = INNER_RIGHT_CHILD;
    
    UINode center = getNamedChild(context, node, CENTER_CHILD);
    UINode top    = getNamedChild(context, node, TOP_CHILD);
    UINode bottom = getNamedChild(context, node, BOTTOM_CHILD);
    UINode left   = getNamedChild(context, node, leftName);
    UINode right  = getNamedChild(context, node, rightName);
    UINode innerLeft   = getNamedChild(context, node, innerLeftName);
    UINode innerRight  = getNamedChild(context, node, innerRightName);

    _renderFrameAsLink( context, top );
    _renderFrameAsLink( context, left );
    _renderFrameAsLink( context, innerLeft );
    _renderFrameAsLink( context, center );
    _renderFrameAsLink( context, innerRight );
    _renderFrameAsLink( context, right );
    _renderFrameAsLink( context, bottom );

  }

  private void _renderFrameAsLink(
    RenderingContext context,
    UINode           node
  )throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    if (node != null )
    {
      Object source = node.getAttributeValue( context, SOURCE_ATTR );

      if ( source != null )
      {
        String uriRef = source.toString();
        MarlinBean link = new MarlinBean(LINK_NAME);
        link.setAttributeValue(TEXT_ATTR, uriRef );
        link.setAttributeValue(DESTINATION_ATTR, uriRef );
        link.render(context);
        writer.startElement("br", null);
        writer.endElement("br");
      }
    }
  }


  protected String getElementName(
    RenderingContext context,
    UINode node
    )
  {
    return null;
  }
}
