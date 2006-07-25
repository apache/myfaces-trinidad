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

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;

import org.apache.myfaces.adfinternal.ui.laf.base.NodeRoleUtils;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/RowLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:54:11 $
 * @author The Oracle ADF Faces Team
 */
public class RowLayoutRenderer extends XhtmlLafRenderer
{
  protected void prerender(
    RenderingContext context,
    UINode           node) throws IOException
  {
    super.prerender(context, node);

    if (_shouldRenderTable(context))
    {
      renderLayoutTableAttributes(context, ZERO, null);
      context.getResponseWriter().startElement("tr", null);
    }
  }

  protected void postrender(
    RenderingContext context,
    UINode           node) throws IOException
  {
    if (_shouldRenderTable(context))
      context.getResponseWriter().endElement("tr");

    super.postrender(context, node);
  }

  /**
   * Renders attributes of the current node.
   */
  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    if (_shouldRenderTable(context))
    {
      renderAttribute(context, node, "width",  WIDTH_ATTR);
      renderAttribute(context, node, "height", HEIGHT_ATTR);
    }

    renderHAlign(context, node);
    renderAttribute(context, node, "valign", V_ALIGN_ATTR);
  }



  /**
   * Called to render a child.  The default implementation
   * will update the context as needed (e.g., calling pushChild()
   * and popChild()) and call through to the renderer returned
   * by <code>child.getRenderer(context)</code>.
   * <p>
   * @param context the rendering context
   * @param child the child under consideration
   */
  protected void renderChild(
    RenderingContext context,
    UINode           child
    ) throws IOException
  {
    if (child == null)
      return;

    boolean renderCells = renderCellElement(context, child);

    if (renderCells)
    {
      context.getResponseWriter().startElement("td", null);
      renderDefaultCellAttributes(context, child);
      super.renderChild(context, child);
      context.getResponseWriter().endElement("td");
    }
    else
    {
      super.renderChild(context, child);
    }
  }

  protected void renderDefaultCellAttributes(
    RenderingContext context,
    UINode           child) throws IOException
  {
  }

  protected boolean renderCellElement(
    RenderingContext context,
    UINode           child
    )
  {
    return !isEqualMarlinName(child, CELL_FORMAT_NAME);
  }

  protected void renderBetweenIndexedChildren(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderNamedChild(context, node, SEPARATOR_CHILD);
  }

  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    if (_shouldRenderTable(context))
    {
      return "table";
    }
    else
    {
      return "tr";
    }
  }
  /**
   * 
   * @param context
   * @return true if the ancestor node is a table 
   * (tableLayout or messageComponentLayout)
   */
  protected boolean hasTableParent(RenderingContext context)
  {
    UINode ancestor = NodeRoleUtils.getStructuralAncestor(context);
    
    return isEqualMarlinName(ancestor, TABLE_LAYOUT_NAME);
  }
  
  /**
   * 
   * @param context
   * @return true if the &lt;table&gt; html element should be rendered.
   */
  private boolean _shouldRenderTable(RenderingContext context)
  {
    return !hasTableParent(context);
  }
}
