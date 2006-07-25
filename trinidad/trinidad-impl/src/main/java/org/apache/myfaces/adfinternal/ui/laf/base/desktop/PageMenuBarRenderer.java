/*
 * Copyright  2003-2006 The Apache Software Foundation.
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

package org.apache.myfaces.adfinternal.ui.laf.base.desktop;

import java.io.IOException;

import org.apache.myfaces.adf.component.UIXPage;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.LinkUtils;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.PageRendererUtils;


/**
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/PageMenuBarRenderer.java#0 $) $Date: 10-nov-2005.18:55:30 $
 * @author The Oracle ADF Faces Team
 */
public class PageMenuBarRenderer extends GlobalHeaderRenderer
{

  protected void renderContent(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    boolean initialLinkSelectedStatus = LinkUtils.isSelected(context);
    UINode pageNode = context.getParentContext().getAncestorNode(0);
    UIXPage component = (UIXPage) pageNode.getUIComponent();

    UINode stamp = this.getNamedChild(context, pageNode, NODE_STAMP_CHILD);
    if(stamp == null)
      return;

    // Save the current path
    int startDepth = getIntAttributeValue(context, node, LEVEL_ATTR, 0);
    Object oldPath = component.getRowKey();
    boolean isNewPath = PageRendererUtils.setNewPath(context, component, startDepth);
    if (!isNewPath)
      return;

    int size = component.getRowCount();
    int rowIndex = component.getRowIndex();

    for (int i = 0; i < size; i++)
    {
      component.setRowIndex(i);
      renderStamp(context, stamp,i == rowIndex);

      if ( i < (size - 1))
        renderBetweenNodes(context);
    }

    // Restore the old path
    component.setRowKey(oldPath);

    // Reset the selected status, which might have been changed on rendering
    LinkUtils.setSelected(context, initialLinkSelectedStatus);
  }




  /**
   * Checks to see whether the globalHeader is empty (contains no
   * indexed children).
   */
  protected boolean isEmpty(
    RenderingContext context,
    UINode           node
    )
  {
    UINode pageNode = context.getParentContext().getAncestorNode(0);
    UIXPage component = (UIXPage) pageNode.getUIComponent();
    int startDepth = getIntAttributeValue(context, node, LEVEL_ATTR, 0);

    return PageRendererUtils.isEmpty(context, component, startDepth);
  }
}
