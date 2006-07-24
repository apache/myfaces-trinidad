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

package org.apache.myfaces.adfinternal.ui.laf.base.desktop;

import org.apache.myfaces.adf.component.UIXHierarchy;
import org.apache.myfaces.adf.component.UIXPage;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.PageRendererUtils;


/**
 * Renderer for outputting lists with a number, bullet, letter, etc in front
 * of each child.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/PageMenuListRenderer.java#0 $) $Date: 10-nov-2005.18:55:31 $
 * @author The Oracle ADF Faces Team
 */
public class PageMenuListRenderer extends MenuListRenderer
{

  protected UIXHierarchy getHierarchyBase(
    RenderingContext context,
    UINode           node
  )
  {
    UINode pageNode = context.getParentContext().getAncestorNode(0);
    return (UIXPage) pageNode.getUIComponent();
  }

  protected UINode getStamp(
    RenderingContext context,
    UINode           node
    )
  {
    UINode pageNode = context.getParentContext().getAncestorNode(0);
    return getNamedChild(context, pageNode, NODE_STAMP_CHILD);
  }

  protected boolean setNewPath(
    RenderingContext context,
    UINode           node,
    UIXHierarchy      component
  )
  {
    int startDepth = getIntAttributeValue(context, node, LEVEL_ATTR, 0);
    return PageRendererUtils.setNewPath(context, ((UIXPage)component), startDepth);
  }
}
