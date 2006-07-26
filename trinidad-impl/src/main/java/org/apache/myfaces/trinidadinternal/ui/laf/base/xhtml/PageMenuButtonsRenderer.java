/*
 * Copyright  2001-2006 The Apache Software Foundation.
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

import org.apache.myfaces.trinidad.component.UIXHierarchy;
import org.apache.myfaces.trinidad.component.UIXPage;
import org.apache.myfaces.trinidad.component.core.nav.CoreCommandNavigationItem;

import org.apache.myfaces.trinidadinternal.ui.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/PageMenuButtonsRenderer.java#0 $) $Date: 10-nov-2005.18:54:06 $
 * @author The Oracle ADF Faces Team
 */
public class PageMenuButtonsRenderer extends GlobalButtonBarRenderer
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
