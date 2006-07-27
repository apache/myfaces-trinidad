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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.TextNode;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;

import org.apache.myfaces.trinidadinternal.ui.composite.RootAttributeMap;
import org.apache.myfaces.trinidadinternal.ui.composite.RootUINodeList;
import org.apache.myfaces.trinidadinternal.ui.composite.UINodeRenderer;

import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafConstants;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/PageButtonBarRenderer.java#0 $) $Date: 10-nov-2005.18:54:59 $
 * @author The Oracle ADF Faces Team
 */
public class PageButtonBarRenderer extends UINodeRenderer
                                   implements UIConstants, XhtmlLafConstants
                                        
{
  private static UINode _createCompositeUINode()
  {
    //
    // create layout when using a small number of items
    //
    TextNode spacer = new TextNode(NBSP_STRING);
    
    MarlinBean smallLayout = new MarlinBean(FLOW_LAYOUT_NAME);
    smallLayout.setNamedChild(SEPARATOR_NAME, spacer);
    
    smallLayout.setIndexedNodeList(RootUINodeList.getNodeList());

    MarlinBean compositeRoot = new MarlinBean(FLOW_LAYOUT_NAME);

    // delegate all of the attributes to the RootAttributeMap
    compositeRoot.setAttributeMap(RootAttributeMap.getAttributeMap());
   
    compositeRoot.addIndexedChild(smallLayout);
    
    return compositeRoot;
  }
  
  /*
  protected void renderContent(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    //
    // Determine the number of visible indexed children
    //
    int visibleChildCount = 0;

    int childCount = node.getIndexedChildCount();
        
    for (int i = 0; i < childCount; i++)
    {
      if (!Boolean.FALSE.equals(
            node.getIndexedChild(i).getAttributeValue(context.RENDERED_ATTR)))
      {
        visibleChildCount++;
      }
    }
    
    
    if (visibleChildCount < 4)
    {
      super.renderContent(context, node);
    }
    else
    {
      // handle rendering choice with button names and OK button
    }
  }
  
  protected void renderBetweenIndexedChildren(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    context.getResponseWriter().writeText(new char[]{NBSP_CHAR}, null);
  }
  */

  protected UINode getRenderingUINode(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return _INSTANCE;
  }

  private static final UINode _INSTANCE = _createCompositeUINode();
}
