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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table;

import java.io.IOException;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.UIXTreeTable;
import org.apache.myfaces.trinidad.model.RowKeySet;

import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.ColumnRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.trinidadinternal.skin.icon.Icon;

public class TreeNodeColumnRenderer extends ColumnRenderer
{

  protected void renderKids(FacesContext          context,
                            RenderingContext   arc,
                            TableRenderingContext tContext,
                            UIComponent           column) throws IOException
  {
    TreeTableRenderingContext ttrc = (TreeTableRenderingContext) tContext;
    boolean isRTL = arc.isRightToLeft();
    UIXTreeTable hGrid = ttrc.getUIXTreeTable();
    final boolean disclosed;
    final String onclick;
    if (hGrid.isContainer())
    {
      RowKeySet treeState = hGrid.getDisclosedRowKeys();
      String jsVarName = ttrc.getJSVarName();
      if (treeState.isContained())
      {
        disclosed = true;
        onclick = TreeUtils.callJSExpandNode(hGrid, jsVarName, false);
      }
      else
      {
        disclosed = false;
        onclick = TreeUtils.callJSExpandNode(hGrid, jsVarName, true);
      }
    }
    else // not a row container
    {
      disclosed = false;
      onclick = null; 
    }

    int focusPath = hGrid.getDepth(TreeUtils.getFocusRowKey(hGrid));
    int depth = hGrid.getDepth() + 1 - focusPath;
    int spacerWidth = _getSpacerWidth(ttrc);

    ResponseWriter writer = context.getResponseWriter();
      
    // In DOM
    // browsers we use margin-left, and have a floating div to displace the
    // arrow. See bug 2296869 for the reason why this approach is
    // preferred.
    writer.startElement("div", null);      

    if (isRTL)
    {
      if (onclick != null)
        depth--;

      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE,
                            "margin-right:" +
                            depth * spacerWidth + "px",
              null);
    }
    else
      writer.writeAttribute(XhtmlConstants.STYLE_ATTRIBUTE, 
              "position:relative;top:0px;left:0px;margin-left:"+
                            depth*spacerWidth+"px",
              null);

    // check to see if this node is in expanded or collapsed state. 
    if (onclick != null)
    {
      writer.startElement("a", null);

      _renderIconID(context, tContext);

      // Render the style class on the link, so that we can 
      // disable the link's text decoration
      renderStyleClass(context, arc,
                       XhtmlConstants.AF_TREE_TABLE_EXPANSION_ICON_STYLE_CLASS);

      writer.writeAttribute("onclick", onclick, null);
      writer.writeAttribute("href", "#", null);

      // Render the expand/collapse Icon
      _renderExpansionIcon(context, arc, disclosed, onclick);
                                  
      writer.endElement("a");
    }
    
    UIComponent nodeStampColumn = ttrc.getTreeNodeStamp();
    // if in screen reader mode render the node depth from the root as well
    _renderNodeStampBasedOnAccessibilty(context, arc, ttrc, nodeStampColumn); 

    writer.endElement("div");
  }


  private int _getSpacerWidth(TreeTableRenderingContext ttrc)
  {
    return ttrc.getSpacerWidth();
  }  


  // Renders the unique id for the expand/collapse icon
  private void _renderIconID(FacesContext          fc,
                             TableRenderingContext tContext) throws IOException
  {
    // we need to render a unique ID for the expand/collapse link, so that
    // PPR can restore the focus correctly after a PPR request:
    String tableName = tContext.getTable().getClientId(fc);
    String id = tableName + NamingContainer.SEPARATOR_CHAR + _ICON_ID; 
    fc.getResponseWriter().writeAttribute("id", id, null);
  }

  // Renders the expansion Icon
  private void _renderExpansionIcon(
    FacesContext          context,
    RenderingContext   arc,
    boolean          disclosed,
    Object           onclick) throws IOException
  {
    final String iconName;
    final String altTextKey;

    if (disclosed)
    {
      iconName = XhtmlConstants.AF_TREE_TABLE_EXPANDED_ICON_NAME;

      altTextKey = (onclick == null) 
        ? _DISABLED_COLLAPSE_TIP_KEY 
        : _COLLAPSE_TIP_KEY;
    }
    else
    {
      iconName = XhtmlConstants.AF_TREE_TABLE_COLLAPSED_ICON_NAME;      
      altTextKey = _EXPAND_TIP_KEY;
    }

    Icon icon = arc.getIcon(iconName);
    if (icon != null)
    {
      Object altText = arc.getTranslatedString(altTextKey);
      OutputUtils.renderIcon(context, arc, icon, altText, null);
    }
  }

  private void _renderNodeStampBasedOnAccessibilty(
    FacesContext          context,
    RenderingContext   arc,
    TreeTableRenderingContext ttrc,
    UIComponent           column) throws IOException
  {      
    if (XhtmlRenderer.isScreenReaderMode(arc))
    {      
      int depth = ttrc.getUIXTreeTable().getDepth() + 1;
      if (arc.isRightToLeft()) 
      {
        super.renderKids(context, arc, ttrc, column);
        TreeUtils.writeNodeLevel(context, arc, depth, _NODE_LEVEL_TEXT_KEY);        
      }
      else 
      {          
        TreeUtils.writeNodeLevel(context, arc, depth, _NODE_LEVEL_TEXT_KEY);         
        super.renderKids(context, arc, ttrc, column);
      }
    }
    else      
        super.renderKids(context, arc, ttrc, column);
  }
        
  // This String is included in the generated IDs that are 
  // rendered for each expand/collapse icon.
  private static final String _ICON_ID = "hgi";
  
  // translation keys
  private static final String _DISABLED_COLLAPSE_TIP_KEY = 
    "af_treeTable.DISABLED_COLLAPSE_TIP";
  private static final String _COLLAPSE_TIP_KEY =
    "af_treeTable.COLLAPSE_TIP";
  private static final String _EXPAND_TIP_KEY = 
    "af_treeTable.EXPAND_TIP";
  private static final String _NODE_LEVEL_TEXT_KEY = 
    "af_treeTable.NODE_LEVEL";       

}
