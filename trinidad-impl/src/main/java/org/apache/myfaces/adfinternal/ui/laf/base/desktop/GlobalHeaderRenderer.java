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

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.component.UIXNavigationLevel;
import org.apache.myfaces.adfinternal.style.Style;
import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.LinkUtils;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafUtils;
import org.apache.myfaces.adfinternal.skin.icon.Icon;

/**
 * GlobalHeader Renderer for the desktop implementation of the 
 * Base Look And Feel.
 * 
 * The base.desktop.GlobalHeaderRenderer exposes a single customizable
 * icon:
 * <ul>
 * <li>af|menuBar::separator-icon: The separator between global header items.
 * </ul>
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/GlobalHeaderRenderer.java#0 $) $Date: 10-nov-2005.18:55:16 $
 * @author The Oracle ADF Faces Team
 */
public class GlobalHeaderRenderer extends HtmlLafRenderer
{
  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    return DIV_ELEMENT;
  }

  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    )throws IOException
  {
    super.renderAttributes(context, node);

    ResponseWriter writer = context.getResponseWriter();
    writer.writeAttribute("width", "100%", null);    
  }  

  /**
   *
   */
  protected void prerender(
    RenderingContext context,
    UINode           node
    )
    throws IOException
  {   
    renderRelatedLinksBlockStart(context, "af_menuBar.BLOCK_TITLE");


    super.prerender(context, node);

    if (isEmpty(context, node))
    {
      renderEmptyGlobalHeader(context, node);
    }
    else
    {
      // Disabled link default style class rendering
      LinkUtils.startDefaultStyleClassDisabled(context);

      ResponseWriter writer = context.getResponseWriter();
      writer.startElement(TABLE_ELEMENT, null);
      renderLayoutTableAttributes(context, "0", null);
      writer.startElement(TABLE_ROW_ELEMENT, null);

      // Get the separator icon and store it away for later
      Icon icon = context.getIcon(AF_MENU_BAR_SEPARATOR_ICON_NAME);
      context.setLocalProperty(_SEPARATOR_ICON_KEY, icon);
    }
  }

  protected void renderContent(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    boolean initialLinkSelectedStatus = LinkUtils.isSelected(context);
    
    UIXNavigationLevel component = (UIXNavigationLevel) node.getUIComponent();     
     
    UINode stamp = node.getNamedChild(context, NODE_STAMP_CHILD);
    
    if(stamp != null)
    { 

      // Save the current key
      Object oldKey = component.getRowKey();      
      int size = component.getRowCount();
      int rowIndex = component.getRowIndex();
      
      for (int i = 0; i < size; i++)
      {
        component.setRowIndex(i);
        renderStamp(context, stamp,i == rowIndex);
        
        if ( i < (size - 1))
          renderBetweenNodes(context);
      }
      
      if (getVisibleIndexedChildCount(context, node) > 0)
        renderBetweenNodes(context);
      
      // Restore the old path
      component.setRowKey(oldKey);

    }
    
    super.renderContent(context, node);
    //Reset the selected status, which might have been changed on rendering
    //  indexed children.
    LinkUtils.setSelected(context, initialLinkSelectedStatus);
  }

  /**
   *
   */
  protected void postrender(
    RenderingContext context,
    UINode           node
    )
    throws IOException
  { 
    if (!isEmpty(context, node))
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.endElement(TABLE_ROW_ELEMENT);         
      writer.endElement(TABLE_ELEMENT);

      // Re-enable link default style class rendering
      LinkUtils.endDefaultStyleClassDisabled(context);
    }

    super.postrender(context, node);

    renderRelatedLinksBlockEnd(context);
  }
  
  protected void renderIndexedChild(
    RenderingContext context,
    UINode           node,
    int              index
    )throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // Start the table cell
    writer.startElement(TABLE_DATA_ELEMENT, null);

    // Render style attributes
    boolean selected = (index == getResolvedSelectedIndexFromCache(context, node));
    renderItemStyleAttrs(context, node, index, selected);

    //Record the selected status so that in case this child happens to be 
    //  a link, some special aspects like accessibility are taken care of.
    LinkUtils.setSelected(context, selected);

    // Render the global header link
    super.renderIndexedChild( context, node, index);

    writer.endElement(TABLE_DATA_ELEMENT);
  }  

    protected void renderStamp(
    RenderingContext context,
    UINode           stamp,
    boolean          selected
    )throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // Start the table cell
    writer.startElement(TABLE_DATA_ELEMENT, null);
    // todo - the index isn't used so putting -1 for now
    renderItemStyleAttrs(context, stamp, -1, selected);

    //Record the selected status so that in case this child happens to be 
    //  a link, some special aspects like accessibility are taken care of.
    LinkUtils.setSelected(context, selected);

    stamp.render(context);

    writer.endElement(TABLE_DATA_ELEMENT);
  }  

  /**
   * Override of renderBetweenIndexedChildren() which 
   * renders the separator Icon.
   */
  protected void renderBetweenIndexedChildren(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderBetweenNodes(context);
  }

/**
   * Override of renderBetweenIndexedChildren() which 
   * renders the separator Icon.
   */
  protected void renderBetweenNodes(
    RenderingContext context
    ) throws IOException
  {
    // Get the separator icon and render it in a table cell
    Icon icon = getSeparatorIcon(context);

    renderTableDataIcon(context, icon, AF_MENU_BAR_SEPARATOR_STYLE_CLASS);
  }

  /**
   * Override of getStyleClass() which forces style class
   * to OraGlobalHeader.
   */
  protected Object getStyleClass(
    RenderingContext context,
    UINode           node
    )
  {
    return AF_MENU_BAR_STYLE_CLASS;
  }

  /**
   * Renders the style attributes for global header items
   */
  protected void renderItemStyleAttrs(
    RenderingContext context,
    UINode           node,
    int              index,
    boolean          selected
    ) throws IOException
  {
    String styleClass = null;

    if (selected)
      styleClass = AF_MENU_BAR_SELECTED_STYLE_CLASS;
    else
      styleClass = AF_MENU_BAR_ENABLED_STYLE_CLASS;

    renderStyleClassAttribute(context, styleClass);
  }

  /**
   * Returns the separator Icon
   */
  protected Icon getSeparatorIcon(RenderingContext context)
  {
    return (Icon)context.getLocalProperty(0, _SEPARATOR_ICON_KEY, null);
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
    if (getVisibleIndexedChildCount(context, node) != 0)
      return false;
      
          
    UIXNavigationLevel component = (UIXNavigationLevel) node.getUIComponent();   
    int rowCount = component.getRowCount();      
    
    if (rowCount > 0 )
      return false;
      
    return true;
  }

  /**
   * Renders the empty global header
   */
  protected void renderEmptyGlobalHeader(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object text = node.getAttributeValue(context, TEXT_ATTR);

    if (text != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.writeText(text, TEXT_ATTR.getAttributeName());
    }
    else if (isIE(context))
    {
      Style style = XhtmlLafUtils.getClassStyle(context, 
                                                AF_MENU_BAR_STYLE_CLASS);
      if (style != null)
      {
        String minHeight = style.getProperty("min-height");
        renderSpacer(context, null, minHeight);
      }
    }
  }
 
  // Key for local property which holds the separator icon
  private static final Object _SEPARATOR_ICON_KEY = new Object();
}
