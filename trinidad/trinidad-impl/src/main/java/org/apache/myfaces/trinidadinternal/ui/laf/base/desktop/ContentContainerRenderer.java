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



import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;


/**
 * ContentContainer Renderer for the desktop implementation of the
 * Base Look And Feel.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/ContentContainerRenderer.java#0 $) $Date: 10-nov-2005.18:55:13 $
 * @author The Oracle ADF Faces Team
 */
public class ContentContainerRenderer extends HtmlLafRenderer
{
  /**
   * Implementation of ElementRenderer.getName();
   */
  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    return TABLE_NAME;
  }

  /**
   * Override of BaseRenderer.renderAttributes().
   */
  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    )throws IOException
  {
    super.renderAttributes(context, node);

    // Render layout table attrs, including width
    Object width = node.getAttributeValue(context, WIDTH_ATTR);
    if (width == null)
      width = _DEFAULT_WIDTH;

    super.renderLayoutTableAttributes(context, "0", width);
  }

  /**
   * Returns the style class for the content container
   */
  protected Object getStyleClass(
    RenderingContext context,
    UINode           node
    )
  {
    Object styleClass = null;
    Object background = getBackground(context, node);

  
    // If we don't have a header, we use the
    // af|panelBox::content-<Background> style classes.
    // If we do have a header, we use the 
    // af|panelBox::transparent<Background> style classes
    
    // Check to see if we have a header
    Object text = getText(context, node);

    if (text == null)
    {
      if (BACKGROUND_TRANSPARENT.equals(background))
        styleClass = AF_PANEL_BOX_CONTENT_TRANSPARENT_STYLE_CLASS;
      else if ( BACKGROUND_MEDIUM.equals(background))
        styleClass = AF_PANEL_BOX_CONTENT_MEDIUM_STYLE_CLASS;
      else if ( BACKGROUND_DARK.equals(background))
        styleClass = AF_PANEL_BOX_CONTENT_DARK_STYLE_CLASS;
      else
        styleClass = AF_PANEL_BOX_CONTENT_LIGHT_STYLE_CLASS;
    }
    else
    {
      if (BACKGROUND_TRANSPARENT.equals(background))
        styleClass = AF_PANEL_BOX_TRANSPARENT_STYLE_CLASS;
      else if ( BACKGROUND_MEDIUM.equals(background))
        styleClass = AF_PANEL_BOX_MEDIUM_STYLE_CLASS;
      else if ( BACKGROUND_DARK.equals(background))
        styleClass = AF_PANEL_BOX_DARK_STYLE_CLASS;
      else
        styleClass = AF_PANEL_BOX_LIGHT_STYLE_CLASS;
    }

    return styleClass;
  }

  /**
   * Override of BaseRenderer.renderContent() which renders
   * the contentContainer's chrome around the child contents.
   */
  protected void renderContent(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    // First, check to see if we have a header.
    Object text = getText(context, node);
    if (text != null)
      _renderHeaderRow(context, node, text);

    // We always render the content row
    _renderContentRow(context, node);
  }

  /**
   * Gets the contentContainer's header text.
   */
  protected Object getText(
    RenderingContext context,
    UINode           node
    )
  {
    // Use getLocalAttribute(), since we retrieve the text attribute
    // multiple times.
    return BaseDesktopUtils.getLocalAttribute(context, node, TEXT_ATTR);
  }

  /**
   * Gets the contentContainer's background value
   */
  protected Object getBackground(
    RenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, BACKGROUND_ATTR);
  }

  /**
   * Hook for subclasses to render child contents
   */
  protected void renderChildContent(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderContent(context, node);
  }

  // Renders the header row
  private void _renderHeaderRow(
    RenderingContext context,
    UINode           node,
    Object           text
    ) throws IOException
  {
    // We should only get here if we have header text
    assert (text != null);

    ResponseWriter writer = context.getResponseWriter();

    // Render the header text inside of its own table row
    writer.startElement(TABLE_ROW_ELEMENT, null);

    // Render the td with the .OraContentContainerHeader style class
    writer.startElement(TABLE_DATA_ELEMENT, null);
    renderStyleClassAttribute(context, AF_PANEL_BOX_HEADER_STYLE_CLASS);

    // Check to see if we have an icon.  Note: We only bother
    // rendering the icon if we actually have a header
    Object icon = getAttributeValue(context, node, ICON_ATTR, null);
    if (icon != null)
    {
      writer.startElement(IMAGE_ELEMENT, null);
      renderURIAttribute(context, SOURCE_ATTRIBUTE, icon);
      writer.writeAttribute(ALT_ATTRIBUTE, "", null);
      writer.endElement(IMAGE_ELEMENT);
    }

    writer.writeText(text, TEXT_ATTR.getAttributeName());

    writer.endElement(TABLE_DATA_ELEMENT);
    writer.endElement(TABLE_ROW_ELEMENT);
  }

  // Renders the table row which contains the
  // contentContainer's child contents.
  private void _renderContentRow(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    // Render the contents inside of its own table row
    writer.startElement(TABLE_ROW_ELEMENT, null);

    // Render the td with the .OraContentContainerContent style class
    writer.startElement(TABLE_DATA_ELEMENT, null);
    renderStyleClassAttribute(context, AF_PANEL_BOX_BODY_STYLE_CLASS);

    // Render the child contents
    renderChildContent(context, node);

    writer.endElement(TABLE_DATA_ELEMENT);
    writer.endElement(TABLE_ROW_ELEMENT);
  }

  // Default width
  private final static String _DEFAULT_WIDTH = "33%";
}
