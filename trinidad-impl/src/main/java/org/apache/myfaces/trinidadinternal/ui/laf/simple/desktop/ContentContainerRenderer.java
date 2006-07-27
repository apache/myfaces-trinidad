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

package org.apache.myfaces.trinidadinternal.ui.laf.simple.desktop;

import java.io.IOException;

import java.util.Map;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;


import org.apache.myfaces.trinidad.util.ArrayMap;

import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;
import org.apache.myfaces.trinidadinternal.util.IntegerUtils;

import org.apache.myfaces.trinidad.component.core.layout.CorePanelBox;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.skin.icon.Icon;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;

/**
 * ContentContainer Renderer for the desktop implementation of the
 * Simple Look And Feel.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/simple/desktop/ContentContainerRenderer.java#0 $) $Date: 10-nov-2005.18:51:21 $
 * @author The Oracle ADF Faces Team
 */
public class ContentContainerRenderer
  extends org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.ContentContainerRenderer
{
  /**
   * Override of the Base Desktop ContentContainerRenderer's
   * renderContent().
   */
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // First, get the Icon objects that we'll use to render
    // this contentContainer.
    IconData icons = _getIconData(context, node);

    // Check to see whether we have a header.  We have a
    // header if the contentContainer's text attribute is set
    Object text = getText(context, node);

    // Determine how many table columns we'll need to render the
    // contentContainer.
    int columnCount = _getColumnCount(icons, text);

    // Okay, start rendering here...  If we have a header, render it

    
    if (text != null)
      _renderHeaderRow(context, node, icons, text, columnCount);

    // Render the contents
    _renderContents(context, node, icons, text, columnCount);
  }

  /**
   * Override of the Base Desktop ContentContainerRenderer's
   * getBackground() which uses a local property to store the background
   * value.
   */
  protected Object getBackground(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // Use getLocalAttribute(), since we retrieve the background attribute
    // multiple times.
    return SimpleDesktopUtils.getLocalAttribute(context,
                                                node,
                                                BACKGROUND_ATTR);
  }

  // Get the IconData to use for rendering this contentContainer
  private IconData _getIconData(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object key = null;
    String[] names = null;

    // Pick the appropriate set of icons based on the background
    Object background = getBackground(context, node);

    if (BACKGROUND_TRANSPARENT.equals(background))
    {
      key = _TRANSPARENT_ICONS_KEY;
      names = _TRANSPARENT_ICON_NAMES;
    }
    else if (BACKGROUND_MEDIUM.equals(background))
    {
      key = _MEDIUM_ICONS_KEY;
      names = _MEDIUM_ICON_NAMES;
    }
    else if (BACKGROUND_DARK.equals(background))
    {
      key = _DARK_ICONS_KEY;
      names = _DARK_ICON_NAMES;
    }
    else
    {
      key = _LIGHT_ICONS_KEY;
      names = _LIGHT_ICON_NAMES;
    }

    // Check to see whether we have already created
    // the IconData for this background color
    // =-=jmw We don't do this optimization because we are sharing the keys
    // with shuttle. We'll have to change this optimization. @todo
    //Skin skin = context.getSkin();
    //IconData icons = (IconData)skin.getProperty(key);
    IconData icons = null;

    if (icons == null)
    {
      // If we haven't created the IconData yet, create it now
      Icon headerStart = context.getIcon(names[0]);
      Icon headerEnd = context.getIcon(names[1]);
      Icon headerBackground = context.getIcon(names[2]);
      Icon bottomStart = context.getIcon(names[3]);
      Icon bottomEnd = context.getIcon(names[4]);
      Icon bottomBackground = context.getIcon(names[5]);
      Icon topStart = context.getIcon(names[6]);
      Icon topEnd = context.getIcon(names[7]);
      Icon topBackground = context.getIcon(names[8]);
      Icon startBackground = context.getIcon(names[9]);
      Icon endBackground = context.getIcon(names[10]);

      icons = new IconData(headerStart,
                           headerEnd,
                           headerBackground,
                           bottomStart,
                           bottomEnd,
                           bottomBackground,
                           topStart,
                           topEnd,
                           topBackground,
                           startBackground,
                           endBackground);


      // Stash away the IconData so that we don't have to re-create
      // it on the next render
      // comment out because it does not work now that we share keys with
      // shuttle's panelBox.
    //skin.setProperty(key, icons);
    }

    return icons;
  }

  // Gets the number of columns that this contentContainer
  // renders
  private int _getColumnCount(
    IconData         icons,
    Object           text
    )
  {
    int columnCount = 1;

    if (((text != null) && (icons.headerStart != null)) ||
        (icons.bottomStart != null)                     ||
        (icons.topStart != null)                        ||
        (icons.startBackground != null))
      columnCount++;

    if (((text != null) && (icons.headerEnd != null)) ||
        (icons.bottomEnd != null)                     ||
        (icons.bottomEnd != null)                     ||
        (icons.endBackground != null))
      columnCount++;

    return columnCount;
  }

  // Renders the header row
  private void _renderHeaderRow(
    UIXRenderingContext context,
    UINode           node,
    IconData         icons,
    Object           text,
    int              columnCount
    ) throws IOException
  {
    // We should only get here if we have header text
    assert (text != null);
    
    FacesContext fContext = context.getFacesContext();
    RenderingContext arc = RenderingContext.getCurrentInstance();
    ResponseWriter writer = fContext.getResponseWriter();



    // Render the header text inside of its own table row
    writer.startElement(TABLE_ROW_ELEMENT, null);

    // If we've got a start icon, render it
    if (icons.headerStart != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);

      // Note, we want to stretch the header start/end icons vertically
      // so that there won't be any gaps above/below these icons if the
      // user zooms up the text size.  Seems like stretched icons do
      // not show up in IE unless the containing table cell has some
      // height specified.
      _writeHeaderIconCellHeight(context, icons.headerStart);

      // Map overrides the icon's normal height to be "100%".
      icons.headerStart.renderIcon(fContext, arc, 
                                   _getStretchedIconAttrs(context));

      writer.endElement(TABLE_DATA_ELEMENT);
    }

    // Render the td with the AF_PANEL_BOX_HEADER_STYLE_CLASS style class
    writer.startElement(TABLE_DATA_ELEMENT, null);
    renderStyleClassAttribute(context, AF_PANEL_BOX_HEADER_STYLE_CLASS);
    org.apache.myfaces.trinidadinternal.renderkit.core.skin.CoreSkinUtils.__renderBackgroundIcon(fContext, arc, icons.headerBackground);

    // Write colspan for header text.  The header text cell takes up
    // space for any empty icon columns.
    Integer textColumnCount = _getHeaderTextColumnCount(icons, columnCount);
    writer.writeAttribute(COLSPAN_ATTRIBUTE, textColumnCount, null);

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

    writer.writeText(text, CorePanelBox.TEXT_KEY.getName());

    writer.endElement(TABLE_DATA_ELEMENT);

    // If we've got an end icon, render it
    if (icons.headerEnd != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      _writeHeaderIconCellHeight(context, icons.headerEnd);
      icons.headerEnd.renderIcon(fContext, arc, 
                                 _getStretchedIconAttrs(context));
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    writer.endElement(TABLE_ROW_ELEMENT);
  }

  // Writes out the height attribute for header table cells which
  // contain the header start/end icons.
  private void _writeHeaderIconCellHeight(
    UIXRenderingContext context,
    Icon             icon
    ) throws IOException
  {
    // Only write the height for platforms that require it
    if (_requiresStretchedImageHeight(context))
    {
      // Use the Icon's height if it is known.  If the height isn't,
      // known, we need to specify some height for IE - otherwise the
      // image does not show up!
      RenderingContext arc = RenderingContext.getCurrentInstance();
      Object height = icon.getImageHeight(arc);

      if (height == null)
        height = _DEFAULT_STRETCHED_IMAGE_HEIGHT;

      context.getResponseWriter().writeAttribute(HEIGHT_ATTRIBUTE, height, null);
    }
  }

  private void _renderContents(
    UIXRenderingContext context,
    UINode           node,
    IconData         icons,
    Object           text,
    int              columnCount
    ) throws IOException
  {
    RenderingContext arc = RenderingContext.getCurrentInstance();
    FacesContext fContext = context.getFacesContext();
    // Render the top row if we have the necessary icons
    if (_hasTopRow(icons, text))
      _renderTopRow(fContext, arc, icons, columnCount);

    // Render the contents inside of its own table row
    _renderContentsRow(context, fContext, arc, node, icons, columnCount);

    // Render the bottom row if we have the necessary icons
    if (_hasBottomRow(icons))
      _renderBottomRow(fContext, arc, icons, columnCount);
  }

  // Render the table row with the contents
  private void _renderContentsRow(
    UIXRenderingContext context,
    FacesContext     fContext,
    RenderingContext arc,
    UINode           node,
    IconData         icons,
    int              columnCount
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement(TABLE_ROW_ELEMENT, null);

    if (icons.startBackground != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      org.apache.myfaces.trinidadinternal.renderkit.core.skin.CoreSkinUtils.__renderBackgroundIcon(fContext, arc, icons.startBackground);
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    // Render the td with the .OraContentContainerContent style class
    writer.startElement(TABLE_DATA_ELEMENT, null);
    renderStyleClassAttribute(context, AF_PANEL_BOX_BODY_STYLE_CLASS);

    // Write colspan for the body cell.  The body cell takes up
    // space for any empty icon columns.
    Integer bodyColumnCount = _getBodyColumnCount(icons, columnCount);
    writer.writeAttribute(COLSPAN_ATTRIBUTE, bodyColumnCount, null);

    // Render the child contents
    renderChildContent(context, node);

    writer.endElement(TABLE_DATA_ELEMENT);

    if (icons.endBackground != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      org.apache.myfaces.trinidadinternal.renderkit.core.skin.CoreSkinUtils.__renderBackgroundIcon(fContext, arc, icons.endBackground);
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    writer.endElement(TABLE_ROW_ELEMENT);
  }
  // Tests whether this contentContainer has a bottom row
  private boolean _hasBottomRow(IconData icons)
  {
    // We have a bottom row if we have a bottom start/end icon
    return ((icons.bottomStart != null) || (icons.bottomEnd != null));
  }

  // Renders the bottom row
  private void _renderBottomRow(
    FacesContext     fContext,
    RenderingContext arc,
    IconData         icons,
    int              columnCount
    ) throws IOException
  {
    ResponseWriter writer = fContext.getResponseWriter();

    // Render the contents inside of its own table row
    writer.startElement(TABLE_ROW_ELEMENT, null);

    // If we've got a start icon, render it
    if (icons.bottomStart != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      OutputUtils.renderIcon(fContext, arc, icons.bottomStart, "", null);     
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    // Render the cell with the bottom background icon.  We first
    // need to determine how many columns the background cell should
    // fill.
    Integer colspan = _getBottomBackgroundColumnCount(icons, columnCount);

    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(COLSPAN_ATTRIBUTE, colspan, null);
    writer.writeAttribute(WIDTH_ATTRIBUTE, "100%", null);
    org.apache.myfaces.trinidadinternal.renderkit.core.skin.CoreSkinUtils.__renderBackgroundIcon(fContext, arc, icons.bottomBackground);
    writer.endElement(TABLE_DATA_ELEMENT);

    // If we've got an end icon, render it
    if (icons.bottomEnd != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      OutputUtils.renderIcon(fContext, arc, icons.bottomEnd, "", null);     
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    writer.endElement(TABLE_ROW_ELEMENT);
  }

  // Tests whether this contentContainer has a top row
  private boolean _hasTopRow(IconData icons, Object text)
  {
    // We have a top row if we have a top start/end icon and
    // there is no header text
    return ((text == null) &&
             ((icons.topStart != null) || (icons.topEnd != null)));
  }

  // Renders the top row
  private void _renderTopRow(
    FacesContext     fContext,
    RenderingContext arc,
    IconData         icons,
    int              columnCount
    ) throws IOException
  {
    ResponseWriter writer = fContext.getResponseWriter();

    // Render the contents inside of its own table row
    writer.startElement(TABLE_ROW_ELEMENT, null);

    // If we've got a start icon, render it
    if (icons.topStart != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      OutputUtils.renderIcon(fContext, arc, icons.topStart, "", null);     
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    // Render the cell with the top background icon.  We first
    // need to determine how many columns the background cell should
    // fill.
    Integer colspan = _getTopBackgroundColumnCount(icons, columnCount);

    writer.startElement(TABLE_DATA_ELEMENT, null);
    writer.writeAttribute(COLSPAN_ATTRIBUTE, colspan, null);
    writer.writeAttribute(WIDTH_ATTRIBUTE, "100%", null);
    org.apache.myfaces.trinidadinternal.renderkit.core.skin.CoreSkinUtils.__renderBackgroundIcon(fContext, arc, icons.topBackground);
    writer.endElement(TABLE_DATA_ELEMENT);

    // If we've got an end icon, render it
    if (icons.topEnd != null)
    {
      writer.startElement(TABLE_DATA_ELEMENT, null);
      OutputUtils.renderIcon(fContext, arc, icons.topEnd, "", null);     
      writer.endElement(TABLE_DATA_ELEMENT);
    }

    writer.endElement(TABLE_ROW_ELEMENT);
  }

  // Returns the number of columns that the header text
  // should occupy
  private static Integer _getHeaderTextColumnCount(
    IconData icons,
    int      columnCount
    )
  {
    // The header text takes up the full width of the table,
    // unless we have start/end icons.
    int textColumnCount = columnCount;

    // If the header has a start icon, leave room for it
    if (icons.headerStart != null)
     textColumnCount--;

    // If the header has an end icon, leave room for it
    if (icons.headerEnd != null)
      textColumnCount--;

    if (textColumnCount == 1)
      return null;

    return IntegerUtils.getInteger(textColumnCount);
  }

  // Returns the number of columns that the body cell should occupy
  private static Integer _getBodyColumnCount(
    IconData icons,
    int      columnCount
    )
  {
    int bodyColumnCount = columnCount;

    // If we have a start background icon, leave room for it
    if (icons.startBackground != null)
      bodyColumnCount--;

    // If we have an end background icon, leave room for it
    if (icons.endBackground != null)
     bodyColumnCount--;

    if (bodyColumnCount == 1)
      return null;

    return IntegerUtils.getInteger(bodyColumnCount);
  }

  // Returns the number of columns for the bottom background cell
  private static Integer _getBottomBackgroundColumnCount(
    IconData icons,
    int      columnCount
    )
  {
    int backgroundColumnCount = columnCount;

    if (icons.bottomStart != null)
      backgroundColumnCount--;

    if (icons.bottomEnd != null)
      backgroundColumnCount--;

    if (backgroundColumnCount == 1)
      return null;

    return IntegerUtils.getInteger(backgroundColumnCount);
  }

  // Returns the number of columns for the top background cell
  private static Integer _getTopBackgroundColumnCount(
    IconData icons,
    int      columnCount
    )
  {
    int backgroundColumnCount = columnCount;

    if (icons.topStart != null)
      backgroundColumnCount--;

    if (icons.topEnd != null)
      backgroundColumnCount--;

    if (backgroundColumnCount == 1)
      return null;

    return IntegerUtils.getInteger(backgroundColumnCount);
  }

  // Tests whether the browser requires a height to be specified
  // in order for a stretched image to show up
  private static boolean _requiresStretchedImageHeight(
    UIXRenderingContext context
    )
  {
    // IE does not display stretched images unless the containing table
    // cell has some height specified.  We might want to make this
    // an Agent instead of checking the Agent info directly.
    return isIE(context);
  }

  // Gets the Map for stetched icons, and sets alt attribute to "".
  private static Map _getStretchedIconAttrs(
    UIXRenderingContext context
    )
  {
    TrinidadAgent agent = context.getAgent();
    Object capImageStretch = agent.getCapability(TrinidadAgent.CAP_IMAGE_STRETCH);

    // If the Agent supports image stretching, return the
    // Map with height=100%.  Otherwise we don't
    // have any attrs.
    Map attrs = new ArrayMap(2);
    if (Boolean.TRUE.equals(capImageStretch))
    {
      attrs.put(Icon.HEIGHT_KEY, "100%");
    }
    else
    {
      attrs.put(Icon.HEIGHT_KEY, null);
    }
    attrs.put(Icon.SHORT_DESC_KEY, "");
    return attrs;

  }

  // A class that we use for storing Icon-related info
  private static class IconData
  {
    public final Icon headerStart;
    public final Icon headerEnd;
    public final Icon headerBackground;
    public final Icon bottomStart;
    public final Icon bottomEnd;
    public final Icon bottomBackground;
    public final Icon topStart;
    public final Icon topEnd;
    public final Icon topBackground;
    public final Icon startBackground;
    public final Icon endBackground;

    public IconData(
      Icon headerStart,
      Icon headerEnd,
      Icon headerBackground,
      Icon bottomStart,
      Icon bottomEnd,
      Icon bottomBackground,
      Icon topStart,
      Icon topEnd,
      Icon topBackground,
      Icon startBackground,
      Icon endBackground
      )
    {
      this.headerStart = headerStart;
      this.headerEnd = headerEnd;
      this.headerBackground = headerBackground;
      this.bottomStart = bottomStart;
      this.bottomEnd = bottomEnd;
      this.bottomBackground = bottomBackground;
      this.topStart = topStart;
      this.topEnd = topEnd;
      this.topBackground = topBackground;
      this.startBackground = startBackground;
      this.endBackground = endBackground;
    }
  }

  // Keys for looking up IconData properties on the Skin
  private static final Object _DARK_ICONS_KEY = new Object();
  private static final Object _MEDIUM_ICONS_KEY = new Object();
  private static final Object _LIGHT_ICONS_KEY = new Object();
  private static final Object _TRANSPARENT_ICONS_KEY = new Object();

  // Icon names
  private static final String[] _DARK_ICON_NAMES =
  {
    "af|panelBox::dark-header-start-icon",
    "af|panelBox::dark-header-end-icon",
    "af|panelBox::dark-header-background-icon",
    "af|panelBox::dark-bottom-start-icon",
    "af|panelBox::dark-bottom-end-icon",
    "af|panelBox::dark-bottom-background-icon",
    "af|panelBox::dark-top-start-icon",
    "af|panelBox::dark-top-end-icon",
    "af|panelBox::dark-top-background-icon",
    "af|panelBox::dark-start-background-icon",
    "af|panelBox::dark-end-background-icon"
  };

  private static final String[] _MEDIUM_ICON_NAMES =
  {
    "af|panelBox::medium-header-start-icon",
    "af|panelBox::medium-header-end-icon",
    "af|panelBox::medium-header-background-icon",
    "af|panelBox::medium-bottom-start-icon",
    "af|panelBox::medium-bottom-end-icon",
    "af|panelBox::medium-bottom-background-icon",
    "af|panelBox::medium-top-start-icon",
    "af|panelBox::medium-top-end-icon",
    "af|panelBox::medium-top-background-icon",
    "af|panelBox::medium-start-background-icon",
    "af|panelBox::medium-end-background-icon"
  };

  private static final String[] _LIGHT_ICON_NAMES =
  {
    "af|panelBox::light-header-start-icon",
    "af|panelBox::light-header-end-icon",
    "af|panelBox::light-header-background-icon",
    "af|panelBox::light-bottom-start-icon",
    "af|panelBox::light-bottom-end-icon",
    "af|panelBox::light-bottom-background-icon",
    "af|panelBox::light-top-start-icon",
    "af|panelBox::light-top-end-icon",
    "af|panelBox::light-top-background-icon",
    "af|panelBox::light-start-background-icon",
    "af|panelBox::light-end-background-icon"
  };

  private static final String[] _TRANSPARENT_ICON_NAMES =
  {
    "af|panelBox::transparent-header-start-icon",
    "af|panelBox::transparent-header-end-icon",
    "af|panelBox::transparent-header-background-icon",
    "af|panelBox::transparent-bottom-start-icon",
    "af|panelBox::transparent-bottom-end-icon",
    "af|panelBox::transparent-bottom-background-icon",
    "af|panelBox::transparent-top-start-icon",
    "af|panelBox::transparent-top-end-icon",
    "af|panelBox::transparent-top-background-icon",
    "af|panelBox::transparent-start-background-icon",
    "af|panelBox::transparent-end-background-icon"
  };

  // Default height that we use for stretched header start/end
  // icons.  If we don't specify some height, stretched images
  // won't show up on IE.
  private static final Integer _DEFAULT_STRETCHED_IMAGE_HEIGHT =
    IntegerUtils.getInteger(1);
}
