/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelBox;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.XhtmlConstants;

public class PanelBoxRenderer
  extends XhtmlRenderer
{
  public PanelBoxRenderer()
  {
    this(CorePanelBox.TYPE);
  }

  protected PanelBoxRenderer(FacesBean.Type type)
  {
    super(type);
  }
  
  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _textKey = type.findKey("text");
    _iconKey = type.findKey("icon");
    _backgroundKey = type.findKey("background");
    _contentStyleKey = type.findKey("contentStyle");
  }
  
  @Override
  public String getDefaultStyleClass(FacesBean bean)
  {
    String background = getBackground(bean);
    if(_BACKGROUND_TRANSPARENT.equals(background))
    {
      return SkinSelectors.AF_PANEL_BOX_TRANSPARENT_STYLE_CLASS;
    }
    else if(_BACKGROUND_LIGHT.equals(background))
    {
      return SkinSelectors.AF_PANEL_BOX_LIGHT_STYLE_CLASS;
    }
    else if( _BACKGROUND_MEDIUM.equals(background))
    {
      return SkinSelectors.AF_PANEL_BOX_MEDIUM_STYLE_CLASS;
    }
    else if( _BACKGROUND_DARK.equals(background))
    {
      return SkinSelectors.AF_PANEL_BOX_DARK_STYLE_CLASS;
    }
    else
    {
      return _BACKGROUND_DEFAULT_STYLE_CLASS;
    }
  }
  
  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  protected boolean hasChildren(UIComponent component)
  {
    return component.getChildCount() > 0;
  }

  @Override
  protected void encodeAll(
      FacesContext     context,
      RenderingContext arc,
      UIComponent      component,
      FacesBean        bean) throws IOException
  {
    super.encodeAll(context, arc, component, bean);
    
    String icon = getIcon(bean);
    String text = getText(bean);
    
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(XhtmlConstants.TABLE_ELEMENT, component); // The frame table
    renderId(context, component);
    renderAllAttributes(context, arc, bean);
    writer.startElement(XhtmlConstants.TABLE_BODY_ELEMENT, null);
    
    if (hasChildren(component) || text != null || icon != null)
    {
      // There's something to render to let build the frame
      _renderContainerTopRow(context, arc);
      
      _renderMiddleRow(context, arc, component, bean, icon, text);
      
      _renderContainerBottomRow(context, arc);
    }

    writer.endElement(XhtmlConstants.TABLE_BODY_ELEMENT);
    writer.endElement(XhtmlConstants.TABLE_ELEMENT);
  }
  
  @Override
  protected void renderAllAttributes(
      FacesContext context, 
      RenderingContext arc, 
      FacesBean bean) throws IOException
  {
    super.renderAllAttributes(context, arc, bean); 
    OutputUtils.renderLayoutTableAttributes(context, arc, "0", null);
  }
    
  
  private void _renderContainerTopRow(
      FacesContext     context,
      RenderingContext arc) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
    if(arc.isRightToLeft())
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_TOP_END_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_TOP_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_TOP_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    else
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_TOP_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_TOP_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_TOP_END_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    
    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
  }
  
  private void _renderContainerBottomRow(
      FacesContext     context,
      RenderingContext arc) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
    if(arc.isRightToLeft())
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_END_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    else
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_BOTTOM_END_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    
    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
  }
  
  private void _renderMiddleRow(
      FacesContext      context,
      RenderingContext  arc,
      UIComponent       component,
      FacesBean         bean,
      Object            icon,
      Object            text) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
    
    // Render left edge
    if(arc.isRightToLeft())
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_END_STYLE_CLASS);
      
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    else
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    
    // Render body
    writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
    renderBody(context, arc, component, bean, icon, text);
    writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);

    // Render right edge
    if(arc.isRightToLeft())
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_START_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    else
    {
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderStyleClass(context, 
                       arc, 
                       SkinSelectors.AF_PANEL_BOX_END_STYLE_CLASS);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
    }
    
    writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
  }
  
  protected void renderBody(
      FacesContext      context,
      RenderingContext  arc,
      UIComponent       component,
      FacesBean         bean,
      Object            icon,
      Object            text) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    
    renderStyleClass(context, 
                     arc, 
                     SkinSelectors.AF_PANEL_BOX_BODY_STYLE_CLASS);
    
    if (hasChildren(component) && (text != null || icon != null))
    {
      // There's both a header and a content, use a table.
      writer.startElement(XhtmlConstants.TABLE_ELEMENT, null);
      OutputUtils.renderLayoutTableAttributes(context, arc, "0", "100%");
      writer.startElement(XhtmlConstants.TABLE_BODY_ELEMENT, null);
      
      // Render header
      writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      _renderHeader(context, arc, icon, text);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
      
      // Render content
      writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      renderContent(context, arc, bean, component);
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
      
      writer.endElement(XhtmlConstants.TABLE_BODY_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_ELEMENT);
    }
    else if(text != null || icon != null)
    {
      // We only have a header, use a div as style class placeholder
      writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
      _renderHeader(context, arc, icon, text);
      writer.endElement(XhtmlConstants.DIV_ELEMENT);
    }
    else
    {
      // We only have a content, use a div as style class placeholder
      writer.startElement(XhtmlConstants.DIV_ELEMENT, null);
      renderContent(context, arc, bean, component);
      writer.endElement(XhtmlConstants.DIV_ELEMENT);
    }
  }
  
  private void _renderHeader(
      FacesContext     context,
      RenderingContext arc,
      Object           icon,
      Object           text) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    
    renderStyleClass(context, 
                     arc, 
                     SkinSelectors.AF_PANEL_BOX_HEADER_STYLE_CLASS);
    
    if(arc.isRightToLeft())
    {
      if(text != null)
      {
        writer.writeText(text, _textKey.getName());
      }
      
      if(icon != null)
      {
        writer.startElement("img", null);
        OutputUtils.renderAltAndTooltipForImage(context, 
                                                arc,  
                                                XhtmlConstants.EMPTY_STRING_ATTRIBUTE_VALUE);
        
        renderEncodedResourceURI(context, "src", icon);
        writer.endElement("img");
      }
    }
    else
    {
      if(icon != null)
      {
        writer.startElement("img", null);
        OutputUtils.renderAltAndTooltipForImage(context, 
                                                arc,  
                                                XhtmlConstants.EMPTY_STRING_ATTRIBUTE_VALUE);
        
        renderEncodedResourceURI(context, "src", icon);
        writer.endElement("img");
      }
      
      if(text != null)
      {
        writer.writeText(text, _textKey.getName());
      }
    }
  }
  
  protected void renderContent(
      FacesContext      context,
      RenderingContext  arc,
      FacesBean         bean,
      UIComponent       component) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    
    renderStyleClass(context, 
                     arc, 
                     SkinSelectors.AF_PANEL_BOX_CONTENT_STYLE_CLASS);
        
    String style = getContentStyle(bean);
    if(style != null)
    {
      writer.writeAttribute("style", style, null);
    }

    encodeAllChildren(context, component);
  }

  protected String getText(FacesBean bean)
  {
    if (_textKey == null)
      return null;
    return toString(bean.getProperty(_textKey));
  }

  protected String getIcon(FacesBean bean)
  {
    if (_iconKey == null)
      return null;
    return toResourceUri(FacesContext.getCurrentInstance(), bean.getProperty(_iconKey));
  }

  protected String getContentStyle(FacesBean bean)
  {
    if (_contentStyleKey == null)
      return null;
    return toString(bean.getProperty(_contentStyleKey));
  }

  protected String getBackground(FacesBean bean)
  {
    if (_backgroundKey == null)
      return null;
    return toString(bean.getProperty(_backgroundKey));
  }
  
  private PropertyKey _textKey;
  private PropertyKey _iconKey;
  private PropertyKey _contentStyleKey;
  private PropertyKey _backgroundKey;
  
  private static final String _BACKGROUND_LIGHT       = "light";
  private static final String _BACKGROUND_TRANSPARENT = "transparent";
  private static final String _BACKGROUND_MEDIUM      = "medium";
  private static final String _BACKGROUND_DARK        = "dark";
  
  private static final String _BACKGROUND_DEFAULT_STYLE_CLASS = 
    SkinSelectors.AF_PANEL_BOX_LIGHT_STYLE_CLASS;
}
