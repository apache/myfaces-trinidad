/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelBox;
import org.apache.myfaces.trinidad.context.RenderingContext;

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
  
  public String getRootStyleClass(FacesBean bean)
  {
    Object background = bean.getProperty(_backgroundKey);
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

  @Override
  protected void encodeAll(
      FacesContext     context,
      RenderingContext arc,
      UIComponent      component,
      FacesBean        bean) throws IOException
  {
    super.encodeAll(context, arc, component, bean);
    
    List<UIComponent> children = _getRenderedChildren(component);
    Object            icon     = bean.getProperty(_iconKey);
    Object            text     = bean.getProperty(_textKey);
    
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement(XhtmlConstants.TABLE_ELEMENT, component); // The frame table
    renderId(context, component);
    renderAllAttributes(context, arc, bean);
    writer.startElement(XhtmlConstants.TABLE_BODY_ELEMENT, null);
    
    if(!children.isEmpty() || text != null || icon != null)
    {
      // There's something to render to let build the frame
      _renderContainerTopRow(context, arc);
      
      _renderMiddleRow(context, arc, component, bean, icon, text, children);
      
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
  
  @Override
  protected void renderStyleAttributes(
      FacesContext     context,
      RenderingContext arc,
      FacesBean        bean) throws IOException
  {
    renderStyleAttributes(context, arc, bean, getRootStyleClass(bean));
  }
  
  @SuppressWarnings("unchecked")
  private List<UIComponent> _getRenderedChildren(UIComponent component)
  {
    int childCount = component.getChildCount();
    if(childCount == 0)
    {
      return Collections.emptyList();
    }
    
    List<UIComponent> result   = new ArrayList<UIComponent>(childCount);
    List<UIComponent> children = component.getChildren();
    for(UIComponent child : children)
    {
      if(child.isRendered())
      {
        result.add(child);
      }
    }
    
    return result;
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
      Object            text,
      List<UIComponent> children) throws IOException
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
    _renderBody(context, arc, component, bean, icon, text, children);
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
  
  private void _renderBody(
      FacesContext      context,
      RenderingContext  arc,
      UIComponent       component,
      FacesBean         bean,
      Object            icon,
      Object            text,
      List<UIComponent> children) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    
    renderStyleClass(context, 
                     arc, 
                     SkinSelectors.AF_PANEL_BOX_BODY_STYLE_CLASS);
    
    if(!children.isEmpty() && (text != null || icon != null))
    {
      // There's both a header and a content, use a table.
      writer.startElement(XhtmlConstants.TABLE_ELEMENT, null);
      OutputUtils.renderLayoutTableAttributes(context, arc, "0", null);
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
      _renderContent(context, arc, bean, children);
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
      _renderContent(context, arc, bean, children);
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
        
        writer.writeURIAttribute("src", icon, _iconKey.getName());
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
        
        writer.writeURIAttribute("src", icon, _iconKey.getName());
        writer.endElement("img");
      }
      
      if(text != null)
      {
        writer.writeText(text, _textKey.getName());
      }
    }
  }
  
  private void _renderContent(
      FacesContext      context,
      RenderingContext  arc,
      FacesBean         bean,
      List<UIComponent> children) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    
    renderStyleClass(context, 
                     arc, 
                     SkinSelectors.AF_PANEL_BOX_CONTENT_STYLE_CLASS);
        
    Object style = bean.getProperty(_contentStyleKey);
    if(style != null)
    {
      writer.writeAttribute("style", style, null);
    }

    for(UIComponent child : children)
    {
      encodeChild(context, child);
    }
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
