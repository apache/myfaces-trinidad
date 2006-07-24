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
package org.apache.myfaces.adfinternal.renderkit.core.xhtml;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.PropertyKey;
import org.apache.myfaces.adf.component.core.layout.CorePanelBox;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;

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
  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _textKey = type.findKey("text");
    _iconKey = type.findKey("icon");
    _backgroundKey = type.findKey("background");
    _contentStyleKey = type.findKey("contentStyle");
  }
  
  public boolean getRendersChildren()
  {
    return true;
  }

  protected void encodeAll(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    super.encodeAll(context, arc, component, bean);
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("table", component);
    renderId(context, component);
    renderAllAttributes(context, arc, bean);
    
    Object text = bean.getProperty(_textKey);
    Object icon = bean.getProperty(_iconKey);
    if(text!=null || icon!=null)
    {
      _renderHeaderRow(context, arc, bean, text, icon);
    }
    
    _renderContentRow(context, arc, component, bean);
    
    writer.endElement("table");
  }
  
  protected void renderAllAttributes(FacesContext context, 
    AdfRenderingContext arc, 
    FacesBean bean) throws IOException
  {
    super.renderAllAttributes(context, arc, bean); 
    OutputUtils.renderLayoutTableAttributes(context, arc, "0", null);
  }
  
  protected void renderStyleAttributes(
    FacesContext        context,
    AdfRenderingContext arc,
    FacesBean           bean) throws IOException
  {
    renderStyleAttributes(context, arc, bean, _getDefaultStyleClass(bean));
  }
  
  private String _getDefaultStyleClass(FacesBean bean)
  { 
    String styleClass = null;
    Object background = bean.getProperty(_backgroundKey);
    
    // If we don't have a header, we use the
    // af|panelBox::content-<Background> style classes.
    // If we do have a header, we use the 
    // af|panelBox::transparent<Background> style classes
    
    // Check to see if we have a header
    Object text = bean.getProperty(_textKey);
    Object icon = bean.getProperty(_iconKey);

    if (text == null && icon == null)
    {
      if (_BACKGROUND_TRANSPARENT.equals(background))
        styleClass = XhtmlConstants.AF_PANEL_BOX_CONTENT_TRANSPARENT_STYLE_CLASS;
      else if ( _BACKGROUND_MEDIUM.equals(background))
        styleClass = XhtmlConstants.AF_PANEL_BOX_CONTENT_MEDIUM_STYLE_CLASS;
      else if ( _BACKGROUND_DARK.equals(background))
        styleClass = XhtmlConstants.AF_PANEL_BOX_CONTENT_DARK_STYLE_CLASS;
      else
        styleClass = XhtmlConstants.AF_PANEL_BOX_CONTENT_LIGHT_STYLE_CLASS;
    }
    else
    {
      if ( _BACKGROUND_TRANSPARENT.equals(background))
        styleClass = XhtmlConstants.AF_PANEL_BOX_TRANSPARENT_STYLE_CLASS;
      else if ( _BACKGROUND_MEDIUM.equals(background))
        styleClass = XhtmlConstants.AF_PANEL_BOX_MEDIUM_STYLE_CLASS;
      else if ( _BACKGROUND_DARK.equals(background))
        styleClass = XhtmlConstants.AF_PANEL_BOX_DARK_STYLE_CLASS;
      else
        styleClass = XhtmlConstants.AF_PANEL_BOX_LIGHT_STYLE_CLASS;
    }

    return styleClass;
  }
  
  private void _renderHeaderRow (
    FacesContext context, 
    AdfRenderingContext arc,
    FacesBean bean, 
    Object text,
    Object icon)
    throws IOException
  {
    assert(text!=null || icon!=null);
    ResponseWriter writer = context.getResponseWriter();    
    writer.startElement("tr", null);
    writer.startElement("td", null);
    renderStyleClass(context, arc, XhtmlConstants.AF_PANEL_BOX_HEADER_STYLE_CLASS);
    
    if(icon != null)
    {
      writer.startElement("img", null);
      OutputUtils.renderAltAndTooltipForImage(context, arc,  
              XhtmlConstants.EMPTY_STRING_ATTRIBUTE_VALUE);
      writer.writeURIAttribute("src", icon, _iconKey.getName());
      writer.endElement("img");
    }
    
    if(text != null)
    {
      writer.writeText(text, _textKey.getName());
    }
    writer.endElement("td");
    writer.endElement("tr");
  }
  
  // Renders the table row which contains the
  // contentContainer's child contents.
  private void _renderContentRow(
    FacesContext context, 
    AdfRenderingContext arc,
    UIComponent         component,    
    FacesBean bean
  ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    
    Object style      = bean.getProperty(_contentStyleKey);
    
    // Render the contents inside of its own table row
    writer.startElement("tr", null);

    // Render the td with the .OraContentContainerContent style class
    writer.startElement("td", null);
    renderStyleClass(context, arc, XhtmlConstants.AF_PANEL_BOX_BODY_STYLE_CLASS);      
    
    if(style != null)
    {
      writer.writeAttribute("style", style, null);
    }

    encodeAllChildren(context, component);

    writer.endElement("td");
    writer.endElement("tr");
  }


  private PropertyKey _textKey;
  private PropertyKey _iconKey;
  private PropertyKey _contentStyleKey;
  private PropertyKey _backgroundKey;
    
  private static final String _BACKGROUND_TRANSPARENT = "transparent";
  private static final String _BACKGROUND_MEDIUM      = "medium";
  private static final String _BACKGROUND_DARK        = "dark";
  
}
