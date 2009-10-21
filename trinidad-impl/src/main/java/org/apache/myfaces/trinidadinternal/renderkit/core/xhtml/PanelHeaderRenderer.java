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
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelHeader;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.XhtmlConstants;
import org.apache.myfaces.trinidad.skin.Icon;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/HeaderRenderer.java#0 $) $Date: 10-nov-2005.18:55:17 $
 */
public class PanelHeaderRenderer extends XhtmlRenderer
{
  public PanelHeaderRenderer()
  {
    this(CorePanelHeader.TYPE);
  }

  protected PanelHeaderRenderer(FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _textKey = type.findKey("text");
    _iconKey = type.findKey("icon");
    _sizeKey = type.findKey("size");
    _messageTypeKey = type.findKey("messageType");
  }
  
  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    Number oldSize = getContextHeaderSize(arc, null);

    int nestLevel = getHeaderNestLevel(arc);
    int size = _getAndStoreSize(arc, bean);

    String messageType = getMessageType(bean);
    String text = getText(arc, bean, messageType);

    List<String> headerElementList = XhtmlConstants.HEADER_ELEMENTS;
    String headerElement = headerElementList.get(
                                 Math.min(size, headerElementList.size() - 1));
    
    ResponseWriter writer = context.getResponseWriter();    
    writer.startElement("div", component);  

    renderId(context, component);
    renderAllAttributes(context, arc, bean);


    boolean nesting = ((nestLevel > 0) &&
        !Boolean.FALSE.equals(arc.getSkin().getProperty(
              SkinProperties.AF_PANELHEADER_INDENT_CONTENT)));
    // =-=AEW What if style class is already set?
    if (nesting)
      renderStyleClass(context, arc, SkinSelectors.HEADER_NEST_STYLE_CLASS);

    writer.startElement(headerElement, null);
    renderStyleClass(context, arc, SkinSelectors.AF_PANEL_HEADER_STYLE_CLASS);
    
    renderIcon(context, arc, component, bean, messageType);

    boolean isError = TrinidadRenderingConstants.MESSAGE_TYPE_ERROR.equals(messageType);

    if (isError)
    {
      // If this is an error header, render the text within
      // a SkinSelectors.AF_PANEL_HEADER_ERROR_STYLE_CLASS span so that header will pick up 
      // the error text foreground color
      writer.startElement("span", null);
      renderStyleClass(context, arc,
                       SkinSelectors.AF_PANEL_HEADER_ERROR_STYLE_CLASS);
    }
    
    if (text != null)
      writer.writeText(text, "text");

    if (isError)
    {
      // Close up the span
      writer.endElement("span");
    }

    // Close up the header
    writer.endElement(headerElement);
    
    // increment header nesting
    incrementHeaderNestLevel(arc);

    if (shouldRenderChildren(bean))
      encodeAllChildren(context, component);

    decrementHeaderNestLevel(arc);

    writer.endElement("div");  

    // using float to indent in IE on windows, but that means you 
    // need to clear after the header or you get strange behavior
    if ( nesting &&
         isIE(arc)) // no need to check "Windows" - IE Mac is desupported
    {
      writer.startElement("div", component);
      writer.writeAttribute("style","clear:both", null);
      writer.endElement("div");
    }

    setContextHeaderSize(arc, oldSize);
  }

  // Renders the header's icon.  If the header has a message type,
  // then the icon is retrieved from the Skin.  Otherwise,
  // we use the icon specified via the header's ICON_ATTR.
  protected void renderIcon(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean,
    String              messageType) throws IOException
  {   
    if ((messageType != null) &&
        !TrinidadRenderingConstants.MESSAGE_TYPE_NONE.equals(messageType))
    {
      // If we've got a message type, get the Icon
      String iconName = getMessageIconName(messageType);
      if (iconName != null)
      {
        Icon icon = arc.getIcon(iconName);
        // If we've got an Icon, render it
        if (icon != null)
        {
          OutputUtils.renderIcon(context,
                                 arc,
                                 icon,
                                 XhtmlConstants.EMPTY_STRING_ATTRIBUTE_VALUE,
                                 null);

        }
      }

    }
    else
    {
      String iconUri = getIconUri(bean);
      if( iconUri != null)
      {
        ResponseWriter writer = context.getResponseWriter();

        writer.startElement("img", null);
        OutputUtils.renderAltAndTooltipForImage(context, arc, 
                           XhtmlConstants.EMPTY_STRING_ATTRIBUTE_VALUE);
        renderStyleClass(context, arc,
                         SkinSelectors.AF_PANEL_HEADER_ICON_STYLE_CLASS);
        renderEncodedResourceURI(context, "src", iconUri);
        writer.endElement("img");
      }
    }
  }

  /**
  * Returns text of header
  */
  protected String getText(
    RenderingContext arc,
    FacesBean           bean,
    String              messageType
    )
  {
    String text = toString(bean.getProperty(_textKey));
    if (text != null)
      return text;

    if ( messageType == null )
      return null;

    String key = null;

    if (TrinidadRenderingConstants.MESSAGE_TYPE_ERROR.equals(messageType))
      key = _ERROR_KEY;
    else if (TrinidadRenderingConstants.MESSAGE_TYPE_WARNING.equals(messageType))
      key = _WARNING_KEY;
    else if (TrinidadRenderingConstants.MESSAGE_TYPE_INFO.equals(messageType))
      key = _INFORMATION_KEY;
    else if (TrinidadRenderingConstants.MESSAGE_TYPE_CONFIRMATION.equals(messageType))
      key = _CONFIRMATION_KEY;
    else if (TrinidadRenderingConstants.MESSAGE_TYPE_PROCESSING.equals(messageType))
      key = _PROCESSING_KEY;
    else
      return null;

    return arc.getTranslatedString(key);
  }

  /**
  * Returns the uri for icon.
  */
  protected String getIconUri(
    FacesBean           bean)
  {
    if (_iconKey == null)
      return null;
    return toResourceUri(FacesContext.getCurrentInstance(), bean.getProperty(_iconKey));
  }

  protected Number getSize(FacesBean bean)
  {
    if (_sizeKey == null)
      return null;
    return (Number) bean.getProperty(_sizeKey);
  }


  protected String getMessageType(FacesBean bean)
  {
    if (_messageTypeKey == null)
      return null;
    return toString(bean.getProperty(_messageTypeKey));
  }

  protected boolean shouldRenderChildren(FacesBean bean)
  {
    return true;
  }


  protected static Number getContextHeaderSize(
    RenderingContext arc,
    Number           defaultValue
    )
  {
    Number number = (Number) arc.getProperties().get(HEADER_SIZE);
    if (number == null)
      number = defaultValue;
    
    return number;
  }

   /**
   * Set the size of the header stored on the context
   */
  protected static void setContextHeaderSize(
    RenderingContext arc,
    Number           size
    )
  {
    arc.getProperties().put(HEADER_SIZE, size);
  }



   /**
   * Returns the current depth of the nesting.
   */
  protected static int getHeaderNestLevel(
    RenderingContext arc
    )
  {
    Number n = (Number) arc.getProperties().get(HEADER_NEST_LEVEL);
    if (n == null)
      return 0;
    return n.intValue();
  }


  protected static void incrementHeaderNestLevel(
    RenderingContext arc
    )throws IOException
  {
    arc.getProperties().put(HEADER_NEST_LEVEL,
                            Integer.valueOf(getHeaderNestLevel(arc) + 1));
  }

  protected static void decrementHeaderNestLevel(
    RenderingContext arc
    )throws IOException
  {
    int headerNestLevel = getHeaderNestLevel(arc);

    assert (headerNestLevel > 0):"cannot decrement header nest level";
    arc.getProperties().put(HEADER_NEST_LEVEL,
                            Integer.valueOf(headerNestLevel - 1));
  }

  /**
   * This method compares the messageType to a number of
   * possible message types (e.g. SkinSelectors.MESSAGE_TYPE_ERROR)
   * and returns an icon name for the appropriate type.
   * This method should be overridden to change the standard
   * icons used for this component.
   * @param messageType
   * @return The icon name for the specfied messageType.
   */
  protected String getMessageIconName(String messageType)
  {
    String iconName = null;

    if (TrinidadRenderingConstants.MESSAGE_TYPE_ERROR.equals(messageType))
      iconName = SkinSelectors.AF_PANEL_HEADER_ERROR_ICON_NAME;
    else if (TrinidadRenderingConstants.MESSAGE_TYPE_WARNING.equals(messageType))
      iconName = SkinSelectors.AF_PANEL_HEADER_WARNING_ICON_NAME;
    else if (TrinidadRenderingConstants.MESSAGE_TYPE_INFO.equals(messageType))
      iconName = SkinSelectors.AF_PANEL_HEADER_INFO_ICON_NAME;
    else if (TrinidadRenderingConstants.MESSAGE_TYPE_CONFIRMATION.equals(messageType))
      iconName = SkinSelectors.AF_PANEL_HEADER_CONFIRMATION_ICON_NAME;
    else if (TrinidadRenderingConstants.MESSAGE_TYPE_PROCESSING.equals(messageType))
      iconName = SkinSelectors.AF_PANEL_HEADER_PROCESSING_ICON_NAME;

    assert ((iconName != null) ||
        TrinidadRenderingConstants.MESSAGE_TYPE_NONE.equals(messageType));

    return iconName;
  }


  private int _getAndStoreSize(
    RenderingContext arc,
    FacesBean           bean)
  {
    Number sizeNumber = getSize(bean);
    
    if (sizeNumber != null && sizeNumber.intValue() >= 0)
    {
      setContextHeaderSize(arc, sizeNumber);
      return sizeNumber.intValue();
    }    
    else
    {
      sizeNumber = getContextHeaderSize(arc, null);

      int size;
      if (sizeNumber == null)
        size = 0;
      else
        size = sizeNumber.intValue() + 1;
        
      setContextHeaderSize(arc, size);
      return size;
    }
  }

  private PropertyKey _sizeKey;
  private PropertyKey _messageTypeKey;
  private PropertyKey _textKey;
  private PropertyKey _iconKey;

  // =-=AEW The following two keys are only public until we can delete
  // the old HeaderRenderer

  // key for retrieving nesting level from the AdfRenderingContext
  public static final Object HEADER_NEST_LEVEL = new Object();

  // key for retrieving size from the AdfRenderingContext
  public static final Object HEADER_SIZE = new Object();


  // text keys
  static private final String _INFORMATION_KEY  = "af_panelHeader.INFORMATION";
  static private final String _WARNING_KEY      = "af_panelHeader.WARNING";
  static private final String _ERROR_KEY        = "af_panelHeader.ERROR";
  static private final String _CONFIRMATION_KEY = "af_panelHeader.CONFIRMATION";
  static private final String _PROCESSING_KEY   = "af_panelHeader.PROCESSING";
}
