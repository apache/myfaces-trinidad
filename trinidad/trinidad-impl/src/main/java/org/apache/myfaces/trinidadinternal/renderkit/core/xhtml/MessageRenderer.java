/*
 * Copyright  2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.output.CoreMessage;

import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;
import org.apache.myfaces.trinidadinternal.util.MessageUtils;

/**
 * Renderer for org.apache.myfaces.trinidad.Message, family org.apache.myfaces.trinidad.Message.
 * 
 */
public class MessageRenderer extends ValueRenderer
{
  public MessageRenderer()
  {
    this(CoreMessage.TYPE);
  }

  protected MessageRenderer(FacesBean.Type type)
  {
    super(type);
  }

  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _forKey         = type.findKey("for");
    _messageTypeKey = type.findKey("messageType");
    _messageKey     = type.findKey("message");
  }

  public boolean getRendersChildren()
  {
    return true;
  }

  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    String message = getMessage(bean);
    String messageType = getMessageType(bean);

    if ((message == null) || (messageType == null))
    {
      String forId = getForId(context, component, bean);
      FacesMessage facesMessage = MessageUtils.getFacesMessage(context, forId);
      if (facesMessage != null)
      {
        if (message == null)
          message = facesMessage.getDetail();
        if (messageType == null)
          messageType = MessageUtils.getMessageTypeFromSeverity(
                        facesMessage.getSeverity());
      }
    }
    
    UIComponent help = component.getFacet("help");   
   
    boolean isError = CoreMessage.MESSAGE_TYPE_ERROR.equals(messageType);
    
    String styleClass = null;
    String inlineStyle;
    if (getIndented(bean))
    {
      inlineStyle = (arc.isRightToLeft()
                     ? _sRTL_INDENTED_STYLE
                     : _sLTR_INDENTED_STYLE);
    }
    else
    {
      inlineStyle = null;
    }
    
    boolean hasHelp = (help != null && help.isRendered());   
    boolean hasMessage = (message != null);
  
    
    // When there's both a help facet and a message, and we need to indent,
    // then we want to use a div around the whole thing - so that both
    // the help facet and message will be indented.  Otherwise, we'll
    // get away with a span.
    boolean useDiv = (hasHelp && hasMessage && (inlineStyle != null));
    if (useDiv)
    {
      writer.startElement("div", component);
    }
    else
    {
      writer.startElement("span", component);
    }

    renderId(context, component);
    // render all the attributes BUT the style classes, since we'll do 
    // that separately.
    renderAllAttributes(context, arc, bean, false);
    
    String beanInlineStyle = getInlineStyle(bean);
    _renderInlineStyles(writer, inlineStyle, beanInlineStyle);

    // get the bean's styleClass attribute. we'll render it along with
    // our default styles if necessary.
    String beanStyleClass = getStyleClass(bean);
 

    // both help facet and message exist and messageType is error
    // so need 2 divs inside span with id
    if ( hasHelp && hasMessage && isError )
    {
      if (beanStyleClass != null)
        renderStyleClass(context, arc, beanStyleClass);        
      
      writer.startElement("div", null);
      renderStyleClass(context, arc, XhtmlConstants.INLINE_INFO_TEXT_STYLE_CLASS); 
      encodeChild(context, help); 
      writer.endElement("div");

      // Use a div for the error message to force it onto a new line
      writer.startElement("div", null);
      renderStyleClass(context, arc, 
                       XhtmlConstants.INLINE_ERROR_TEXT_STYLE_CLASS);
      renderPossiblyFormattedText(context, message);
      writer.endElement("div");
    }    
    // Either
    // 1. Only help facet exists
    // 2. Only message exists
    // 3. Both exist and messageType not error
    else if (hasHelp || hasMessage)
    {
      

      // if there's a help facet the styleclass should not be error
      if (hasHelp)
        styleClass = XhtmlConstants.INLINE_INFO_TEXT_STYLE_CLASS;
      else
        styleClass = isError ? XhtmlConstants.INLINE_ERROR_TEXT_STYLE_CLASS :
                               XhtmlConstants.INLINE_INFO_TEXT_STYLE_CLASS; 
      // render default styles along with bean's styleClass if it exists.
      if (beanStyleClass != null)
      {
        StringBuffer styleClassBuffer = 
          new StringBuffer(styleClass.length() + beanStyleClass.length() + 1);
        styleClassBuffer.append(styleClass);
        styleClassBuffer.append(' ');
        styleClassBuffer.append(beanStyleClass);
        renderStyleClass(context, arc, styleClassBuffer.toString());
      }
      else
        renderStyleClass(context, arc, styleClass);
      

      if ( hasHelp )        
      encodeChild(context, help); 

      if (hasHelp && hasMessage)
      {
        writer.startElement("br", null);
        writer.endElement("br");
      }

      if (hasMessage)
      {
        renderPossiblyFormattedText(context, message);
      }


    }

    if (useDiv)
    {
      writer.endElement("div");
    }
    else
    {
      writer.endElement("span");
    }
  }

  //
  // NEW HOOKS
  // 

  protected boolean getIndented(FacesBean bean)
  {
    return false;
  }

  protected String getFor(FacesBean bean)
  {
    return toString(bean.getProperty(_forKey));
  }

  protected String getMessageType(FacesBean bean)
  {
    // We're used in some composite circumstances where
    // the message type is always derived from the presence
    // of a message, and cannot be overridden
    if (_messageTypeKey == null)
      return null;

    return toString(bean.getProperty(_messageTypeKey));
  }

  protected String getMessage(FacesBean bean)
  {
    // Ditto.
    if (_messageKey == null)
      return null;

    return toString(bean.getProperty(_messageKey));
  }

  
  protected String getForId(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    String forValue = getFor(bean);
    if (forValue == null)
      return null;

    return MessageUtils.getClientIdFor(context,
                                       component,
                                       forValue);
  }


  private void _renderInlineStyles(
    ResponseWriter      writer,
    String              inlineStyle, 
    String              beanInlineStyle) throws IOException
  {
    // if neither of these are null, then combine them and render
    if (inlineStyle != null && beanInlineStyle != null)
    {
      StringBuffer buffer = new StringBuffer(inlineStyle.length() +
                                             beanInlineStyle.length() + 
                                             1);
      buffer.append(inlineStyle);
      if (!inlineStyle.endsWith(";"))
        buffer.append(";");
      buffer.append(beanInlineStyle);
      writer.writeAttribute("style", buffer.toString(), null);
    }
    else if (inlineStyle != null)
      writer.writeAttribute("style", inlineStyle, null);
    else if (beanInlineStyle != null)
      writer.writeAttribute("style", beanInlineStyle, null);
  }

  private PropertyKey _forKey;
  private PropertyKey _messageTypeKey;
  private PropertyKey _messageKey;


  static private final String _sLTR_INDENTED_STYLE = "margin-left:21px";
  static private final String _sRTL_INDENTED_STYLE = "margin-right:21px";
}
