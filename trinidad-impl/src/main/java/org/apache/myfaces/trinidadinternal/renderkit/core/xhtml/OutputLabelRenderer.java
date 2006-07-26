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
import org.apache.myfaces.trinidad.component.core.output.CoreOutputLabel;

import org.apache.myfaces.trinidadinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.trinidadinternal.util.MessageUtils;
import org.apache.myfaces.trinidadinternal.skin.icon.Icon;

/**
 * Renderer for org.apache.myfaces.trinidad.Label, family org.apache.myfaces.trinidad.Output.
 * 
 * @todo Support "anchor"
 * @todo Support messageDescUrl and targetFrame
 */
public class OutputLabelRenderer extends ValueRenderer
{
  public OutputLabelRenderer()
  {
    this(CoreOutputLabel.TYPE);
  }

  protected OutputLabelRenderer(FacesBean.Type type)
  {
    super(type);
  }

  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _accessKeyKey          = type.findKey("accessKey");
    _forKey                = type.findKey("for");
    _messageTypeKey        = type.findKey("messageType");
    _showRequiredKey       = type.findKey("showRequired");
  }


  public boolean getRendersChildren()
  {
    return true;
  }

  /**
   * @todo Often, we can get by with just a single label, not
   * a span and a label
   * @todo If all that is set is "required", it seems that we
   *  *don't* render a span, but do render the icon.  This is strange.
   */
  protected void encodeAll(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  { 
    ResponseWriter rw = context.getResponseWriter();
    String value = getConvertedString(context, component, bean);

    String forId = getForId(context, component, bean);
    FormData fd = arc.getFormData();
    if (fd != null)
      fd.addLabel(forId, value); 
    
    String messageType = _getMessageType(context, bean, forId);

    boolean noSpanNeeded =
      ((value == null) &&
       ((messageType == null) || "none".equals(messageType)));

    if (!noSpanNeeded)
    {
      rw.startElement("span", component);
      renderId(context, component);
      renderAllAttributes(context, arc, bean);
    }

    boolean encodedIcons = encodeIcons(context, arc, component,
                                       bean, messageType, forId);

    if (value != null)
    {
      if (encodedIcons)
      {
        rw.writeText(XhtmlConstants.NBSP_STRING, null);
      }
      
      char accessKey;
      if (supportsAccessKeys(arc))
      {
        accessKey = getAccessKey(bean);
      }
      else
      {
        accessKey = CHAR_UNDEFINED;
      }
      
      int accessKeyIndex = AccessKeyUtils.getAccessKeyIndex(value, accessKey);
      
      boolean needsLabel = isLabelTagNeeded(arc, bean, forId, accessKeyIndex); 
      
      if (needsLabel)
      {
        rw.startElement("label", component);
        if (forId != null)
        {
          rw.writeAttribute("for", forId, "for");
          // Remember this label so we don't output it twice
          HiddenLabelUtils.rememberLabel(arc, forId);
        }
        
        if (accessKey != CHAR_UNDEFINED)
        {
          rw.writeAttribute("accesskey",
                            new Character(accessKey),
                            "accessKey");
          
        }                   
      }
      
      
      AccessKeyUtils.renderAccessKeyText(context,
                                         value,
                                         accessKey,
                                         "u");
      
      if (needsLabel)
      {
        rw.endElement("label");
      }
    }

    if (!noSpanNeeded)
    {
      rw.endElement("span");
    }
  }

  protected boolean encodeIcons(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean,
    String              messageType,
    String              forId) throws IOException
  {
    boolean encodedIcons = false;

    if ((null != messageType) &&
        !"none".equals(messageType))
    {
      String vAlign = getDefaultValign(bean);
      String destination  = getMessageDescUrl(bean);
      String targetFrame = getMessageTargetFrame(bean);
      String anchor       = MessageUtils.getAnchor(forId);

      encodedIcons = renderMessageSymbol(context, arc, messageType,
                                         destination, anchor, 
                                         targetFrame, vAlign);
    }

    if (getShowRequired(bean))
    {
      // Get the required Icon from the context
      Icon icon = arc.getIcon(XhtmlConstants.REQUIRED_ICON_ALIAS_NAME);
      if (icon != null)
      {
        String vAlign = getDefaultValign(bean);
        _renderIcon(context, arc, icon, null, null, null, "REQUIRED_TIP", vAlign);
      }

      // Render the required icon
      encodedIcons = true;
    }
    
    return encodedIcons;
  }

  protected boolean isLabelTagNeeded(
    AdfRenderingContext arc, 
    FacesBean           bean,
    String              forId, 
    int                 accessKeyIndex
  )
  {
    return (((forId != null) &&
            !isInaccessibleMode(arc)) ||
            (accessKeyIndex >= 0));
  }

  /**
   * @todo Support targetFrame???
   */
  protected boolean renderMessageSymbol(
    FacesContext        context,
    AdfRenderingContext arc,
    Object              type,
    Object              destination,
    Object              anchor,
    Object              targetFrame,
    Object              vAlign
    ) throws IOException
  {
    // Get the name of the Icon
    String iconName = null;
    String altTextKey = null;

    if (XhtmlConstants.MESSAGE_TYPE_ERROR.equals(type))
    {
      iconName = (destination == null) ? XhtmlConstants.ERROR_ICON_ALIAS_NAME :
                                         XhtmlConstants.ERROR_ANCHOR_ICON_ALIAS_NAME;
      altTextKey = "ERROR_TIP";
    }
    else if (XhtmlConstants.MESSAGE_TYPE_INFO.equals(type))
    {
      iconName = (destination == null) ? XhtmlConstants.INFO_ICON_ALIAS_NAME :
                                         XhtmlConstants.INFO_ANCHOR_ICON_ALIAS_NAME;
      altTextKey = "INFO_TIP";
    }
    else if (XhtmlConstants.MESSAGE_TYPE_WARNING.equals(type))
    {
      iconName = (destination == null) ? XhtmlConstants.WARNING_ICON_ALIAS_NAME :
                                         XhtmlConstants.WARNING_ANCHOR_ICON_ALIAS_NAME;
      altTextKey = "WARNING_TIP";
    }

    if (iconName != null)
    {
      // Get the Icon to render from the skin
      Icon icon = arc.getIcon(iconName);
      
      if (icon != null)
      {
        _renderIcon(context,
                    arc,
                    icon,
                    destination,
                    anchor,
                    targetFrame,
                    altTextKey,
                    vAlign);

        return true;
      }
    }

    return false;
  }


  //
  // OVERRIDES
  //
  
   protected void renderAllAttributes(
     FacesContext        context,
     AdfRenderingContext arc,
     FacesBean           bean) throws IOException
   {
     renderAllAttributes(context, arc, bean, false);
     renderStyleAttributes(context, arc, bean, getDefaultStyleClass());
   }  
  
  protected String getDefaultStyleClass()
  {
    return "af|outputLabel";
  }

  private void _renderIcon(
    FacesContext        context,
    AdfRenderingContext arc,
    Icon             icon,
    Object           destination,
    Object           anchor,
    Object           targetFrame,
    String           altTextKey,
    Object           vAlign
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    boolean renderedAnchor = false;

    if ((destination != null) || (anchor != null))
    {
      if (supportsNavigation(arc))
      {
        writer.startElement("a", null);
        renderEncodedActionURI(context, "href", destination);
        writer.writeAttribute("target", targetFrame, null);
        writer.writeAttribute("name", anchor, null);
         
        // Set renderedAnchor to true so that we know that
        // we need to close the anchor element.
        renderedAnchor = true;
      }
    }

    // Get ready to render the Icon.  We need to get the
    // alt text, and also check to see whether the Icon
    // should render the style class
    Object altText = arc.getTranslatedString(altTextKey);

    // Apply the default alignment
    if (vAlign == null)
      vAlign = OutputUtils.getMiddleIconAlignment(arc);

    // Render the icon, specifying embedded=renderedAnchor.
    // This allows text-based Icons to render their style class
    // and altText directly on the anchor itself
    OutputUtils.renderIcon(context,
                           arc,
                           icon,
                           altText,
                           vAlign,
                           renderedAnchor);

    // Close up the anchor if necessary
    if (renderedAnchor)
      writer.endElement("a");
  }

  private String _getMessageType(
    FacesContext        context,
    FacesBean           bean,
    String              forId) throws IOException
  {
    // Derive the message type
    String messageType = getMessageType(bean);
    if (null == messageType)
    {
      FacesMessage message = MessageUtils.getFacesMessage(context, forId);
      if (message != null)
      {
        messageType = MessageUtils.getMessageTypeFromSeverity(
                        message.getSeverity());
      }
    }

    return messageType;
  }


  //
  // NEW HOOKS
  // 

  protected boolean getShowRequired(FacesBean bean)
  {
    Object o = bean.getProperty(_showRequiredKey);
    if (o == null)
      o = _showRequiredKey.getDefault();

    return Boolean.TRUE.equals(o);
  }  

  /**
   * we default the valign. the user can use skinning to override.
   */ 
  protected String getDefaultValign(FacesBean bean)
  {
    return null;
  }

  protected char getAccessKey(FacesBean bean)
  {
    return toChar(bean.getProperty(_accessKeyKey));
  }


  protected String getFor(FacesBean bean)
  {
    return toString(bean.getProperty(_forKey));
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

  protected String getMessageType(FacesBean bean)
  {
    // We're used in some composite circumstances where
    // the message type is always derived from the presence
    // of a message, and cannot be overridden
    if (_messageTypeKey == null)
      return null;
    return toString(bean.getProperty(_messageTypeKey));
  }
  

  protected String getMessageDescUrl(FacesBean bean)
  {
    return null;
  }  
  

  protected String getMessageTargetFrame(FacesBean bean)
  {
    
    return null;
  }    


  private PropertyKey _accessKeyKey;
  private PropertyKey _forKey;
  private PropertyKey _messageTypeKey;
  private PropertyKey _showRequiredKey;
}
