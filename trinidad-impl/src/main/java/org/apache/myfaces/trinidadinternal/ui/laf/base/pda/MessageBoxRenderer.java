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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import java.io.IOException;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.component.core.output.CoreMessages;

import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.beans.MarlinBean;
import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.MessageBoxUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafRenderer;
import org.apache.myfaces.trinidadinternal.skin.icon.Icon;


/**
 * Renders a message box.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/MessageBoxRenderer.java#0 $) $Date: 10-nov-2005.18:54:58 $
 * @author The Oracle ADF Faces Team
 */
public class MessageBoxRenderer extends XhtmlLafRenderer
{
  // check for number of links at pre-, post-, and content.
  @Override
  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {

    if (MessageBoxUtils.sIsRendered(context, node, _allMessages))
    {
      super.prerender(context, node);
      context.setLocalProperty( _MB_IS_RENDERED, Boolean.TRUE);
    }
  }

  @Override
  protected void postrender(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object isRendered = context.getLocalProperty( 0, _MB_IS_RENDERED, null);
    if ( Boolean.TRUE.equals(isRendered) )
      super.postrender(context, node);
  }


  //
  // do the rendering work
  //
  @Override
  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object isRendered = context.getLocalProperty( 0, _MB_IS_RENDERED, null);
    if ( Boolean.TRUE.equals(isRendered) )
    {
      int messageType
        = _getMessageTypeBySeverity(MessageBoxUtils.sGetMaxSeverity(context));

      Icon icon  = _getIcon(context, messageType);

      String messageStyle = _getMessageTextStyle(messageType);
      Object message = node.getAttributeValue(context, MESSAGE_ATTR);

      _renderLine(context,
                  node,
                  messageStyle,
                  icon,
                  messageType,
                  message);
    }


  }

  private static final void _renderLine(
    UIXRenderingContext context,
    UINode           node,
    String           messageStyle,
    Icon             icon,
    int              messageType,
    Object           message
    )throws IOException
  {

    ResponseWriter writer = context.getResponseWriter();

    MarlinBean text = new MarlinBean(STYLED_TEXT_NAME);
    RenderingContext arc = RenderingContext.getCurrentInstance();
    FacesContext fContext = context.getFacesContext();
    icon.renderIcon(fContext, arc, null);

    if (icon != null)
      writer.write("&nbsp;");

    String messageKey = _MESSAGE_TYPE_KEYS[messageType];

    text.setStyleClass(messageStyle);
    text.setAttributeValue(TEXT_ATTR, getTranslatedString(context, messageKey) );

    writer.startElement("b", node.getUIComponent());
    text.render(context);

    if (message != null)
    {
      writer.startElement("br", null);
      writer.endElement("br");

      writer.writeText( message, CoreMessages.MESSAGE_KEY.getName());
    }

    writer.endElement("b");
    sep.render(context, node);
  }

  private String _getMessageTextStyle(
    int messageType
    )
  {
    return (messageType == _ERROR_TYPE)
             ? AF_MESSAGES_ERROR_STYLE_CLASS
             : AF_MESSAGES_HEADER_STYLE_CLASS;
  }


  //
  // Private methods
  //

  private int _getMessageTypeBySeverity(FacesMessage.Severity severity)
  {
    if (FacesMessage.SEVERITY_ERROR.compareTo(severity) == 0)
      return _ERROR_TYPE;
    else if (FacesMessage.SEVERITY_WARN.compareTo(severity) == 0)
      return _WARNING_TYPE;
    else if (FacesMessage.SEVERITY_INFO.compareTo(severity) == 0)
      return _INFORMATION_TYPE;
    else return _CONFIRMATION_TYPE;
  }

  private Icon _getIcon(
    UIXRenderingContext context,
    int              messageType)
  {
    return context.getIcon(_getIconName(messageType));
  }

  private String _getIconName(
    int messageType
    )
  {
    return _ICON_NAMES[messageType];
  }

  // images
  private final static String[] _ICON_NAMES =
  {
    AF_MESSAGES_INFO_ICON_NAME,
    AF_MESSAGES_WARNING_ICON_NAME,
    AF_MESSAGES_ERROR_ICON_NAME,
    AF_MESSAGES_CONFIRMATION_ICON_NAME,
  };

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return null;
  }


  //
  // Private variables
  //

  // On PDA we render only global messages
  private final static boolean _allMessages = false;

  // local message types
  static private final int _INFORMATION_TYPE  = 0;
  static private final int _WARNING_TYPE      = 1;
  static private final int _ERROR_TYPE        = 2;
  static private final int _CONFIRMATION_TYPE = 3;

  // text keys
  static private final String _INFORMATION_KEY  = "af_messages.INFORMATION";
  static private final String _WARNING_KEY      = "af_messages.WARNING";
  static private final String _ERROR_KEY        = "af_messages.ERROR";
  static private final String _CONFIRMATION_KEY = "af_messages.CONFIRMATION";

  // message type
  private final static String[] _MESSAGE_TYPE_KEYS =
  {
    _INFORMATION_KEY,
    _WARNING_KEY,
    _ERROR_KEY,
    _CONFIRMATION_KEY
  };


  private static final Object _MB_IS_RENDERED = new Object();
  // Create a Separator renderer, but don't render the ID
  // =-=AEW This hack is necessary for RenderKit test passing;
  // it can be cleaned up as long as you fix the RenderKit test too.
  private static final SeparatorRenderer sep = new SeparatorRenderer()
  {
    @Override
    protected void renderID(UIXRenderingContext context, UINode node)
    {
      ;
    }
  };
}
