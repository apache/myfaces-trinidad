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
package org.apache.myfaces.trinidadinternal.util;

import java.util.Iterator;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.core.output.CoreMessage;
import org.apache.myfaces.trinidad.convert.ClientConverter;
import org.apache.myfaces.trinidad.util.MessageFactory;

import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRendererUtils;
import org.apache.myfaces.trinidadinternal.share.util.FastMessageFormat;
import org.apache.myfaces.trinidadinternal.ui.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;

/**
 * Utility functions used for messaging.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/util/MessageUtils.java#0 $) $Date: 10-nov-2005.18:49:11 $
 * @author The Oracle ADF Faces Team
 */
public class MessageUtils
{
  private MessageUtils(){}

  public static FacesMessage getFacesMessage(
    FacesContext context,
    String       clientId)
  {
    if (clientId != null)
    {
      Iterator messages = context.getMessages(clientId);
      if (messages.hasNext())
      {
        return (FacesMessage) messages.next();
      }
    }

    return null;
  }

  /**
   * Returns a string constant message type for the severity
   * of a message.
   */
  public static String getMessageTypeFromSeverity(
   FacesMessage.Severity severity)
  {
    if (severity == FacesMessage.SEVERITY_INFO)
      return CoreMessage.MESSAGE_TYPE_INFO;
    else if (severity == FacesMessage.SEVERITY_WARN)
      return CoreMessage.MESSAGE_TYPE_WARNING;
    else
      return CoreMessage.MESSAGE_TYPE_ERROR;
  }

  /**
   * @todo Check algorithm used in RI for the "for" attribute
   *  on <h:outputLabel>;  use the same algorithm.
   * @todo This method is getting called a lot;  cache something somewhere?
   */
  public static String getClientIdFor(
    FacesContext context,
    UIComponent  from,
    String       forParam)
  {
    return CoreRendererUtils.getRelativeId(context, from, forParam);
  }


  /**
   * @todo right now I'm just appending '_msgAnc_' to the front of the
   * client id for the anchor.
   * @todo in ie6 if you don't use an anchor,
   * but rather link to the id of the input,
   * it will put the focus in the input, so maybe we shouldn't
   * return an anchor on ie6.
   */
  public static String getAnchor(
    String clientID
  )
  {
    if (clientID == null)
      return null;

    return "_msgAnc_" + clientID;
  }

  public static String getGlobalMessage(
     RenderingContext context,
     String summary,
     String detail)
  {
    if ((summary != null) && summary.equals(detail))
      return summary;

    String[] parameters = new String[] { summary, detail };
    String pattern = BaseLafRenderer.getTranslatedString(context,
                                                      _GLOBAL_FORMAT_KEY);

    return (new FastMessageFormat(pattern)).format(parameters);
  }

  public static String createErrorAlertMessage(
     FacesContext context,
     UIComponent component,
     String detailMessage)
  {
    Object label = BaseLafUtils.getComponentLabel(component);
    String text = (label != null) ? label.toString() : null;
    return createErrorAlertMessage(context, text, detailMessage);
  }

  public static String createErrorAlertMessage(
     FacesContext context,
     String label,
     String detailMessage)
  {
    String msg = detailMessage;

    // if message exists prepend the component label
    if ((detailMessage != null) && (label != null))
    {
      String[] parameters = new String[] { label, detailMessage };
      msg = MessageFactory.getMessage(context,
                                      ClientConverter.ALERT_FORMAT_KEY,
                                      parameters).getSummary();
    }
    return msg;
  }


  static private final String _GLOBAL_FORMAT_KEY =
    "af_messages.GLOBAL_MESSAGE_FORMAT";
}
