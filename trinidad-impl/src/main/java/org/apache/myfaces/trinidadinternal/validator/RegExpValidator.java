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
package org.apache.myfaces.trinidadinternal.validator;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.MessageFactory;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.FormRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;

public class RegExpValidator
                       extends org.apache.myfaces.trinidad.validator.RegExpValidator
                         implements InternalClientValidator
{
  public RegExpValidator()
  {
    super();
  }

  /**
   * Opportunity for the ClientValidator to return script content.
   * For HTML, this will be javascript that will be embedded in a
   * script tag. For HTML this method is expected to return an
   * implementation of the javascript Validator object.
   * <p>This method will be called once per validator instance.
   * Content that should only be written once per request
   * should only be returned once.
   */
  public String getClientScript(FacesContext context, UIComponent component)
  {
    // since we are instance of InternalClientValidator this method will not
    // be invoked. see xhtml.FormRenderer#_addOnSubmitValidator(,,,,)
    // getLibKey() will serve the purpose to load the necessary script
    return null;
  }

  /**
   * Called to retrieve the appropriate client
   * validation code for the node and context.
   * For HTML, this will be javascript that will be embedded in a
   * script tag. For HTML this method is expected to return a
   * constructor of the javascript Validator object
   * returned by getClientScript().
   *
   * @todo - Though component or client id could have been null in call to
   * getClientScript() - we are still rendering the script here.
   * Probaly we should avoid null check for components totally and allow to see
   * how and where we get to set it rather than loggin for now.
   */
  public String getClientValidation(FacesContext context, UIComponent component)
  {
    String clientId = component.getClientId(context);

    if (clientId != null)
    {
      FormRenderer.addPatternMapping( clientId, getPattern());
    }
    
    String jsPattern = XhtmlUtils.escapeJS(getPattern());
    String esNoMatchMsgPattern = XhtmlUtils.escapeJS(
                  _getNoMatchMessageDetail(context));


    StringBuffer outBuffer = new StringBuffer(28
                                              + jsPattern.length()
                                              + esNoMatchMsgPattern.length());

    outBuffer.append("new RegExpFormat('"); // 18
    outBuffer.append(jsPattern);
    outBuffer.append("',");                 //  2
    outBuffer.append("{NM:'");              //  5
    outBuffer.append(esNoMatchMsgPattern);
    outBuffer.append("'}");                 //  2
    outBuffer.append(")");                  //  1

    return outBuffer.toString();
  }

  /**
   * @todo again here we dont check for the component nor the client id.
   * If it true in getClientScript it should be true here to.
   */
  public String getLibKey(
    FacesContext context,
    UIComponent component
    )
  {
     return "RegExpFormat()";
  }

 

  private String _getNoMatchMessageDetail(
    FacesContext context)
  {
    String noMatchMsg = getNoMatchMessageDetail();
    Object[] params = new Object[] {"{0}", "{1}", "{2}"};

    String noMatchDetMsg
      = MessageFactory.getMessage(context,
                                  RegExpValidator.NO_MATCH_MESSAGE_ID,
                                  noMatchMsg,
                                  params).getDetail();
    return noMatchDetMsg;
  }

  // -= Simon Lessard =-
  // TODO: Nothing is logged in this class as of 2006-08-04
  @SuppressWarnings("unused")
  private static final TrinidadLogger _LOG  = TrinidadLogger.createTrinidadLogger(
     RegExpValidator.class);
}
