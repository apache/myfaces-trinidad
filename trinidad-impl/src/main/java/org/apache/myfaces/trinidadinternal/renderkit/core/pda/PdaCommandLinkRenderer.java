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
package org.apache.myfaces.trinidadinternal.renderkit.core.pda;

import java.io.IOException;

import java.util.Collections;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.nav.CoreCommandLink;
import org.apache.myfaces.trinidad.context.RenderingContext;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.CommandLinkRenderer;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;

/**
 * On PDA,just render as input element of submit type if the browser 
 * doesn't support javascript.
 */

public class PdaCommandLinkRenderer extends CommandLinkRenderer
{
  public PdaCommandLinkRenderer()
  {
    super(CoreCommandLink.TYPE);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext    arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    if (supportsScripting(arc))
    {
      encodeBegin(context, arc, component, bean);
      encodeEnd(context, arc, component, bean);
      return;
    }

   
     // For Non-JavaScript browsers render the commandLink as
     // input submit element
     
    String clientId = getClientId(context, component);
    if (canSkipRendering(arc, clientId))
      return;

    // Set client ID
    assert(arc.getCurrentClientId() == null);
    arc.setCurrentClientId(clientId);

    ResponseWriter rw = context.getResponseWriter();
    String element = "input";
    rw.startElement(element, component);
    renderId(context, component);

    // Write the text and access key
    String text = getText(bean);
    rw.writeAttribute("type", "submit", null);
    
    if (getDisabled(bean))
    {
      rw.writeAttribute("disabled", Boolean.TRUE, "disabled");
      // Skip over event attributes when disabled
      renderStyleAttributes(context, arc, bean);
    }
    else
    {
      renderAllAttributes(context, arc, bean);
    }

    char accessKey;
    if (supportsAccessKeys(arc))
    {
      accessKey = getAccessKey(bean);
      if (accessKey != CHAR_UNDEFINED)
      {
        rw.writeAttribute("accesskey",
                             Character.valueOf(accessKey),
                             "accessKey");
      }
    }
    else
    {
      accessKey = CHAR_UNDEFINED;
    }
    rw.writeAttribute("name", XhtmlUtils.getEncodedParameter
                                   ( XhtmlConstants.SOURCE_PARAM )
                                   +  clientId, null);

    rw.writeAttribute("id", clientId , "id");
    rw.writeAttribute("value", text, "text");
    //This style makes an input element appear as a link
    rw.writeAttribute("style",
       "border:none;background:inherit;text-decoration:underline;", null);
    rw.endElement(element);
    arc.setCurrentClientId(null);
  }
  
  /**
   * Renders the client ID as both "id" and "name"
   * @param context the FacesContext object
   * @param component the UIComponent object
   * @throws IOException
   */
  @Override
  protected void renderId(
    FacesContext context,
    UIComponent  component) throws IOException
  { 
    if (shouldRenderId(context, component))
    {
      String clientId = getClientId(context, component);
      context.getResponseWriter().writeURIAttribute("id", clientId, "id");
      RenderingContext arc = RenderingContext.getCurrentInstance();
      // For Non-JavaScript browsers, name attribute is handled separately 
      // so skip it here
      if (supportsScripting(arc))
      {
        context.getResponseWriter().writeURIAttribute("name", clientId, "id");
      }
    } 
  }

}
