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
import java.util.Collections;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.nav.CoreCommandButton;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

public class CommandButtonRenderer extends CommandLinkRenderer
{
  public CommandButtonRenderer()
  {
    this(CoreCommandButton.TYPE);
  }

  protected CommandButtonRenderer(FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _iconKey = type.findKey("icon");
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
    String clientId = getClientId(context, component);
    if (canSkipRendering(arc, clientId))
      return;

    if (getPartialSubmit(bean))
    {
       AutoSubmitUtils.writeDependencies(context, arc);
    }

    // Make sure we don't have anything to save
    assert(arc.getCurrentClientId() == null);
    arc.setCurrentClientId(clientId);
 	
    ResponseWriter rw = context.getResponseWriter();
    String icon = getIcon(bean);

    //if icon is set, render as an image element within a link element
    //since "buttons" html element is not supported and "input" element of
    //type=image does not support "onClick" JS handler.
    if((icon != null) && !_supportsOnClickOnImgInput(arc)) 
    {
      if(!getDisabled(bean))
      {
        rw.startElement(XhtmlConstants.LINK_ELEMENT, component);
        renderEncodedActionURI(context, XhtmlConstants.HREF_ATTRIBUTE, "#");
        rw.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, 
                           getOnclick(bean), null);
        rw.startElement("img", component);
        renderAllAttributes(context, arc, bean);
        renderEncodedResourceURI(context, "src", icon);
        rw.endElement("img");
        rw.endElement(XhtmlConstants.LINK_ELEMENT);
      }    
      else
      {
        //If disabled attribute is set on PDAs for commandButtons set as icon,
        //render a static image
        rw.startElement("img",component);
        renderAllAttributes(context, arc, bean);
        renderEncodedResourceURI(context, "src", icon);
        rw.endElement("img");
      }
    } 
    else
    {
      boolean useButtonTag = useButtonTags(arc);
      String element = useButtonTag ? "button" : "input";
      rw.startElement(element, component);
      renderId(context, component);

      // Write the text and access key
      String text = getText(bean);
      
      if (useButtonTag)
        rw.writeAttribute("type", getButtonType(), null);
      else if (icon != null)
        rw.writeAttribute("type", "image", null);
      else
        rw.writeAttribute("type", getInputType(), null);

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
      if (useButtonTag)
      {
        AccessKeyUtils.renderAccessKeyText(context,
                                           text,
                                           accessKey,
                                           SkinSelectors.AF_ACCESSKEY_STYLE_CLASS);
        if (icon != null)
          OutputUtils.renderImage(context, arc, icon, null, null, null,
                                    getShortDesc(bean));
      } 
      else
      {
        // For Non-JavaScript browsers, encode the name attribute with the 
        // parameter name and value thus it would enable the browsers to 
        // include the name of this element in its payLoad if it submits the
        // page.
         
        if(!supportsScripting(arc))
        {
          String encodingKey = 
                        (icon != null)? TrinidadRenderingConstants.NO_JS_INPUT_IMAGE_KEY
                                      : TrinidadRenderingConstants.NO_JS_PARAMETER_KEY;
                                      
          rw.writeAttribute("name", TrinidadRenderingConstants.SOURCE_PARAM + encodingKey
                                    + clientId, null);
        }
                
        if (icon != null)
        {
          renderEncodedResourceURI(context, "src", icon);
        }
        else
        {
          rw.writeAttribute("value", text, "text");
        }
      }
 
      rw.endElement(element);
    }
    arc.setCurrentClientId(null);
  }

  protected String getButtonType()
  {
    return "button";
  }

  protected String getInputType()
  {
    return "submit";
  }
    
  protected boolean useButtonTags(RenderingContext arc)
  {
    return (supportsScripting(arc) &&
            supportsAdvancedForms(arc) &&
            supportsIntrinsicEvents(arc));
            
  }

  /**
   * Override to return any state-based (selected, disabled, etc.)
   * CSS style markers.  HINT: use an immutable, cached List<String>
   * for better performance.
   */
  protected List<String> getStateStyleClasses(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean)
  {
    if (getDisabled(bean))
      return _DISABLED_STATE_LIST;
    return null;
  }

  // FIXME: move this implementation to XhtmlRenderer
  @Override
  protected void renderStyleAttributes(
    FacesContext        context,
    RenderingContext    arc,
    FacesBean           bean,
    String              defaultStyleClass) throws IOException
  {
    String styleClass = getStyleClass(bean);
    // -= Simon =-
    // FIXME: How come inline style is never read locally?
    // String inlineStyle = getInlineStyle(bean);
    List<String> stateStyleClasses = getStateStyleClasses(context, arc, bean);

    if ((styleClass==null) && 
        (defaultStyleClass != null) && 
        (stateStyleClasses == null))
    {
      renderStyleClass(context, arc, defaultStyleClass);
    }
    else
    {
      List<String> parsedStyleClasses = OutputUtils.parseStyleClassList(styleClass);
      int userStyleClassCount;
      if (parsedStyleClasses == null)
        userStyleClassCount = (styleClass == null) ? 0 : 1;
      else
        userStyleClassCount = parsedStyleClasses.size();

      int numStates =   ((stateStyleClasses != null) ? 
                         stateStyleClasses.size() : 0);
      int numClasses = userStyleClassCount +
                        ((defaultStyleClass != null) ? 1 : 0) +
                        numStates;
      if (numClasses > 0)
      {
        // set all the styleClasses in one array so we can pass it to 
        // renderStyleClasses
        String[] styleClasses = new String[numClasses];
        
        int i=0;
        if (parsedStyleClasses != null)
        {
          while (i < userStyleClassCount)
          {
            styleClasses[i] = parsedStyleClasses.get(i);
            i++;
          }
        }
        else if (styleClass != null)
        {
          styleClasses[i++] = styleClass;
        }
        if (defaultStyleClass != null)
          styleClasses[i++] = defaultStyleClass;
         
        for (int j=0; j < numStates; j++, i++)
        {
          styleClasses[i] = stateStyleClasses.get(j);
        }        

        renderStyleClasses(context, arc, styleClasses);         
      }
    }

    String style = getInlineStyle(bean);
    if (style != null)
    {
      context.getResponseWriter().writeAttribute("style",
                                                 style,
                                                 "inlineStyle");
    }
  }

  @Override
  protected String getDefaultStyleClass(FacesBean bean)
  {
    return SkinSelectors.AF_COMMAND_BUTTON_STYLE_CLASS;
  }

  protected String getIcon(FacesBean bean)
  {
    return toResourceUri(FacesContext.getCurrentInstance(), bean.getProperty(_iconKey));
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
  
  /**
   * Returns true if the agent supports the "onclick" JS Handler in an "input" 
   * HTML element of type "image"
   *
   */
  static private boolean _supportsOnClickOnImgInput(RenderingContext arc)
  {
    Object cap = arc.getAgent().getCapabilities().get(
                      TrinidadAgent.CAP_ONCLICK_IMG_INPUT);
    return !Boolean.FALSE.equals(cap);
  }

  private PropertyKey _iconKey;

  static private final List<String> _DISABLED_STATE_LIST =
    Collections.singletonList(SkinSelectors.STATE_DISABLED);
}
