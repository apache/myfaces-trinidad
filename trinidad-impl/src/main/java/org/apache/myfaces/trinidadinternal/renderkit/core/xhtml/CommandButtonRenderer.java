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
    if (getPartialSubmit(bean))
    {
      AutoSubmitUtils.writeDependencies(context, arc);
    }

    String clientId = component.getClientId(context);
    // Make sure we don't have anything to save
    assert(arc.getCurrentClientId() == null);
    arc.setCurrentClientId(clientId);
 
    boolean useButtonTag = useButtonTags(arc);
    String element = useButtonTag ? "button" : "input";
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement(element, component);
    renderId(context, component);

    // Write the text and access key
    String text = getText(bean);
    String icon = getIcon(bean);

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
                          new Character(accessKey),
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
    return toUri(bean.getProperty(_iconKey));
  }

  private PropertyKey _iconKey;

  static private final List<String> _DISABLED_STATE_LIST =
    Collections.singletonList(SkinSelectors.STATE_DISABLED);
}
