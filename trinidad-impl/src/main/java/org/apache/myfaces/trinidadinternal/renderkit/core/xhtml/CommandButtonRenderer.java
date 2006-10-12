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
    rw.writeAttribute("type", useButtonTag ? getButtonType() : getInputType(), null);
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

    // Write the text and access key
    String text = getText(bean);
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
                                         getText(bean),
                                         accessKey,
                                         SkinSelectors.AF_ACCESSKEY_STYLE_CLASS);
    }
    else
    {
      rw.writeAttribute("value", text, "text");
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
  protected void renderStyleAttributes(
    FacesContext        context,
    RenderingContext    arc,
    FacesBean           bean,
    String              defaultStyleClass) throws IOException
  {
    String styleClass = getStyleClass(bean);
    String inlineStyle = getInlineStyle(bean);
    List<String> stateStyleClasses = getStateStyleClasses(context, arc, bean);

    if ((styleClass==null) && 
        (defaultStyleClass != null) && 
        (stateStyleClasses == null))
    {
      renderStyleClass(context, arc, defaultStyleClass);
    }
    else
    {
      int numStates =   ((stateStyleClasses != null) ? 
                         stateStyleClasses.size() : 0);
      int numClasses = ((styleClass != null) ? 1 : 0) +
                        ((defaultStyleClass != null) ? 1 : 0) +
                        numStates;
      if (numClasses > 0)
      {
        // set all the styleClasses in one array so we can pass it to 
        // renderStyleClasses
        String[] styleClasses = new String[numClasses];
        
        int i=0;
        if (styleClass != null)
          styleClasses[i++] = styleClass;
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

  protected String getDefaultStyleClass(FacesBean bean)
  {
    return SkinSelectors.AF_COMMAND_BUTTON_STYLE_CLASS;
  }

  static private final List<String> _DISABLED_STATE_LIST =
    Collections.singletonList(SkinSelectors.STATE_DISABLED);
}
