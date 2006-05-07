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
package org.apache.myfaces.adfinternal.renderkit.core.xhtml;

import java.io.IOException;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import javax.faces.context.ResponseWriter;
import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.PropertyKey;
import org.apache.myfaces.adf.component.core.input.CoreSelectBooleanRadio;
import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;

/**
 */
public class SimpleSelectBooleanRadioRenderer extends SimpleSelectBooleanRenderer
{
  public SimpleSelectBooleanRadioRenderer()
  {
    this(CoreSelectBooleanRadio.TYPE);
  }

  public SimpleSelectBooleanRadioRenderer(FacesBean.Type type)
  {
    super(type);
  }
  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _groupKey = type.findKey("group");
  }
  
  //**********************
  //decode
  //**********************
  
  public Object getSubmittedValue(
    FacesContext context,
    UIComponent  component)
  {
    String group = getGroup(getFacesBean(component));
    if (group != null)
    {
      Object newValue = (context.getExternalContext().
                         getRequestParameterMap().get(group));
      String clientId = component.getClientId(context);
      if (clientId.equals(newValue))
        return Boolean.TRUE;
    }
    
    return Boolean.FALSE;
  }

  
  //**********************
  //encode
  //**********************

  protected Object getValueAttr(AdfRenderingContext arc)
  {
    return arc.getCurrentClientId();
  }

  protected Object getType()
  {
    return "radio";
  }

  protected String getIconAltTextName(
    boolean selected
  )
  {
    return (selected
      ? "af_selectBooleanRadio.READONLY_CHECKED_TIP" 
      : "af_selectBooleanRadio.READONLY_NOT_CHECKED_TIP");
  }
  
  protected String getIconName(
    boolean selected,
    boolean disabled
  )
  {
    final String iconName;

    if (disabled)
    {
      iconName = (selected ? 
              XhtmlConstants.AF_SELECT_BOOLEAN_RADIO_DISABLED_SELECTED_ICON_NAME : 
              XhtmlConstants.AF_SELECT_BOOLEAN_RADIO_DISABLED_UNSELECTED_ICON_NAME);
    }
    else
    {
      iconName = (selected ? 
              XhtmlConstants.AF_SELECT_BOOLEAN_RADIO_READONLY_SELECTED_ICON_NAME : 
              XhtmlConstants.AF_SELECT_BOOLEAN_RADIO_READONLY_UNSELECTED_ICON_NAME);
    }
    
    return iconName;           
  }

  protected void renderNameAttribute(
    FacesContext        context,
    AdfRenderingContext arc,
    FacesBean           bean
    )throws IOException
  {
    String group = getGroup(bean);
    if (group != null)
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.writeAttribute("name", group, null);
    }
  }

  /**
   * Returns true if the component should render the ID as a name.
   * By default, don't if the component is readonly.
   */
  protected boolean shouldRenderName(
    FacesContext context,
    UIComponent  component)
  {
    return false;
  }


  protected boolean isRadio()
  {
    return true;
  }


  protected String getCompositeId(String clientId)
  {
    return clientId + XhtmlConstants.COMPOSITE_ID_EXTENSION + "r";   
  }
  

  protected void renderSpanEventHandlers(
    FacesContext context, 
    FacesBean    bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    if ( isAutoSubmit(bean))
      rw.writeAttribute("onclick", getAutoSubmitScript(bean) , null);
      
    rw.writeAttribute("ondblclick", getOndblclick(bean),  "ondblclick");
    rw.writeAttribute("onkeydown", getOnkeydown(bean),  "onkeydown");
    rw.writeAttribute("onkeyup", getOnkeyup(bean),  "onkeyup");
    rw.writeAttribute("onkeypress", getOnkeypress(bean),  "onkeypress");
    rw.writeAttribute("onmousedown", getOnmousedown(bean),  "onmousedown");
    rw.writeAttribute("onmousemove", getOnmousemove(bean),  "onmousemove");
    rw.writeAttribute("onmouseout", getOnmouseout(bean),  "onmouseout");
    rw.writeAttribute("onmouseover", getOnmouseover(bean),  "onmouseover");
    rw.writeAttribute("onmouseup", getOnmouseup(bean),  "onmouseup");
  }
  
  protected void renderInputEventHandlers(
    FacesContext context, 
    FacesBean    bean) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.writeAttribute("onclick", getOnclick(bean),  "onclick");
    writer.writeAttribute("onblur", getOnblur(bean),  "onblur");
    writer.writeAttribute("onfocus", getOnfocus(bean),  "onfocus");
    writer.writeAttribute("onchange", getOnchange(bean),  "onchange");
  }
  
  protected String getGroup(FacesBean bean)
  {
    return toString(bean.getProperty(_groupKey));
  }
  
  protected String getContentStyleClass(FacesBean bean)
  {
   return "af|selectBooleanRadio::content";
  }
  
  protected String getRootStyleClass(FacesBean bean)
  {
   return "af|selectBooleanRadio";
  }
  
  private PropertyKey _groupKey;
}
