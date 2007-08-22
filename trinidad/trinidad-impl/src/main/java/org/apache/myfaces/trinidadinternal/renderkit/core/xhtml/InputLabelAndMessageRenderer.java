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
import java.util.List;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import javax.faces.context.ResponseWriter;
import org.apache.myfaces.trinidad.bean.FacesBean;

import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.util.MessageUtils;

public abstract class InputLabelAndMessageRenderer extends LabelAndMessageRenderer
{

  protected InputLabelAndMessageRenderer(FacesBean.Type type)
  {
    super(type);
  }
  
  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _simpleKey   = type.findKey("simple");
    _disabledKey   = type.findKey("disabled");
    _readOnlyKey   = type.findKey("readOnly");
  }  
  
  @Override
  public void decode(FacesContext context, UIComponent component)
  {
     getFormInputRenderer().decode(context, component);
  }
  
  @Override
  public Object getConvertedValue(
    FacesContext context,
    UIComponent  component,
    Object       submittedValue)
  {
    return  getFormInputRenderer().getConvertedValue(context,
                                              component,
                                              submittedValue);
  }

  @Override
  protected String getLabelFor(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean)
  {
      
    if (!getFormInputRenderer().renderAsElement(context, arc, bean))
      return null;

    return __getCachedClientId(arc);
  }


  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext    arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    if (getSimple(bean))
    {
      String clientId = component.getClientId(context);
      // If we're a leaf component, see if we can skip our rendering
      if (isLeafRenderer() && canSkipRendering(arc, clientId))
        return;

      String saved = arc.getCurrentClientId();
      arc.setCurrentClientId(clientId);
      
      // add the label to FormData so that it can be used in 
      // client side validation error messages.
      String value = getLabel(bean);
      FormData fd = arc.getFormData();
      if (fd != null)
        fd.addLabel(clientId, value); 
      
      FacesMessage msg = MessageUtils.getFacesMessage(context, clientId);
      if (msg != null)
      {
        ResponseWriter writer = context.getResponseWriter();
        writer.startElement("a", null);
        String anchor = MessageUtils.getAnchor(clientId);
        writer.writeAttribute("name", anchor, null);
        writer.endElement("a");
      }
      
      delegateRenderer(context, arc, component, bean, getFormInputRenderer());
      arc.setCurrentClientId(saved);
    }
    else
    {
      super.encodeAll(context, arc, component, bean);
    }
  }
  
  /**
   * If it's known that the field content is not editable, return false. 
   * Otherwise, assume it is editable and return true
   */
  @Override
  protected boolean isContentEditable(FacesBean bean)
  {
    return !getFormInputRenderer().getReadOnly(
                                   FacesContext.getCurrentInstance(), bean);  
  }
 
  @Override
  protected void renderFieldCellContents(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    delegateRenderer(context, arc, component, bean, getFormInputRenderer());
  }
 
  protected boolean getSimple(FacesBean bean)
  {
    Object o = bean.getProperty(_simpleKey);
    if (o == null)
      o = _simpleKey.getDefault();

    return !Boolean.FALSE.equals(o);
  }
   
  /**
   * Render the styles and style classes that should go on the root dom element.
   * (called from LabelAndMessageRenderer, the superclass)
   * @param context
   * @param arc
   * @param component
   * @param bean
   * @throws IOException
   */  
  @Override
  protected void renderRootDomElementStyles(
  FacesContext     context,
  RenderingContext arc,
  UIComponent      component,
  FacesBean        bean) throws IOException
  {
    // get the style classes that I want to render on the root dom element here.  
    String styleClass         = getStyleClass(bean);
    String contentStyleClass  = getRootStyleClass(bean);
    String disabledStyleClass = null;
    String readOnlyStyleClass = null;
    String requiredStyleClass = null;
   
    // readOnly takes precedence over disabled for the state.  
    // -= Simon =- Why?
    if(isReadOnly(bean))
    { // FIXME: Unlike FormInputRenderer, this isReadOnly does not check  
      //        if the specified value binding is writable. Inconsistent,
      //        what behavior should we keep?
      readOnlyStyleClass = SkinSelectors.STATE_READ_ONLY;
    }
    else if (isDisabled(bean))
    {
      disabledStyleClass = SkinSelectors.STATE_DISABLED;
    }
   
    if(labelShowRequired(bean))
    {
      requiredStyleClass = SkinSelectors.STATE_REQUIRED;
    }
   
    List<String> parsedStyleClasses =
      OutputUtils.parseStyleClassList(styleClass);
    int userStyleClassCount;
    if (parsedStyleClasses == null)
      userStyleClassCount = (styleClass == null) ? 0 : 1;
    else
      userStyleClassCount = parsedStyleClasses.size();

    String[] styleClasses = new String[userStyleClassCount + 4];
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

    styleClasses[i++] = contentStyleClass;
    styleClasses[i++] = disabledStyleClass;
    styleClasses[i++] = readOnlyStyleClass;
    styleClasses[i++] = requiredStyleClass;

    renderStyleClasses(context, arc, styleClasses);
    renderInlineStyle(context, arc, bean);  
  }
    
  protected boolean isDisabled(FacesBean bean)
  {
    Object o = bean.getProperty(_disabledKey);
    if (o == null)
      o = _disabledKey.getDefault();

    return !Boolean.FALSE.equals(o);
  }
  
  protected boolean isReadOnly(FacesBean bean)
  {
    Object o = bean.getProperty(_readOnlyKey);
    if (o == null)
      o = _readOnlyKey.getDefault();

    return !Boolean.FALSE.equals(o);
  }
   
  /**
   * @todo Default shortDesc to label when inside the data area
   * of the table (and for screenReaderMode + radio buttons?!?)
   */
  abstract protected FormInputRenderer getFormInputRenderer();
  

  private PropertyKey   _simpleKey;
  
  private PropertyKey   _disabledKey;
  private PropertyKey   _readOnlyKey;
}
