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


import org.apache.myfaces.adf.bean.FacesBean;

import org.apache.myfaces.adf.bean.PropertyKey;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;

abstract public class FormInputRenderer extends FormElementRenderer
{
  protected FormInputRenderer(FacesBean.Type type)
  {
    super(type);
  }
  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _simpleKey   = type.findKey("simple");
  }
  
  /**
   * Render the client ID as both an "id" and a "name"
   */
  protected void renderId(
    FacesContext context,
    UIComponent  component) throws IOException
  {
    String clientId = getClientId(context, component);
    context.getResponseWriter().writeAttribute("id", clientId, "id");

    if (shouldRenderName(context, component))
    // Don't render the name if it's read-only
      context.getResponseWriter().writeAttribute("name", clientId, "id");
  }

  /**
   * Returns true if the component should render the ID as a name.
   * By default, don't if the component is readonly.
   */
  protected boolean shouldRenderName(
    FacesContext context,
    UIComponent  component)
  {
    FacesBean bean = getFacesBean(component);
    return !getReadOnly(context, bean);
  }


  protected void renderDisabledAttribute(
    FacesContext        context,
    AdfRenderingContext arc,
    FacesBean           bean) throws IOException
  {
   
    if (getDisabled(bean))
    {
      context.getResponseWriter().writeAttribute("disabled",
                                                 Boolean.TRUE,
                                                 "disabled");
    }
  }
 
  /**
   * used in the form input components for the 'content' piece.
   * @param context
   * @param arc
   * @param bean
   * @param renderStyleAttrs, whether to render the styleClass/inlineStyle
   * attribute values on the 'content' piece. This is usually false.
   * @throws IOException
   */
  protected void renderAllAttributes(
    FacesContext        context,
    AdfRenderingContext arc,
    FacesBean           bean,
    boolean             renderStyleAttrs) throws IOException
  {
    super.renderAllAttributes(context, arc, bean, renderStyleAttrs);
    renderDisabledAttribute(context, arc, bean);
    renderStyleClass(context, arc, getContentStyleClass(bean));
    renderInlineStyleAttribute(context, arc, getContentStyle(bean));
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
  protected void renderRootDomElementStyles(
  FacesContext        context,
  AdfRenderingContext arc,
  UIComponent         component,
  FacesBean           bean) throws IOException
  {
   // get the style classes that I want to render on the root dom element here.  
   // readOnly takes precedence over disabled for the state.  
   String styleClass = getStyleClass(bean);
   String disabledStyleClass = null;
   String readOnlyStyleClass = getReadOnly(context, bean) ? "p_AFReadOnly" : null;
   if (readOnlyStyleClass == null)
    disabledStyleClass = getDisabled(bean) ? "p_AFDisabled" : null;
   renderStyleClasses(context, arc, new String[]{styleClass,
                                                 getRootStyleClass(bean),  
                                                 disabledStyleClass, 
                                                 readOnlyStyleClass });
    
    renderInlineStyle(context, arc, bean);  
  }
  /*
   * override to return the content style class, like af|inputText::content
   * if component is af:inputText.
   */
  abstract protected String getContentStyleClass(FacesBean bean); 
  /*
   * override to return the root style class, like af|inputText
   * if component is af:inputText.
   */  
  abstract protected String getRootStyleClass(FacesBean bean);
  
  private PropertyKey _simpleKey;

}
