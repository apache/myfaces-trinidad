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
package org.apache.myfaces.trinidadinternal.taglib;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.faces.webapp.UIComponentTag;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.TagSupport;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * 
 * <p>Tag implementation that adds an attribute with a specified name
 * and value to the component whose tag it is nested inside,
 * if the component does not already contain an attribute with the
 * same name.  This tag creates no output to the page currently
 * being created.</p>
 * 
 * <p>This tag differs from the JSF f:attribute tag in that when the value 
 * attribute is bound to an EL expression, the EL expression is attached 
 * to the component as a value binding for later evaluation (unlike the 
 * JSF f:attribute tag which evaluates the expression immediately as 
 * discussed in this 
 * <a href="https://javaserverfaces.dev.java.net/issues/show_bug.cgi?id=74">JSR127 bug</a>).  
 * One other minor modification is that the name attribute cannot be EL-bound.
 * </p>
 * 
 * @todo Remove this class when Trinidad upgrades to depend on JSF v1.2
 */
public class AttributeTag extends TagSupport 
{
  /**
   * <p>Set the attribute name.</p>
   * @param name The new attribute name
   */
  public void setName(String name) 
  {
    _name = name;
  }

  /**
   * <p>Set the attribute value.</p>
   * @param value The new attribute value
   */
  public void setValue(String value) 
  {
    _value = value;
  }

  /**
   * <p>Register the specified attribute name and value with the
   * {@link UIComponent} instance associated with our most immediately
   * surrounding {@link UIComponentTag} instance, if this {@link UIComponent}
   * does not already have a value for the specified attribute name.</p>
   *
   * <p> Note: When the value attribute is bound to an EL expression, the EL
   *           expression is attached to the component as a value binding for 
   *           later evaluation (unlike the JSF f:attribute tag which evaluates
   *           the expression immediately).
   *
   * @exception JspException if a JSP error occurs
   */
  @SuppressWarnings("unchecked")
  @Override
  public int doStartTag() throws JspException 
  {
    // Locate our parent UIComponentTag
    UIComponentTag tag =
        UIComponentTag.getParentUIComponentTag(pageContext);
    if (tag == null) 
    { // PENDING - i18n
      throw new JspException(_LOG.getMessage(
        "NOT_NESTED_IN_UICOMPONENTTAG"));
    }

    // Add this attribute if it is not already defined
    UIComponent component = tag.getComponentInstance();
    if (component == null) 
    { // PENDING - i18n
      throw new JspException(_LOG.getMessage(
        "NO_COMPONENT_ASSOCIATED_WITH_UICOMPONENTTAG"));
    }
    String nameVal = _name;
    if (UIComponentTag.isValueReference(_name)) 
    {
      throw new JspException(_LOG.getMessage(
        "NAME_ATTRIBUTE_CANNOT_BE_EL_BOUND"));
    }
    
    if (component.getAttributes().get(nameVal) == null) 
    {
      ValueBinding vb = null;
      if (UIComponentTag.isValueReference(_value)) 
      {
        vb = _getFacesContext().getApplication().createValueBinding(_value);
      }
      if (vb == null)
        component.getAttributes().put(nameVal, _value);
      else
        component.setValueBinding(nameVal, vb);
    }
    
    return (SKIP_BODY);
  }


  /**
   * <p>Release references to any acquired resources.
   */
  @Override
  public void release() 
  {
    super.release();
    _name = null;
    _value = null;
  }

  /**
   * <p>Retrieve the {@link FacesContext} instance for this request, caching
   * it the first time.</p>
   */
  private FacesContext _getFacesContext() 
  {
    // For non-obvious reasons, Tomcat is not calling release()
    // on AttributeTags, so the _context ends up getting held across
    // requests.  Disabling this optimization!
    //    if (_context == null) 
    //      _context = FacesContext.getCurrentInstance();
    //    return (_context);
    return FacesContext.getCurrentInstance();
  }

  // The name of the attribute to be created, if not already present.
  private String _name = null;
  // The value to be associated with this attribute, if it is created.
  private String _value = null;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    AttributeTag.class);
}
