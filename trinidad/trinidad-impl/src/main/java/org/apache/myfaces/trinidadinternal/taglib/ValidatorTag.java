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

import javax.faces.component.EditableValueHolder;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.faces.validator.Validator;
import javax.faces.webapp.UIComponentTag;
import javax.servlet.jsp.JspException;
import javax.servlet.jsp.tagext.TagSupport;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * This tag implements a generic validator tag.
 * It adds support for a "binding" attribute as well as a "validatorId"
 *
 * @todo it should be removed after we consume JSF1.2.
 *
 */
public final class ValidatorTag extends TagSupport
{

  /**
   * Sets the validatorId that will be used to find a registered Validator.
   * This property will be used if there is no
   * binding on this tag, or if the binding returns null
   * (in the latter case the Validator instance will be set on the binding).
   * @param validatorId
   */
  public void setValidatorId(String validatorId)
  {
    _validatorId = validatorId;
  }

  /**
   * if this EL binding expression is set, it will be used to get at a
   * Validator instance. This expression must be a ValueBinding to a
   * property that returns a Validator.
   * @param binding
   */
  public void setBinding(String binding)
  {
    _binding = binding;
  }

  @Override
  public int doStartTag() throws JspException
  {
    UIComponentTag tag =
        UIComponentTag.getParentUIComponentTag(pageContext);
    if (tag == null)
    {
      _LOG.severe("VALIDATOR_NOT_INSIDE_UICOMPONENT");
      return SKIP_BODY;
    }

    // make sure that this UIComponent has just been created:
    if (tag.getCreated())
    {
      // Create and register an instance with the appropriate component
      Validator validator = createValidator();
      if (validator != null)
      {
        EditableValueHolder evh = (EditableValueHolder) tag.getComponentInstance();
        evh.addValidator(validator);
      }
      else
      {
        _LOG.warning("CANNOT_CREATE_VALIDATOR", new String[] {_validatorId, _binding});
      }
    }

    return (SKIP_BODY);

  }

  /**
   * Release state.
   */
  @Override
  public void release()
  {
    super.release();
    _validatorId = null;
    _binding = null;
  }


  protected Validator createValidator() throws JspException
  {
    FacesContext context = FacesContext.getCurrentInstance();
    final ValueBinding binding;
    // 1. if there is a binding, then use that to get a Validator instance.
    if (_binding != null)
    {
      binding = context.getApplication().createValueBinding(_binding);
      Validator validator = (Validator) binding.getValue(context);
      if (validator != null)
        return validator;
    }
    else
      binding = null;

    // 2. if there was no binding, or the binding returned null, then use
    // the validatorId to get at an instance.
    if (_validatorId != null)
    {
      Validator validator =
        context.getApplication().createValidator(_validatorId);
      // 3. if there was a binding, then set the Validator instance on the binding.
      if (binding != null)
        binding.setValue(context, validator);
      return validator;
    }
    else
      _LOG.severe("MISSING_VALIDATORID");

    return null;
  }

//  private static Throwable _unwrap(Throwable t)
//  {
//    while(true)
//    {
//      Throwable causedBy = null;
//      // OC4J does not unwrap the following exceptions:
//      if (t instanceof JspException)
//      {
//        causedBy = ((JspException) t).getRootCause();
//      }
//      else if (t instanceof ServletException)
//      {
//        causedBy = ((ServletException) t).getRootCause();
//      }
//      if ((causedBy == null) || (causedBy == t))
//        return t;
//      else
//        t = causedBy;
//    }
//  }

  private String _validatorId = null;
  private String _binding = null;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ValidatorTag.class);
}
