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

import java.util.Iterator;

import javax.el.ValueExpression;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;
import javax.faces.validator.Validator;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXEditableValue;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;

abstract public class EditableValueRenderer extends ValueRenderer
{
  protected EditableValueRenderer(FacesBean.Type type)
  {
    super(type);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
   {
    super.findTypeConstants(type);
    _submittedValueKey = type.findKey("submittedValue");
    _readOnlyKey = type.findKey("readOnly");
    _disabledKey = type.findKey("disabled");
    _requiredKey = type.findKey("required");
    _immediateKey = type.findKey("immediate");
    _validatorsKey = type.findKey("validators");
  }

  //
  // DECODING IMPLEMENTATION
  //
  @Override
  public void decode(FacesContext context, UIComponent component)
  {
    if (skipDecode(context))
      return;

    Object submittedValue;
    if (!wasSubmitted(context, component))
      submittedValue = null;
    else
      submittedValue = getSubmittedValue(context,
                                         component,
                                         component.getClientId(context));

    if (_LOG.isFinest())
    {
      _LOG.finest("Value submitted for ID {0} is {1}",
                  new Object[]{component.getClientId(context),
                               submittedValue});
    }

    EditableValueHolder evh = (EditableValueHolder) component;
    evh.setSubmittedValue(submittedValue);
  }

  protected Object getSubmittedValue(
    FacesContext context,
    UIComponent  component,
    String       clientId)
  {
    return context.getExternalContext().
                                getRequestParameterMap().get(clientId);
  }

  /**
   * Converts a string value into the component's value
   * @param context the FacesContext
   * @param component the component
   * @param newValue the unconverted string value
   */
  @Override
  public Object getConvertedValue(
    FacesContext context,
    UIComponent  component,
    Object       submittedValue) throws ConverterException
  {
    FacesBean bean = getFacesBean(component);
    Converter converter = getConverter(bean);
    if (converter == null)
      converter = getDefaultConverter(context, bean);

    if (converter != null)
    {
      return converter.getAsObject(context,
                                   component,
                                   submittedValue.toString());
    }

    return submittedValue;
  }

  /**
   * Override this method to return "false" if the component was
   * not actually submitted (if, for instance, it was disabled
   * or "read-only".
   */
  protected boolean wasSubmitted(
    FacesContext context,
    UIComponent  component)
  {
    FacesBean bean = getFacesBean(component);
    return !getDisabled(bean) && !getReadOnly(context, bean);
  }


  /**
   * All editable components need IDs.
   */
  @Override
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  /**
   * Override to include "submitted value".
   */
  @Override
  protected String getConvertedString(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    Object o = getSubmittedValue(bean);
    if (o != null)
      return o.toString();

    return super.getConvertedString(context, component, bean);
  }

  protected Object getSubmittedValue(FacesBean bean)
  {
    return bean.getProperty(_submittedValueKey);
  }



  protected void addOnSubmitConverterValidators(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean
  )throws IOException
  {

   // Bug 2748146: Don't do validation of a disabled field! If the field is
    // disabled, the user can't have updated it (there is one way for the
    // client to hurt themselves here: by changing the disabled state as part
    // of a PPR update after the user has updated the field).
    boolean disabled = getDisabled(bean);

    if (!disabled)
    {
      boolean requiredField = getRequired(bean);

      Converter converter = getConverter(bean);

      if ( converter == null)
        converter = getDefaultConverter(context, bean);

      Iterator<Validator> validators = getValidators(bean);

      if (requiredField ||
          (converter != null) ||
          validators.hasNext())
      {

        FormData fData = arc.getFormData();
        if (fData == null)
        {
          _LOG.warning("COMPONENT_REQUIRES_FORM", component);
          return;
        }

        boolean immediate = isImmediate(bean);
        ((CoreFormData) fData).addOnSubmitConverterValidators(component,
                                             converter,
                                             validators,
                                             getClientId(context, component),
                                             immediate,
                                             requiredField,
                                             getRequiredMessageKey());

      }
    }
  }

  protected String getRequiredMessageKey()
  {
    return UIXEditableValue.REQUIRED_MESSAGE_ID;
  }

  /**
   * @todo This will need to be cached!!!
   */
  protected boolean getReadOnly(FacesContext context, FacesBean bean)
  {
    Object o = bean.getProperty(_readOnlyKey);
    if (o == null)
      o = _readOnlyKey.getDefault();

    // If the component says it's read-only, it is.
    if (Boolean.TRUE.equals(o))
      return true;

    // Now, if the ValueExpression underlying the value says it's
    // read-only, then again, it is.
    ValueExpression ve = getValueExpression(bean);
    if ((ve != null) && ve.isReadOnly(context.getELContext()))
    {
      if (_LOG.isFiner())
      {
        _LOG.finer("Value expression {0} is read-only",
                   ve.getExpressionString());
      }

      return true;
    }

    return false;
  }

  protected boolean getDisabled(FacesBean bean)
  {
    Object o = bean.getProperty(_disabledKey);
    if (o == null)
      o = _disabledKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  protected boolean getRequired(FacesBean bean)
  {
    Object o = bean.getProperty(_requiredKey);
    if (o == null)
      o = _requiredKey.getDefault();

    return Boolean.TRUE.equals(o);
  }


  protected boolean isImmediate(FacesBean bean)
  {
    Object o = bean.getProperty(_immediateKey);
    if (o == null)
      o = _immediateKey.getDefault();

    return Boolean.TRUE.equals(o);
  }

  @SuppressWarnings("unchecked")
  protected Iterator<Validator> getValidators(FacesBean bean)
  {
    return (Iterator<Validator>)bean.entries(_validatorsKey);
  }

  private PropertyKey _submittedValueKey;
  private PropertyKey _readOnlyKey;
  private PropertyKey _disabledKey;
  private PropertyKey _requiredKey;
  private PropertyKey _immediateKey;
  private PropertyKey _validatorsKey;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    EditableValueRenderer.class);
}
