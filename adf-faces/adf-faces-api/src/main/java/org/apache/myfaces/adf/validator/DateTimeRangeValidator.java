/*
 * Copyright  2004-2006 The Apache Software Foundation.
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

package org.apache.myfaces.adf.validator;

import java.util.Date;

import javax.faces.application.FacesMessage;
import javax.faces.component.StateHolder;
import javax.faces.component.UIComponent;
import javax.faces.component.ValueHolder;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.DateTimeConverter;
import javax.faces.el.ValueBinding;
import javax.faces.validator.Validator;
import javax.faces.validator.ValidatorException;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.PropertyKey;
import org.apache.myfaces.adf.util.ComponentUtils;
import org.apache.myfaces.adf.util.MessageFactory;


/**
 * <p><strong>DateTimeRangeValidator</strong> is a {@link Validator} that checks
 * the value of the corresponding component against specified minimum and
 * maximum dates.  The following algorithm is implemented:</p>
 * <ul>
 * <li>If the passed value is <code>null</code>, exit immediately.</li>
 * <li>If both a <code>maximum</code> and <code>minimum</code> property
 *     has been configured on this {@link Validator}, check the component
 *     value against both limits.  If the component value is not within
 *     this specified range, throw a {@link ValidatorException} containing a
 *     {@link Validator#NOT_IN_RANGE_MESSAGE_ID} message.</li>
 * <li>If a <code>maximum</code> property has been configured on this
 *     {@link Validator}, check the component value against
 *     this limit.  If the component value is greater than the
 *     specified maximum, throw a {@link ValidatorException} containing a
 *     MAXIMUM_MESSAGE_ID message.</li>
 * <li>If a <code>minimum</code> property has been configured on this
 *     {@link Validator}, check the component value against
 *     this limit.  If the component value is less than the
 *     specified minimum, throw a {@link ValidatorException} containing a
 *     MINIMUM_MESSAGE_ID message.</li>
 * </ul>
 * <p>The detail part of faces message which arise during validation can be
 * customised by overriding the message associated with each message id by calling
 * appropriate setter methods.</p>
 * <p>The methods used for customizing the detail message associated with each id
 * is given below:</p>
 * <ul>
 * <li>{@link #MAXIMUM_MESSAGE_ID} - {@link #setMaximumMessageDetail(String)}</li>
 * <li>{@link #MINIMUM_MESSAGE_ID} - {@link #setMinimumMessageDetail(String)}</li>
 * <li>{@link #NOT_IN_RANGE_MESSAGE_ID} - {@link #setNotInRangeMessageDetail(String)} - </li></ul>
 *  Then this message will be used to construct faces message
 *  when validation fails based on the above-mentioned algorithm

 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/validator/DateTimeRangeValidator.java#0 $) $Date: 10-nov-2005.19:08:33 $
 * @author The Oracle ADF Faces Team
 * @todo The error message date/time reads
 * "Date cannot be before Mon Feb 16 16:11:13 PST 2004", in a perfect the
 * date should probably be in the format of the converter....
 */
public class DateTimeRangeValidator implements Validator, StateHolder {


  public static final String VALIDATOR_ID = "org.apache.myfaces.adf.DateTimeRange";


  /**
   * <p>The message identifier of the {@link javax.faces.application.FacesMessage}
   * to be created if the maximum value check fails.  The message format
   * string for this message may optionally include <code>{0}</code>,
   * <code>{1}</code> and <code>{3}</code> placeholders,
   * which will be replaced by user input, component label and configured
   * maximum value.</p>
   */
  public static final String MAXIMUM_MESSAGE_ID =
      "org.apache.myfaces.adf.validator.DateTimeRangeValidator.MAXIMUM";

  /**
   * <p>The message identifier of the {@link javax.faces.application.FacesMessage}
   * to be created if the minimum value check fails.  The message format
   * string for this message may optionally include <code>{0}</code>,
   * <code>{1}</code> and <code>{2}</code> placeholders, which will be replaced
   * by user input, component label and configured minimum value.</p>
   */
  public static final String MINIMUM_MESSAGE_ID =
      "org.apache.myfaces.adf.validator.DateTimeRangeValidator.MINIMUM";


  /**
   * <p>The message identifier of the {@link javax.faces.application.FacesMessage}
   * to be created if the maximum or minimum value check fails, and both
   * the maximum and minimum values for this validator have been set.
   * The message format string for this message may optionally include
   * <code>{0}</code>, <code>{1}</code>, <code>{2}</code> and <code>{3}</code>
   * placeholders, which will be replaced by user input, component label,
   * configured minimum value and configured maximum value.</p>
   */
  public static final String NOT_IN_RANGE_MESSAGE_ID =
      "org.apache.myfaces.adf.validator.DateTimeRangeValidator.NOT_IN_RANGE";


  /**
   * Construct a {@link Validator} with no preconfigured limits.
   */
  public DateTimeRangeValidator()
  {
    super();
  }

  /**
   * Construct a {@link Validator} with the specified preconfigured
   * limit.
   *
   * @param maximum Maximum value to allow
   */
  public DateTimeRangeValidator(Date maximum)
  {
    super();
    setMaximum(maximum);
  }

  /**
   * Construct a {@link Validator} with the specified preconfigured
   * limits.
   *
   * @param maximum Maximum value to allow
   * @param minimum Minimum value to allow
   *
   */
  public DateTimeRangeValidator(Date maximum, Date minimum)
  {
    super();
    setMaximum(maximum);
    setMinimum(minimum);
  }

  /**
   * Return the maximum value to be enforced by this {@link
   * Validator} or null if it has not been
   * set.
   */
  public Date getMaximum()
  {
    Object maxDate = _facesBean.getProperty(_MAXIMUM_KEY);
    return ComponentUtils.resolveDate(maxDate);
  }

  /**
   * Set the maximum value to be enforced by this {@link Validator}.
   *
   * @param maximum The new maximum value
   *
   */
  public void setMaximum(Date maximum)
  {
    _facesBean.setProperty(_MAXIMUM_KEY, maximum);
  }


  /**
   * Return the minimum value to be enforced by this {@link
   * Validator}, or null if it has not been
   * set.
   */
  public Date getMinimum()
  {
    Object minDate = _facesBean.getProperty(_MINIMUM_KEY);
    return ComponentUtils.resolveDate(minDate);
  }

  /**
   * Set the minimum value to be enforced by this {@link Validator}.
   *
   * @param minimum The new minimum value
   *
   */
  public void setMinimum(Date minimum)
  {
    _facesBean.setProperty(_MINIMUM_KEY, minimum);
  }

  /**
   * <p>Custom error message to be used, for creating detail part of the
   * {@link FacesMessage}, when input value exceeds the maximum value set.</p>
   * Overrides detail message identified by message id {@link #MAXIMUM_MESSAGE_ID}
   * @param maximumMessageDetail Custom error message.
   */
  public void setMaximumMessageDetail(String maximumMessageDetail)
  {
    _facesBean.setProperty(_MAXIMUM_MESSAGE_DETAIL_KEY, maximumMessageDetail);
  }

  /**
   *  <p>Return custom detail error message that was set for creating {@link FacesMessage},
   *  for cases where input value exceeds the <code>maximum</code> value set.</p>
   * @return Custom error message.
   * @see #setMaximumMessageDetail(String)
   */
  public String getMaximumMessageDetail()
  {
    Object maxMsgDet = _facesBean.getProperty(_MAXIMUM_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(maxMsgDet);
  }

  /**
   * <p>Custom error message to be used, for creating detail part of the
   * {@link FacesMessage}, when input value is less the set
   * <code>minimum</code> value.</p>
   * Overrides detail message identified by message id {@link #MINIMUM_MESSAGE_ID}
   * @param minimumMessageDetail Custom error message.
   */
  public void setMinimumMessageDetail(String minimumMessageDetail)
  {
    _facesBean.setProperty(_MINIMUM_MESSAGE_DETAIL_KEY, minimumMessageDetail);
  }

  /**
   * <p>Return custom detail error message that was set for creating {@link FacesMessage},
   * for cases where, input value is less than the <code>minimum</code> value set.</p>
   * @return Custom error message.
   * @see #setMinimumMessageDetail(String)
   */
  public String getMinimumMessageDetail()
  {
    Object minMsgDet = _facesBean.getProperty(_MINIMUM_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(minMsgDet);
  }

  /**
   * <p>Custom error message to be used, for creating detail part of the
   * {@link FacesMessage}, when input value is not with in the range,
   * when <code>minimum</code> and <code>maximum</code> is set.</p>
   * Overrides detail message identified by message id {@link #NOT_IN_RANGE_MESSAGE_ID}
   * @param notInRangeMessageDetail Custom error message.
   */
  public void setNotInRangeMessageDetail(String notInRangeMessageDetail)
  {
    _facesBean.setProperty(_NOT_IN_RANGE_MESSAGE_DETAIL_KEY, notInRangeMessageDetail);
  }

  /**
   * <p>Return custom detail error message that was set for creating {@link FacesMessage},
   * for cases where, input value exceeds the <code>maximum</code> value and is
   * less than the <code>minimum</code> value set.</p>
   * @return Custom error message.
   * @see #setNotInRangeMessageDetail(String)
   */
  public String getNotInRangeMessageDetail()
  {
    Object notInRngMsg = _facesBean.getProperty(_NOT_IN_RANGE_MESSAGE_DETAIL_KEY);
    return ComponentUtils.resolveString(notInRngMsg);
  }

  /**
   * @exception NullPointerException {@inheritDoc}
   * @exception ValidatorException {@inheritDoc}
   * @exception IllegalArgumentException if <code>value</code> is not of type
   * {@link java.util.Date}
   */
  public void validate(
    FacesContext context,
    UIComponent  component,
    Object       value) throws ValidatorException
  {
    if ((context == null) || (component == null))
    {
      throw new NullPointerException();
    }

    if (value != null)
    {

      Date converted = _getDateValue(value);

      // even after this values can change. But this should be very remote.
      Date max = getMaximum();
      Date min = getMinimum();
      if (max != null && (converted.after(max)))
      {
        if (min != null)
        {
           throw new ValidatorException
                      (_getNotInRangeMessage(context, component, value, min, max));
        }
        else
        {
           throw new ValidatorException
                      (_getMaximumMessage(context, component, value, max));
        }
      }

      if (min != null && (converted.before(min)))
      {
        if (max != null)
        {
          throw new ValidatorException
                      (_getNotInRangeMessage(context, component, value, min, max));
        }
        else
        {
          FacesMessage msg = _getMinimumMessage(context, component, value, min);
          throw new ValidatorException(msg);
        }
      }
    }
  }



  //  StateHolder Methods

  public Object saveState(FacesContext context)
  {
    return _facesBean.saveState(context);
  }


  public void restoreState(FacesContext context, Object state)
  {
    _facesBean.restoreState(context, state);
  }

  /**
   * <p>Set the {@link ValueBinding} used to calculate the value for the
   * specified attribute if any.</p>
   *
   * @param name Name of the attribute for which to set a {@link ValueBinding}
   * @param binding The {@link ValueBinding} to set, or <code>null</code>
   *  to remove any currently set {@link ValueBinding}
   *
   * @exception NullPointerException if <code>name</code>
   *  is <code>null</code>
   * @exception IllegalArgumentException if <code>name</code> is not a valid
   *            attribute of this validator
   */
  public void setValueBinding(String name, ValueBinding binding)
  {
    ValidatorUtils.setValueBinding(_facesBean, name, binding) ;
  }

  /**
   * <p>Return the {@link ValueBinding} used to calculate the value for the
   * specified attribute name, if any.</p>
   *
   * @param name Name of the attribute or property for which to retrieve a
   *  {@link ValueBinding}
   *
   * @exception NullPointerException if <code>name</code>
   *  is <code>null</code>
   * @exception IllegalArgumentException if <code>name</code> is not a valid
   * attribute of this validator
   */
  public ValueBinding getValueBinding(String name)
  {
    return ValidatorUtils.getValueBinding(_facesBean, name);
  }

  public boolean equals(Object o)
  {
    if ( o instanceof DateTimeRangeValidator)
    {
      DateTimeRangeValidator that = (DateTimeRangeValidator)o;

      if ( _transientValue == that._transientValue &&
           (ValidatorUtils.equals(getMinimum(), that.getMinimum())) &&
           (ValidatorUtils.equals(getMaximum(), that.getMaximum())) &&
           (ValidatorUtils.equals(getMaximumMessageDetail(),
                                   that.getMaximumMessageDetail())) &&
           (ValidatorUtils.equals(getMinimumMessageDetail(),
                                   that.getMinimumMessageDetail())) &&
           (ValidatorUtils.equals(getNotInRangeMessageDetail(),
                                   that.getNotInRangeMessageDetail()))
          )
      {
        return true;
      }
    }
    return false;
  }

  public int hashCode()
  {
    int result = 17;
    Object max = getMaximum();
    Object min = getMinimum();
    Object maxMsgDet        =  getMaximumMessageDetail();
    Object minMsgDet        =  getMinimumMessageDetail();
    Object notInRangeMsgDet =  getNotInRangeMessageDetail();

    result = 37 * result + ( max == null ? 0 : max.hashCode());
    result = 37 * result + ( min == null ? 0 : min.hashCode());
    result = 37 * result + ( _transientValue ? 0 : 1);
    result = 37 * result + ( maxMsgDet == null ? 0: maxMsgDet.hashCode());
    result = 37 * result + ( minMsgDet == null ? 0: minMsgDet.hashCode());
    result = 37 * result + ( notInRangeMsgDet == null ? 0: notInRangeMsgDet.hashCode());
    return result;
  }

  public boolean isTransient()
  {
    return (_transientValue);
  }


  public void setTransient(boolean transientValue)
  {
    _transientValue = transientValue;
  }

  private static Date _getDateValue(
    Object value) throws IllegalArgumentException
  {
    if (value instanceof Date)
    {
      return ( (Date)value );
    }

    throw new IllegalArgumentException("'value' is not of type java.util.Date");
  }

  private FacesMessage _getNotInRangeMessage(
    FacesContext context,
    UIComponent component,
    Object value,
    Object min,
    Object max)
  {
    Converter converter = _getConverter(context, component);

    Object cValue = _getConvertedValue(context, component, converter, value);
    Object cMin   = _getConvertedValue(context, component, converter, min);
    Object cMax   = _getConvertedValue(context, component, converter, max);

    Object msg   = _getRawNotInRangeMessageDetail();
    Object label = ValidatorUtils.getComponentLabel(component);

    Object[] params = {label, cValue, cMin, cMax};

    return MessageFactory.getMessage(context, NOT_IN_RANGE_MESSAGE_ID,
                                      msg, params, component);
  }


  
  private Object _getRawNotInRangeMessageDetail()
  {
    return _facesBean.getRawProperty(_NOT_IN_RANGE_MESSAGE_DETAIL_KEY);
  }


  private FacesMessage _getMaximumMessage(
    FacesContext context,
    UIComponent component,
    Object value,
    Object max)
  {
    Converter converter = _getConverter(context, component);

    Object cValue = _getConvertedValue(context, component, converter, value);
    Object cMax   = _getConvertedValue(context, component, converter, max);

    Object msg   = _getRawMaximumMessageDetail();
    Object label = ValidatorUtils.getComponentLabel(component);

    Object[] params = {label, cValue, cMax};

    return MessageFactory.getMessage(context,
                                     MAXIMUM_MESSAGE_ID,
                                     msg,
                                     params,
                                     component);
  }

  private Object _getRawMaximumMessageDetail()
  {
    return _facesBean.getRawProperty(_MAXIMUM_MESSAGE_DETAIL_KEY);
  }

  private FacesMessage _getMinimumMessage(
    FacesContext context,
    UIComponent component,
    Object value,
    Object min)
  {
    Converter converter = _getConverter(context, component);

    Object cValue = _getConvertedValue(context, component, converter, value);
    Object cMin   = _getConvertedValue(context, component, converter, min);


    Object msg      = _getRawMinimumMessageDetail();
    Object label    = ValidatorUtils.getComponentLabel(component);

    Object[] params = {label, cValue, cMin};

    return MessageFactory.getMessage(context, MINIMUM_MESSAGE_ID,
                                     msg, params, component);
  }

  private Object _getRawMinimumMessageDetail()
  {
    return _facesBean.getRawProperty(_MINIMUM_MESSAGE_DETAIL_KEY);
  }

  private Converter _getConverter(
    FacesContext context,
    UIComponent component)
  {
    Converter converter = null;
    if (component instanceof ValueHolder)
    {
      converter = ((ValueHolder) component).getConverter();
    }

    if (converter == null)
    {
      // Use the DateTimeConverter's CONVERTER_ID, not Date.class,
      // because there is in fact not usually a converter registered
      // at Date.class
      converter = context.getApplication().createConverter(
                      DateTimeConverter.CONVERTER_ID);
    }

    assert(converter != null);

    return converter;
  }


  private Object _getConvertedValue(
    FacesContext context,
    UIComponent  component,
    Converter    converter,
    Object       value)
  {
    return converter.getAsString(context, component, value);
  }

  private static final FacesBean.Type _TYPE = new FacesBean.Type();

  private static final PropertyKey _MINIMUM_KEY =
    _TYPE.registerKey("minimum", Date.class);

  private static final PropertyKey _MAXIMUM_KEY =
    _TYPE.registerKey("maximum", Date.class );

  private static final PropertyKey _MAXIMUM_MESSAGE_DETAIL_KEY =
    _TYPE.registerKey("maximumMessageDetail", String.class);

  private static final PropertyKey _MINIMUM_MESSAGE_DETAIL_KEY =
    _TYPE.registerKey("minimumMessageDetail", String.class);

  private static final PropertyKey _NOT_IN_RANGE_MESSAGE_DETAIL_KEY =
    _TYPE.registerKey("notInRangeMessageDetail", String.class);

  private FacesBean _facesBean = ValidatorUtils.getFacesBean(_TYPE);

  private boolean _transientValue = false;

}
