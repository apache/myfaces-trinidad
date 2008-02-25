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
package org.apache.myfaces.trinidaddemo.composite;

import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;

import javax.faces.application.FacesMessage;
import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.ConverterException;
import javax.faces.convert.NumberConverter;
import javax.faces.render.Renderer;
import javax.faces.validator.LongRangeValidator;

import org.apache.myfaces.trinidad.component.core.input.CoreInputText;

/**
 * An experiment in building a renderer that uses a composite
 * component strategy to render.  Some basic principles:
 * <ul>
 * <li> The child components get re-created on each pass through
 *    the system;  this means seeing if they exist in both Apply Request
 *    Values (<code>decode()</code>) and Render Response
 *    (<code>encodeBegin()</code>), and marking the components
 *    transient so they don't get saved.
 * <li> The model is the tricky part:  instead of using real
 *   <code>ValueBindings</code> on the children, I let them
 *   use local values, and then manully transfer over their local values
 *   into an overall "local value" during validate().  Unfortunately,
 *   using ValueBindings to automate the transfer wouldn't quite work,
 *   since the transfer wouldn't happen 'til Update Model, which is
 *   too late to preserve the semantics of an editable value component in JSF.
 * </ul>
 */
public class DateFieldAsRenderer extends Renderer
{
  @SuppressWarnings("unchecked")
  @Override
  public void decode(FacesContext context, UIComponent component)
  {
    _addChildren(context, component);

    Map<String, Object> attrs = component.getAttributes();
    if (Boolean.TRUE.equals(attrs.get("readOnly")) ||
        Boolean.TRUE.equals(attrs.get("disabled")))
      return;

    // Just clue in component that we have been "submitted" so
    // that it doesn't short-circuit anything
    EditableValueHolder evh = (EditableValueHolder) component;
    evh.setSubmittedValue(Boolean.TRUE);

    // Because these components weren't around during processDecodes(),
    // they didn't get decoded.  So, run that now.
    component.getFacet("month").processDecodes(context);
    component.getFacet("year").processDecodes(context);
    component.getFacet("day").processDecodes(context);
  }

  @Override
  public Object getConvertedValue(
    FacesContext context,
    UIComponent  component,
    Object       submittedValue)
  {
    EditableValueHolder monthComp = (EditableValueHolder) component.getFacet("month");
    EditableValueHolder yearComp = (EditableValueHolder) component.getFacet("year");
    EditableValueHolder dayComp = (EditableValueHolder) component.getFacet("day");

    if (!monthComp.isValid() ||
        !yearComp.isValid() ||
        !dayComp.isValid())
    {
      // =-=AEW What to do????????
      //setValid(false);
      return null;
    }

    int year = ((Number) yearComp.getValue()).intValue();
    // We'll be 1970 - 2069.  Good enough for a demo.
    if (year < 70)
      year += 100;

    int month = ((Number) monthComp.getValue()).intValue() - 1;
    int day = ((Number) dayComp.getValue()).intValue();

    Date oldValue = (Date) ((EditableValueHolder) component).getValue();
    //Date newValue = (Date) oldValue.clone();
    Calendar calendar = Calendar.getInstance();
    calendar.setLenient(true);
    calendar.setTime(oldValue);
    calendar.set(Calendar.YEAR, year);
    calendar.set(Calendar.MONTH, month);
    calendar.set(Calendar.DAY_OF_MONTH, day);
    
    // Invalid day given the month
    if (day != calendar.get(Calendar.DAY_OF_MONTH))
    {
      int numberOfDaysInMonth = day - calendar.get(Calendar.DAY_OF_MONTH);
      FacesMessage message = new FacesMessage(FacesMessage.SEVERITY_ERROR,
                    "Invalid date.",
                    "This month only has " + numberOfDaysInMonth + " days!");
      throw new ConverterException(message);
    }

    return calendar.getTime();
  }

  @Override
  public void encodeBegin(FacesContext context,
                          UIComponent component) throws IOException
  {
    _addChildren(context, component);
  }

  @Override
  public void encodeChildren(FacesContext context,
                             UIComponent component) throws IOException
  {
    ResponseWriter out = context.getResponseWriter();

    UIComponent month = component.getFacet("month");
    month.encodeBegin(context);
    month.encodeChildren(context);
    month.encodeEnd(context);

    out.writeText("\u00a0/\u00a0", null);

    UIComponent day = component.getFacet("day");
    day.encodeBegin(context);
    day.encodeChildren(context);
    day.encodeEnd(context);

    out.writeText("\u00a0/\u00a0", null);

    UIComponent year = component.getFacet("year");
    year.encodeBegin(context);
    year.encodeChildren(context);
    year.encodeEnd(context);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @SuppressWarnings("unchecked")
  private void _addChildren(FacesContext context, UIComponent component)
  {
    // If the components are already there, bail.
    if (component.getFacet("month") != null)
      return;

    String id = component.getId();
    if (id == null)
    {
      id = context.getViewRoot().createUniqueId();
      component.setId(id);
    }

    Map<String, UIComponent> facets = component.getFacets();
    facets.clear();

    Date value = (Date) ((EditableValueHolder) component).getValue();
    Calendar calendar = null;
    if(value != null)
    {
      calendar = Calendar.getInstance();
      calendar.setLenient(true);
      calendar.setTime(value);
    }

    CoreInputText month = _createTwoDigitInput(context);
    month.setShortDesc("Month");
    month.setId(id + "_month");

    LongRangeValidator monthRange = _createLongRangeValidator(context);
    monthRange.setMinimum(1);
    monthRange.setMaximum(12);
    month.addValidator(monthRange);
    if (value != null)
      month.setValue(new Integer(calendar.get(Calendar.MONTH) + 1));
    facets.put("month", month);

    CoreInputText day = _createTwoDigitInput(context);
    day.setShortDesc("Day");
    day.setId(id + "_day");
    LongRangeValidator dayRange = _createLongRangeValidator(context);
    dayRange.setMinimum(1);
    dayRange.setMaximum(31);
    day.addValidator(dayRange);
    if (value != null)
      day.setValue(new Integer(calendar.get(Calendar.DAY_OF_MONTH)));
    facets.put("day", day);

    CoreInputText year = _createTwoDigitInput(context);
    year.setShortDesc("Year");
    year.setId(id + "_year");
    if (value != null)
    {
      int yearValue = calendar.get(Calendar.YEAR) - 1900;
      if (yearValue >= 100)
        yearValue -= 100;
      year.setValue(new Integer(yearValue));
    }

    facets.put("year", year);
  }

  private LongRangeValidator _createLongRangeValidator(FacesContext context)
  {
    return (LongRangeValidator)
      context.getApplication().createValidator(LongRangeValidator.VALIDATOR_ID);
  }

  private CoreInputText _createTwoDigitInput(FacesContext context)
  {
    CoreInputText input = new CoreInputText();
    input.setColumns(2);
    input.setMaximumLength(2);
    input.setTransient(true);
    input.setRequired(true);
    input.setSimple(true);

    NumberConverter converter = (NumberConverter)
      context.getApplication().createConverter(NumberConverter.CONVERTER_ID);
    converter.setIntegerOnly(true);
    converter.setMaxIntegerDigits(2);
    converter.setMinIntegerDigits(2);
    input.setConverter(converter);

    return input;
  }
}
