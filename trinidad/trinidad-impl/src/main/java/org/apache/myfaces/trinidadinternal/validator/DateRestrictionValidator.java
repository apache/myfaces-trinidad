package org.apache.myfaces.trinidadinternal.validator;
/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

import java.io.IOException;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.util.MessageFactory;
import org.apache.myfaces.trinidad.validator.ClientValidator;
import org.apache.myfaces.trinidadinternal.convert.GenericConverterFactory;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafUtils;
import org.apache.myfaces.trinidadinternal.util.JsonUtils;

/**
 * @author Matthias Wessendorf
 *
 */
public class DateRestrictionValidator extends org.apache.myfaces.trinidad.validator.DateRestrictionValidator
                                       implements ClientValidator
{
  public DateRestrictionValidator()
  {
    super();
    _initJsDateMap();
  }

  @Override
  public void validate(
    FacesContext context,
    UIComponent  component,
    Object       value) throws ValidatorException
  {
    if (value == null)
      return;
    
    if (!(value instanceof Date))
    {
      GenericConverterFactory fac = GenericConverterFactory.getCurrentInstance();
      value = fac.convert(value, Date.class);
    }
    super.validate(context, component, value);
  }
  
  public Collection<String> getClientImportNames()
  {
    return _IMPORT_NAMES;
  }

  public String getClientScript(
   FacesContext context,
   UIComponent component)
  {
    return null;
  }


  /**
   * @todo this should have not_in_range messages, not just max and min!
   * @todo Format these numbers properly.
   */
  public String getClientValidation(
    FacesContext context,
    UIComponent component)
  {
    String[] weekdaysValue = getInvalidDaysOfWeek();
    String weekdaysValues = null;
    StringBuilder sb1 = new StringBuilder();
    
    String[] monthValue = getInvalidMonths();
    String monthValues = null;
    StringBuilder sb2 = new StringBuilder();
    monthValues = sb2.toString();

    try
    {
      JsonUtils.writeObject(sb1, weekdaysValue, false);
      weekdaysValues = sb1.toString();
      JsonUtils.writeObject(sb2, monthValue, false);
      monthValues = sb2.toString();
    }
    catch (IOException e)
    {
      weekdaysValues  = "null";
      monthValues  = "null";
    }

    return _getTrDateRestrictionValidator(context, component, WEEKDAY_MESSAGE_ID, MONTH_MESSAGE_ID, VALIDATOR_ID, weekdaysValues, monthValues);
  }
  
  
  public String getClientLibrarySource(
   FacesContext context)
  {
    return null;
  }
  
  private static String _getTrDateRestrictionValidator(
      FacesContext context,
      UIComponent component,
      String weekId,
      String monthId,
      String defaultId,
      String weekdaysValues,
      String monthValues)
  {
    StringBuffer outBuffer = new StringBuffer(250);
    outBuffer.append("new TrDateRestrictionValidator(");

    FacesMessage weekMessage =
      MessageFactory.getMessage(context, weekId,
                                  new Object[]{"{0}", "{1}", "{2}"});

    outBuffer.append("{WV:'");
    outBuffer.append(XhtmlLafUtils.escapeJS(weekMessage.getDetail()));

    outBuffer.append("',WV_S:'");
    outBuffer.append(XhtmlLafUtils.escapeJS(weekMessage.getSummary()));    

    FacesMessage monthMessage =
      MessageFactory.getMessage(context, monthId,
                                  new Object[]{"{0}", "{1}", "{2}"});

    outBuffer.append("',MV:'");
    outBuffer.append(XhtmlLafUtils.escapeJS(monthMessage.getDetail()));
    outBuffer.append("',MV_S:'");
    outBuffer.append(XhtmlLafUtils.escapeJS(monthMessage.getSummary()));  
    
    FacesMessage defaultMessage =
      MessageFactory.getMessage(context, defaultId,
                                  new Object[]{"{0}", "{1}"});

    outBuffer.append("',D:'");
    outBuffer.append(XhtmlLafUtils.escapeJS(defaultMessage.getDetail())); 
    outBuffer.append("',D_S:'");
    outBuffer.append(XhtmlLafUtils.escapeJS(defaultMessage.getSummary()));
    outBuffer.append("'},null,0,");
    outBuffer.append(weekdaysValues);
    outBuffer.append(',');
    outBuffer.append(monthValues);
    outBuffer.append(',');
    outBuffer.append(_getMapAsJson(_jsWeekDays));
    outBuffer.append(',');
    outBuffer.append(_getMapAsJson(_jsMonths));
    outBuffer.append(")");

    return outBuffer.toString();
  }
  
  private static String _getMapAsJson(Map map)
  {
    StringBuilder sb = new StringBuilder();
    try
    {
      JsonUtils.writeMap(sb, map, false);
    }
    catch (IOException e)
    {
      sb.append("null");
    }
    return sb.toString();
  }
  
  private void _initJsDateMap()
  {
    _jsWeekDays = new HashMap<Integer, String>();
    _jsWeekDays.put(Calendar.SUNDAY-1, "sun");
    _jsWeekDays.put(Calendar.MONDAY-1, "mon");
    _jsWeekDays.put(Calendar.TUESDAY-1, "tue");
    _jsWeekDays.put(Calendar.WEDNESDAY-1, "wed");
    _jsWeekDays.put(Calendar.THURSDAY-1, "thu");
    _jsWeekDays.put(Calendar.FRIDAY-1, "fri");
    _jsWeekDays.put(Calendar.SATURDAY-1, "sat");
    
    _jsMonths = new HashMap<Integer, String>();
    _jsMonths.put(Calendar.JANUARY, "jan");
    _jsMonths.put(Calendar.FEBRUARY, "feb");
    _jsMonths.put(Calendar.MARCH, "mar");
    _jsMonths.put(Calendar.APRIL, "apr");
    _jsMonths.put(Calendar.MAY, "may");
    _jsMonths.put(Calendar.JUNE, "jun");
    _jsMonths.put(Calendar.JULY, "jul");
    _jsMonths.put(Calendar.AUGUST, "aug");
    _jsMonths.put(Calendar.SEPTEMBER, "sep");
    _jsMonths.put(Calendar.OCTOBER, "oct");
    _jsMonths.put(Calendar.NOVEMBER, "nov");
    _jsMonths.put(Calendar.DECEMBER, "dec");
  }
  
  private static final Collection<String> _IMPORT_NAMES = Collections.singletonList( "TrNumberConverter()" );
  private static Map<Integer, String> _jsMonths = null;
  private static Map<Integer, String> _jsWeekDays = null;
}