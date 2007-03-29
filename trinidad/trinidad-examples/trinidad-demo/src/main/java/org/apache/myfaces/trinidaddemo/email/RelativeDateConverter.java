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
package org.apache.myfaces.trinidaddemo.email;

import java.text.DateFormat;
import java.text.MessageFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;
import javax.faces.component.UIComponent;
import javax.faces.convert.Converter;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RequestContext;

public class RelativeDateConverter implements Converter
{
  public RelativeDateConverter()
  {
  }

  public String getAsString(FacesContext context, UIComponent component,
                            Object value)
  {
    if (value == null)
      return null;

    if (!(value instanceof Date))
      return value.toString();

    Date date = (Date) value;

    RequestContext afContext = RequestContext.getCurrentInstance();
    TimeZone tz = afContext.getTimeZone();
    if (tz == null)
      tz = TimeZone.getDefault();

    Locale locale = context.getViewRoot().getLocale();
    if (_isToday(date, tz, locale))
    {
      DateFormat format = DateFormat.getTimeInstance(DateFormat.SHORT,
                                                     locale);
      String dateString = format.format(date);
      String todayMask = MessageUtils.getString(context, "TODAY_MASK");
      return MessageFormat.format(todayMask, new Object[]{dateString});
    }
    else
    {
      DateFormat format = DateFormat.getDateTimeInstance(DateFormat.SHORT,
                                                         DateFormat.SHORT,
                                                         locale);
      return format.format(date);
    }
  }

  public Object getAsObject(FacesContext context, UIComponent component,
                            String value)
  {
    throw new UnsupportedOperationException();
  }

  static private boolean _isToday(Date date, TimeZone tz, Locale locale)
  {
    Calendar calendar = Calendar.getInstance(tz, locale);
    calendar.setTime(date);

    int year = calendar.get(Calendar.YEAR);
    int dayOfYear = calendar.get(Calendar.DAY_OF_YEAR);

    calendar.setTime(new Date());
    if ((year == calendar.get(Calendar.YEAR)) &&
        (dayOfYear == calendar.get(Calendar.DAY_OF_YEAR)))
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}
