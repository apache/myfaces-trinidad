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
package org.apache.myfaces.trinidaddemo;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.model.DateListProvider;

public class GermanHolidayDateListProvider implements DateListProvider
{
  
  public GermanHolidayDateListProvider()
  {
    _germanHolidays = new ArrayList<Date>();
    _germanHolidays.add(_newDate("01.01.2007"));
    _germanHolidays.add(_newDate("06.04.2007"));
    _germanHolidays.add(_newDate("09.04.2007"));
    _germanHolidays.add(_newDate("01.05.2007"));
    _germanHolidays.add(_newDate("17.05.2007"));
    _germanHolidays.add(_newDate("07.06.2007"));
    _germanHolidays.add(_newDate("03.10.2007"));
    _germanHolidays.add(_newDate("01.11.2007"));
    _germanHolidays.add(_newDate("25.12.2007"));
    _germanHolidays.add(_newDate("26.12.2007"));
  }

  public List<Date> getDateList(FacesContext context, Calendar base,
      Date rangeStart, Date rangeEnd)
  {
    
    List<Date> returnDates = new ArrayList<Date>();
    
    for (Date it : _germanHolidays)
    {
      if(!it.before(rangeStart) && !it.after(rangeEnd)){
        base.setTime(it);
        returnDates.add(base.getTime());
      }
    }
    
    return returnDates;
  }

  private Date _newDate(String string)
  {
    SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy");
    Date ret = null;
    try
    {
      ret = sdf.parse(string);
    } catch (ParseException e)
    {
      e.printStackTrace();
    }
    return ret;
  }


  private List<Date> _germanHolidays = null;
}