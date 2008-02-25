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

import java.util.Date;

import org.apache.myfaces.trinidad.model.DateListProvider;

public class DateRestrictionValidatorBean
{

  public String getCountry()
  {
    return _country;
  }
  public void setCountry(String country)
  {
    this._country = country;
  }
  public DateListProvider getNationalHolidays()
  {
    return _nationalHolidays;
  }
  public void setNationalHolidays(DateListProvider nationalHolidays)
  {
    this._nationalHolidays = nationalHolidays;
  }
  public Date getTestInvalidDays()
  {
    return _testInvalidDays;
  }
  public void setTestInvalidDays(Date testInvalidDays)
  {
    this._testInvalidDays = testInvalidDays;
  }
  public Date getTestInvalidDaysOfWeek()
  {
    return _testInvalidDaysOfWeek;
  }
  public void setTestInvalidDaysOfWeek(Date testInvalidDaysOfWeek)
  {
    this._testInvalidDaysOfWeek = testInvalidDaysOfWeek;
  }
  public Date getTestInvalidMonth()
  {
    return _testInvalidMonth;
  }
  public void setTestInvalidMonth(Date testInvalidMonth)
  {
    this._testInvalidMonth = testInvalidMonth;
  }
  
  private DateListProvider _nationalHolidays = null;
  private String _country = null;
  private Date _testInvalidDays = null;
  private Date _testInvalidDaysOfWeek = null;
  private Date _testInvalidMonth = null;
  
}