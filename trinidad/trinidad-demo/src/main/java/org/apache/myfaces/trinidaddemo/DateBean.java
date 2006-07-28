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
package org.apache.myfaces.trinidaddemo;
import java.util.Date;

public class DateBean implements java.io.Serializable
{
  public DateBean()
  {

    _date1 = new Date();
    _date2 = new Date();
    _date3 = new Date();
    _date4 = new Date();
    _minDate = new Date(System.currentTimeMillis() - 24 * 60 * 60 * 1000);
    _maxDate = new Date();
  }

  public String action()
  {
    return "success";
  }

  public Date getDate1()
  {
    return _date1;
  }

  public void setDate1(Date date)
  {
    _date1 = date;
  }

  public Date getDate2()
  {
    return _date2;
  }

  public void setDate2(Date date)
  {
    _date2 = date;
  }

  public Date getDate3()
  {
    return _date3;
  }

  public void setDate3(Date date)
  {
    _date3 = date;
  }

  public Date getDate4()
  {
    return _date4;
  }

  public void setDate4(Date date)
  {
    _date4 = date;
  }

  public void setMinDate(Date minDate)
  {
    _minDate = minDate;
  }

   public Date getMinDate()
  {
    return _minDate;
  }

  public void setMaxDate(Date maxDate)
  {
    _maxDate = maxDate;
  }

   public Date getMaxDate()
  {
    return _maxDate;
  }

  public Date getCurrentDate()
  {
    return new Date();
  }

  public void setCurrentDate(Date date)
  {
    return;
  }

  private Date _date1;
  private Date _date2;
  private Date _date3;
  private Date _date4;
  private Date _minDate;
  private Date _maxDate;

}
