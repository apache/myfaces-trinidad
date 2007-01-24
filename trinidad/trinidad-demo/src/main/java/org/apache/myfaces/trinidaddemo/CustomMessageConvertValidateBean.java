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

import java.awt.Color;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

public class CustomMessageConvertValidateBean implements java.io.Serializable
{
  public CustomMessageConvertValidateBean()
  {
    _date1 = new Date();
    _date2 = new Date();
    _minDate = new Date(System.currentTimeMillis() - 24 * 60 * 60 * 1000);
    _maxDate = new Date();

    _messages.put("DOB_REQD_ID","You should specify \"{0}\" to apply for license");
    _messages.put("PIN_NO_MATCH_ID","Value \"{1}\" is not a valid \"{0}\". It should be a valid three digit number, first digit not being zero. The expected pattern is \"{2}\".");
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

  public String getRegExpValue()
  {
    return _regExpValue;
  }

  public void setRegExpValue(String regExpValue)
  {
    _regExpValue = regExpValue;
  }

   public String getByteLengthValue()
  {
    return _byteLegthValue;
  }

  public void setByteLengthValue(String value)
  {
    _byteLegthValue = value;
  }

  public Color getColor()
  {
    return _color;
  }

  public void setColor(Color colorValue)
  {
    _color = colorValue;
  }

  public Map<String, String> getMessages()
  {
    return _messages;
  }

  public String getMinDateTip()
  {
    if (_dft instanceof SimpleDateFormat)
    {
      ((SimpleDateFormat)_dft).applyPattern("M/d/yyyy");
    }
    return _dft.format(_minDate);
  }

  public String getMaxDateTip()
  {
    if (_dft instanceof SimpleDateFormat)
    {
      ((SimpleDateFormat)_dft).applyPattern("M/d/yyyy");
    }
    return _dft.format(_maxDate);
  }


  private DateFormat _dft = DateFormat.getDateInstance();

  private Map<String, String> _messages = new HashMap<String, String>();

  private Color _color = new Color(255,0,0);

  private String _byteLegthValue = null;

  private String _regExpValue = null;

  private Date _date1;

  private Date _date2;

  private Date _minDate;

  private Date _maxDate;

}
