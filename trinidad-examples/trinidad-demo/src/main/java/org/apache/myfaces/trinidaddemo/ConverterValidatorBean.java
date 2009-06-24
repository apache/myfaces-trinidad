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

import java.io.Serializable;

import java.math.BigDecimal;

import javax.faces.event.ValueChangeEvent;

public class ConverterValidatorBean implements Serializable
{
  public ConverterValidatorBean()
  {
  }

  public Number getCurrencyValue()
  {
    return _currencyValue;
  }

  public void setCurrencyValue(Number value)
  {
    _currencyValue = value;
  }

  public Number getIntegerOnlyValue()
  {
    return _intOnlyValue;
  }

  public void setIntegerOnlyValue(Number value)
  {
    _intOnlyValue =  value;
  }

  public void setPercentValue(Number value)
  {
    _percentValue = value;
  }

  public Number getPercentValue()
  {
    return _percentValue;
  }

  public Number getGroupValue()
  {
    return _groupValue;
  }

  public void setGroupValue(Number value)
  {
    _groupValue = value;
  }
  
  public void setBigDecimalValue(BigDecimal bigDecimalValue)
  {
    _bigDecimalValue = bigDecimalValue;
  }

  public BigDecimal getBigDecimalValue()
  {
    return _bigDecimalValue;
  }

  public void valueChanged(ValueChangeEvent vce)
  {
    System.out.println("valueChangeListener called.");
    System.out.println("   Old value = " + vce.getOldValue());
    System.out.println("   New value = " + vce.getNewValue());
  }

  private Number _currencyValue = new Double(78.57);

  private Number _intOnlyValue = new Double(99.99);

  private Number _percentValue = new Double(0.55);

  private Number _groupValue   = new Double(77777.89);

  private BigDecimal _bigDecimalValue   = new BigDecimal(2.00);

}
