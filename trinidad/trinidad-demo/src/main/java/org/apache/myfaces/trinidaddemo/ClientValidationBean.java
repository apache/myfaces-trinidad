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
import java.math.BigDecimal;



public class ClientValidationBean implements java.io.Serializable
{

  public ClientValidationBean()
  {
  }

  public BigDecimal getBigDecimal()
  {
    return _bigDecimal ;
  }

  public void setBigDecimal(BigDecimal bigDecimal)
  {
    _bigDecimal = bigDecimal;
  }


  public Integer getInteger()
  {
    return _integer ;
  }

  public void setInteger(Integer integer)
  {
    _integer = integer;
  }

  public Short getShort()
  {
    return _short ;
  }

  public void setShort(Short shortObj)
  {
    _short = shortObj;
  }

  public Byte getByte()
  {
    return _byte ;
  }

  public void setByte(Byte byteObj)
  {
    _byte = byteObj;
  }

  public Long getLong()
  {
    return _long ;
  }

  public void setLong(Long longObj)
  {
    _long = longObj;
  }

  public Long getLongValue()
  {
    return _longValue ;
  }

  public void setLongValue(Long longObj)
  {
    _longValue = longObj;
  }

  public Float getFloat()
  {
    return _float ;
  }

  public void setFloat(Float floatObj)
  {
    _float = floatObj;
  }

  public Double getDouble()
  {
    return _double ;
  }

  public void setDouble(Double doubleObj)
  {
    _double = doubleObj;
  }


  public String getText()
  {
    return _text ;
  }

  public void setText(String text)
  {
    _text = text;
  }

  public Integer getSsn()
  {
    return _ssn ;
  }

  public void setSsn(Integer ssn)
  {
    _ssn = ssn;
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


  private Integer _ssn = null;
  private String _text = "This is bound text";
  private Integer _integer = null;
  private BigDecimal _bigDecimal = null;
  private Long _long = null;
  private Long _longValue = null;
  private Short _short = null;
  private Byte _byte = null;
  private Double _double = null;
  private Float _float = null;

  private String _regExpValue = null;

  private String _byteLegthValue = null;


}

