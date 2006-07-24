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
package org.apache.myfaces.adfinternal.convert;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;

public final class NumberConverter extends org.apache.myfaces.adf.convert.NumberConverter
{
  public NumberConverter()
  {
  }
  
  public Object getAsObject(
    FacesContext context,
    UIComponent component,
    String value)
    throws ConverterException
  {
    Object number = super.getAsObject(context, component, value);
    if (number == null) // bug 4137626
      return null;

    // this is causing bug 4920160    
    number = 
      DateTimeConverter.__typeConvert(context, this, component, value, number);
    return number;
  }
  
  public String getAsString(
    FacesContext context, 
    UIComponent component,
    Object value)
    throws ConverterException 
  {
    if (value == null)
      return null;

    GenericConverterFactory fac = GenericConverterFactory.getCurrentInstance();
    // we support other types of numbers, like oracle.jbo.domain.Number:
    if ((!(value instanceof Number)) && fac.isConvertible(value, Number.class))
    {
      value = fac.convert(value, Number.class);
    }
    // bug 4214147:
    return super.getAsString(context, component, value);
  }
}
