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
package org.apache.myfaces.trinidadinternal.validator;
import java.util.Date;

import javax.faces.validator.ValidatorException;
import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidadinternal.convert.GenericConverterFactory;

public class DateTimeRangeValidator extends org.apache.myfaces.trinidad.validator.DateTimeRangeValidator
{
  public DateTimeRangeValidator()
  {
  }

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
}
