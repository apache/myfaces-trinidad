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
import java.util.Collection;
import java.util.Collections;
import java.util.Date;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;

import org.apache.myfaces.trinidad.validator.ClientValidator;
import org.apache.myfaces.trinidadinternal.convert.GenericConverterFactory;

public class DateTimeRangeValidator extends org.apache.myfaces.trinidad.validator.DateTimeRangeValidator
                                       implements ClientValidator
{
  public DateTimeRangeValidator()
  {
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
    Date max = getMaximum();
    Date min = getMinimum();
    String maxStr = (max == null) ? null : Long.toString(max.getTime());
    String minStr = (min == null) ? null : Long.toString(min.getTime());
    
    return _getTrDateTimeRangeValidator(context, component, maxStr, minStr);
    
  }
  
  
  public String getClientLibrarySource(
   FacesContext context)
  {
    return null;
  }
  
  private static String _getTrDateTimeRangeValidator(
      FacesContext context,
      UIComponent component,
      String max,
      String min)
  {
    StringBuilder outBuffer = new StringBuilder(31 + min.length() + max.length());
    outBuffer.append("new TrDateTimeRangeValidator(");
    outBuffer.append(max);
    outBuffer.append(',');
    outBuffer.append(min);
    outBuffer.append(")");

    return outBuffer.toString();
  }

  
  private static final Collection<String> _IMPORT_NAMES = Collections.singletonList( "TrNumberConverter()" );
  
  
}