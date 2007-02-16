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
package org.apache.myfaces.trinidadinternal.convert;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.trinidad.convert.ClientConverter;
import org.apache.myfaces.trinidadinternal.util.JsonUtils;

public final class NumberConverter extends org.apache.myfaces.trinidad.convert.NumberConverter
                   implements ClientConverter
{
  public NumberConverter()
  {
  }
  
  @Override
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
  
  @Override
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

  public String getClientConversion(FacesContext context, UIComponent component)
  {
    String hintPattern = this.getHintPattern();
    Map<String, String> cMessages = null;
    if(hintPattern != null)
    {
      cMessages = new HashMap<String, String>();
      cMessages.put("hintPattern", hintPattern);
    }
    
    return _getTrNumberConverter(context, component, cMessages);
  }

  public Collection<String> getClientImportNames()
  {
    return _IMPORT_NAMES;
  }

  public String getClientLibrarySource(FacesContext context)
  {
    return null;
  }

  public String getClientScript(FacesContext context, UIComponent component)
  {
    return null;
  }
  
  private String _getTrNumberConverter(
      FacesContext context,
      UIComponent  component,
      Map<?, ?>    messages)
    {
      StringBuilder outBuffer = new StringBuilder(250);

      outBuffer.append("new TrNumberConverter(");

      String pattern = this.getPattern();
      String type = this.getType();

      try
      {
        JsonUtils.writeString(outBuffer, pattern, false); 
      } catch (Exception e)
      {
        outBuffer.append("null");
      }
      outBuffer.append(',');
      try
      {
        JsonUtils.writeString(outBuffer, type, false);
      } catch (Exception e)
      {
        outBuffer.append("null");
      }
      outBuffer.append(',');
      try
      {
        JsonUtils.writeMap(outBuffer, messages, false); 
      } catch (Exception e)
      {
        outBuffer.append("null");
      }
      outBuffer.append(')');

      return outBuffer.toString();
    }
  private static final Collection<String> _IMPORT_NAMES = Collections.singletonList( "TrNumberConverter()" );
}
