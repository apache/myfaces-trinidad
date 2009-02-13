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
package org.apache.myfaces.trinidaddemo.convertValidate;

import java.util.Collection;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.trinidad.convert.ClientConverter;
import org.apache.myfaces.trinidad.util.LabeledFacesMessage;

/**
 * <p>Social Security number converter.</p>
 *
 */
public class SSNConverter implements Converter, ClientConverter
{
    public static final String CONVERTER_ID = "org.apache.myfaces.trinidaddemo.SSN";

    public Object getAsObject(
      FacesContext context,
      UIComponent component,
      String value)
    {
      // In a real app all the error messages would be translated
      if ( value == null || value.trim().length() == 0)
        return null;

      String tValue = value.trim();

      int length = tValue.length();
      if ( length < 9 )
      {
        throw new ConverterException(_getMessage(component, _SHORT_ERROR_TEXT));
      }

      if ( length > 11 )
      {
        throw new ConverterException(_getMessage(component, _LONG_ERROR_TEXT));
      }

      if (length == 9)
      {
        try
        {
          return Integer.valueOf(tValue);
        }
        catch(NumberFormatException nfe)
        {
          throw new ConverterException(_getMessage(component,
                                                   _INVALID_ERROR_TEXT));
        }
      }

      if ( length == 11 &&
           tValue.charAt(3) == '-' &&
           tValue.charAt(6) == '-')
      {
        String v = tValue.substring(0,3) +
                   tValue.substring(4,6) +
                   tValue.substring(7);

        try
        {
          return Integer.valueOf(v);
        }
        catch(NumberFormatException nfe)
        {
          throw new ConverterException(_getMessage(component,
                                                   _INVALID_ERROR_TEXT));
        }

      }
      throw new ConverterException(_getMessage(component, _INVALID_ERROR_TEXT));
    }

    public String getAsString(
      FacesContext context,
      UIComponent component,
      Object value)
    {
      if ( value == null || !(value instanceof Integer))
        return null;

      Integer integerValue = (Integer)value;

      String valueString = integerValue.toString();

      String ssn = valueString.substring(0,3) + '-' +
                   valueString.substring(3,5) + '-' +
                   valueString.substring(5,9);
      return ssn;
    }


  public Collection<String> getClientImportNames()
  {
    return null;
  }

  public String getClientLibrarySource(
   FacesContext context)
  {
    return context.getExternalContext().getRequestContextPath() + 
            "/jsLibs/ssnConverter.js";    
  }

  public String getClientConversion(
    FacesContext context,
   UIComponent component)
  {

    // in a real app the messages would be translated
    return ("new SSNConverter({"
            + "SUM:'Invalid social security number.',"
            + "S:'Value \"{1}\" is too short.',"
            + "L:'Value \"{1}\" is too long.',"
            + "N:'Value \"{1}\" is not a valid social security number.'})"
            );
  }

  @SuppressWarnings("unchecked")
  public String getClientScript(
   FacesContext context,
   UIComponent component)
  {
    return null;
  }
  
  private LabeledFacesMessage _getMessage(
   UIComponent component,
   String text)
  {
    // Using the LabeledFacesMessage allows the <tr:messages> component to
    // properly prepend the label as a link.
    LabeledFacesMessage lfm =
      new LabeledFacesMessage(FacesMessage.SEVERITY_ERROR,
                              "Conversion Error", text);
    if (component != null)
    {
      Object label = null;
      label = component.getAttributes().get("label");
      if (label == null)
        label = component.getValueExpression("label");
      if (label != null)
        lfm.setLabel(label);
    }
    return lfm;
  }

  private static final String _SHORT_ERROR_TEXT
    = "The value is too short to be a social security number";

  private static final String _LONG_ERROR_TEXT
    = "The value is too long to be a social security number";

  private static final String _INVALID_ERROR_TEXT
    = "The value is not a valid social security number";

}
