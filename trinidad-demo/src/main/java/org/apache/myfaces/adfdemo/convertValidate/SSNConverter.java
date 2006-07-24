/*
 * Copyright 2006 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.myfaces.adfdemo.convertValidate;


import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.adf.convert.ClientConverter;
import org.apache.myfaces.adf.util.LabeledFacesMessage;

/**
 * <p>Social Security number converter.</p>
 *
 */
public class SSNConverter implements Converter, ClientConverter
{
    public static final String CONVERTER_ID = "org.apache.myfaces.adfdemo.SSN";

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
      int intValue = integerValue.intValue();

      String valueString = integerValue.toString();

      String ssn = valueString.substring(0,3) + '-' +
                   valueString.substring(3,5) + '-' +
                   valueString.substring(5,9);
      return ssn;
    }


  public String getClientConversion(
    FacesContext context,
   UIComponent component)
  {

    // in a real app the messages would be translated
    return ("new SSNConverter({"
            + "S:'{0} - Value \"{1}\" is too short.',"
            + "L:'{0} - Value \"{1}\" is too long.',"
            + "N:'{0} - Value \"{1}\" is not a valid social security number.'})"
            );
  }

  public String getClientScript(
   FacesContext context,
   UIComponent component)
  {
    // check if the script has already been returned this request
    Object scriptReturned =
                context.getExternalContext().getRequestMap().get(CONVERTER_ID);

    // if scriptReturned is null the script hasn't been returned yet
    if ( scriptReturned == null)
    {
      context.getExternalContext().getRequestMap().put(CONVERTER_ID,
                                                       Boolean.TRUE);
      return  _sSSNjs;
    }
    // if scriptReturned is not null, then script has already been returned,
    // so don't return it again.
    else
      return null;

   }

  private LabeledFacesMessage _getMessage(
   UIComponent component,
   String text)
  {
    // Using the LabeledFacesMessage allows the <af:messages> component to
    // properly prepend the label as a link.
    LabeledFacesMessage lfm =
      new LabeledFacesMessage(FacesMessage.SEVERITY_ERROR,
                              "Conversion Error", text);
    if (component != null)
    {
      Object label = null;
      label = component.getAttributes().get("label");
      if (label == null)
        label = component.getValueBinding("label");
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

  private static final String _sSSNjs =
    "function ssnGetAsString(value)"+
    "{return value.substring(0,3) + '-' " +
          "+ value.substring(3,5) + '-' + value.substring(5);}" +
    "function ssnGetAsObject(value)" +
      "{if (!value)return (void 0);" +
      "var len=value.length;"+
      "var messageKey = SSNConverter.NOT;" +
      "if (len < 9 )"+
        "messageKey = SSNConverter.SHORT;" +
      "else if (len > 11)"+
        "messageKey = SSNConverter.LONG;" +
      "else if (len == 9)" +
      "{ if (!isNaN(value))" +
          "return value;" +
      "}" +
      "else if (len == 11 && value.charAt(3) == '-' && " +
                "value.charAt(6) == '-')" +
      "{" +
        "var result = value.substring(0,3) + value.substring(4,6) + " +
                    "value.substring(7);"+
        "if (!isNaN(result))"+
          "return result;" +
      "}" +
     "if (messageKey!=void(0) && this._messages!=void(0))" +
       "return new ConverterException(this._messages[messageKey]);" +
     "return void(0);}" +
    "function SSNConverter(messages)" +
      "{this._messages = messages;}" +
    "SSNConverter.prototype = new Converter();" +
    "SSNConverter.prototype.getAsString = ssnGetAsString;" +
    "SSNConverter.prototype.getAsObject = ssnGetAsObject;" +
    "SSNConverter.SHORT = 'S';" +
    "SSNConverter.LONG  = 'L';" +
    "SSNConverter.NOT   = 'N';";

}
