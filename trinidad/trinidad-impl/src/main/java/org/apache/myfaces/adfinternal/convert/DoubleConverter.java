/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.convert;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;

/**
 * <p>Implementation for <code>java.lang.Double</code> values.</p>
 *
 * @author The Oracle ADF Faces Team
 */
public class DoubleConverter extends javax.faces.convert.DoubleConverter
                             implements InternalClientConverter
{
    /**
     * <p>The message identifier of the FacesMessage to be created if
     * the value cannot be converted
     */
    public static final String CONVERT_MESSAGE_ID =
        "org.apache.myfaces.adf.convert.DoubleConverter.CONVERT";

  public Object getAsObject(
    FacesContext context, 
    UIComponent component,
    String value) 
  {
    try
    {
      return super.getAsObject(context, component, value);
    }
    catch(ConverterException ce)
    {
      throw ConverterUtils.createConverterException(context, 
                                                         component,
                                                         CONVERT_MESSAGE_ID, 
                                                         value);                                                      
    }     
    
  }


  public String getClientScript(
   FacesContext context,
   UIComponent component)
  {
    return null;
  }

  public String getLibKey(
   FacesContext context,
   UIComponent component)
  {
    return "DecimalConvert()";
  }

  /**
   * @todo translations
   * @param context
   * @return
   */
  public String getClientConversion(
    FacesContext context,
    UIComponent component)
  {
    return "new DecimalFormat()";
  }

  public String getClientConversionFormat(
   FacesContext context,
   UIComponent component)
  {
    return ConverterUtils.getClientConversionFormat(context, component,
                                                    CONVERT_MESSAGE_ID);
  }
}
