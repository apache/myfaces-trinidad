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

package org.apache.myfaces.trinidadinternal.convert;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;

/**
 * <p>Implementation for <code>java.lang.Short</code> values.</p>
 *
 *
 * @author The Oracle ADF Faces Team
 */
public class ShortConverter extends javax.faces.convert.ShortConverter
                              implements InternalClientConverter
{


    /**
     * <p>The message identifier of the FacesMessage to be created if
     * the value is greater than Short.MAX_VALUE.
     * The message format string for this
     * message may optionally include a <code>{2}</code> placeholder, which
     * will be replaced by Short.MAX_VALUE.</p>
     */
    public static final String MAXIMUM_MESSAGE_ID =
        "org.apache.myfaces.trinidad.convert.ShortConverter.MAXIMUM";

    /**
     * <p>The message identifier of the FacesMessage to be created if
     * the value is less than Short.MIN_VALUE.
     * The message format string for this
     * message may optionally include a <code>{2}</code> placeholder, which
     * will be replaced by Short.MIN_VALUE.</p>
     */
    public static final String MINIMUM_MESSAGE_ID =
        "org.apache.myfaces.trinidad.convert.ShortConverter.MINIMUM";

    /**
     * <p>The message identifier of the FacesMessage to be created if
     * the value cannot be converted to an integer
     */
    public static final String CONVERT_MESSAGE_ID =
        "org.apache.myfaces.trinidad.convert.ShortConverter.CONVERT";



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
     
      throw ConverterUtils.getIntegerConverterException(context, 
                                                        component, 
                                                        ce,
                                                        value,
                                                        CONVERT_MESSAGE_ID,
                                                        MAXIMUM_MESSAGE_ID,
                                                        _SHORT_MAX,
                                                        MINIMUM_MESSAGE_ID,
                                                        _SHORT_MIN);
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
    return ConverterUtils.getClientConversion(context, component,
                                              MAXIMUM_MESSAGE_ID,
                                              MINIMUM_MESSAGE_ID,
                                              CONVERT_MESSAGE_ID,
                                              _SHORT_MAX, _SHORT_MIN);
  }


  private static final String _SHORT_MAX = Short.toString(Short.MAX_VALUE);
  private static final String _SHORT_MIN = Short.toString(Short.MIN_VALUE);
}
