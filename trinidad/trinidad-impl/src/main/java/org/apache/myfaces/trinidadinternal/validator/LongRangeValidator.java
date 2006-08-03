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

package org.apache.myfaces.trinidadinternal.validator;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import javax.faces.validator.ValidatorException;
import org.apache.myfaces.trinidad.util.MessageFactory;

import org.apache.myfaces.trinidadinternal.convert.ConverterUtils;
import org.apache.myfaces.trinidadinternal.util.IntegerUtils;




/**
 * <p>Implementation for <code>java.lang.Long</code> values.</p>
 *
 * @author The Oracle ADF Faces Team
 */
public class LongRangeValidator extends javax.faces.validator.LongRangeValidator
                                implements InternalClientValidator
{



  public void validate(
    FacesContext context,
    UIComponent component,
    Object value
    ) throws ValidatorException
  {
    try
    {
      super.validate(context, component, value);
    }
    catch (ValidatorException ve)
    {
         
      Object label = component.getAttributes().get("label");
      if ( null == label)
       label = component.getValueBinding("label");
       
      if (value != null && value instanceof Number)
      {
        long longValue = ((Number)value).longValue(); 
        
        if (longValue > _getMaximum()) 
        {
          String maxStr = IntegerUtils.getString(_getMaximum());
          FacesMessage msg = MessageFactory.getMessage(context,
                                            MAXIMUM_MESSAGE_ID,
                                            new Object[]{label, value, maxStr},
                                            label);                                       
          throw new ValidatorException(msg);         
        }
        else
        {
          String minStr = IntegerUtils.getString(_getMinimum());   
          FacesMessage msg = MessageFactory.getMessage(context,
                                            MINIMUM_MESSAGE_ID,
                                            new Object[]{label, value, minStr},
                                            label);
                                            
                                         
          throw new ValidatorException(msg);    
        }
      }
      else
      {
        throw ve;
      }
    }     
  }

  public String getLibKey(
   FacesContext context,
   UIComponent component)
  {
    return "DecimalConvert()";
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
    String maxStr = IntegerUtils.getString(_getMaximum());
    String minStr = IntegerUtils.getString(_getMinimum());
    return  ConverterUtils.getClientValidation(context, component,
                                               MAXIMUM_MESSAGE_ID,
                                               MINIMUM_MESSAGE_ID,
                                               VALIDATOR_ID,
                                               maxStr, minStr);
  }

  // not overriding getMaximum because I'll suddenly
  // start returning something other than 0...
  private long _getMaximum()
  {
    if (_maximumSet)
      return super.getMaximum();

    return Long.MAX_VALUE;
  }

  private long _getMinimum()
  {
    if (_minimumSet)
      return super.getMinimum();

    return Long.MIN_VALUE;
  }


  /**
   * <p>Set the maximum value .</p>
   *
   * @param maximum The new maximum value
   *
   */
  public void setMaximum(long maximum)
  {
    super.setMaximum(maximum);
    _maximumSet = true;
    
  }

  /**
   * <p>Set the minimum value .</p>
   *
   * @param minimum The new minimum value
   *
   */
  public void setMinimum(long minimum)
  {
    super.setMinimum(minimum);
    _minimumSet = true;
  }

  public Object saveState(FacesContext context)
  {
    Object values[] = new Object[2];
    Object clientValues[] = new Object[2];
    
    clientValues[0] = _maximumSet ? Boolean.TRUE : Boolean.FALSE;
    clientValues[1] = _minimumSet ? Boolean.TRUE : Boolean.FALSE;
    
    values[0] = super.saveState(context);
    values[1] = clientValues;
    return (values);
  }


  public void restoreState(FacesContext context, Object state)
  {
    Object values[] = (Object[]) state;
    super.restoreState(context, values[0]);
    Object clientValues[] = (Object[])values[1];
    _maximumSet = ((Boolean) clientValues[0]).booleanValue();
    _minimumSet = ((Boolean) clientValues[1]).booleanValue();
  }


  private boolean _maximumSet = false;
  private boolean _minimumSet = false;

}
