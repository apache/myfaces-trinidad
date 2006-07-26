/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.ui.data.bind;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.data.BoundValue;


/**
 * BoundValue that wraps another BoundValue and returns the result of
 * that BoundValue as a String, if possible.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bind/ToStringBoundValue.java#0 $) $Date: 10-nov-2005.18:56:42 $
 * @author The Oracle ADF Faces Team
 */
public class ToStringBoundValue implements BoundValue
{
  /**
   * Converts an Object into a String for non-null values.
   */
  public static String convertToString(
    Object data
    )
  {
    if (data != null)
    {
      if (data instanceof char[])
      {
        return new String((char[])data);
      }
      else
      {
        return data.toString();
      }
    }
    else
    {
      return null;
    }
  }


  /**
   * Creates a ToStringBoundValue.
   * @param wrapped the BoundValue to convert into a String.
   */
  public ToStringBoundValue(
    BoundValue wrapped
    )
  {
    if (wrapped == null)
      throw new IllegalArgumentException();
      
    _wrapped = wrapped;
  }

  public Object getValue(
    RenderingContext context
    )
  {
    Object value = _wrapped.getValue(context);
    
    if (value != null)
    {
      value = convertToString(value);
    }
    
    return value;
  }
  
  private BoundValue _wrapped;
}
