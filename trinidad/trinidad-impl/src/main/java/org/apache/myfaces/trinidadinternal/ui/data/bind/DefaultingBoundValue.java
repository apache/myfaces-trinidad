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
package org.apache.myfaces.trinidadinternal.ui.data.bind;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;


/**
 * A BoundValue implementation that wraps another and returns a
 * provided default value in place of null.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bind/DefaultingBoundValue.java#0 $) $Date: 10-nov-2005.18:56:38 $
 * @author The Oracle ADF Faces Team
 */
public class DefaultingBoundValue implements BoundValue
{
  /**
   * Creates a DefaultingBoundValue.
   * @param wrapped the BoundValue to try first
   * @param defaultValue the value to return if <code>wrapped</code>
   *    returns null
   */
  public DefaultingBoundValue(
    BoundValue wrapped,
    Object     defaultValue
    )
  {
    if (wrapped == null)
      throw new IllegalArgumentException();
          
    _wrapped = wrapped;
    _default = new FixedBoundValue(defaultValue);
  }

  /**
   * Creates a DefaultingBoundValue.
   * @param wrapped the BoundValue to try first
   * @param defaultBoundValue the BoundValue to use if <code>wrapped</code>
   *    returns null
   */
  public DefaultingBoundValue(
    BoundValue wrapped,
    BoundValue defaultBoundValue
    )
  {
    if (wrapped == null)
      throw new IllegalArgumentException();
          
    _wrapped = wrapped;
    _default = defaultBoundValue;
  }

  public Object getValue(
    UIXRenderingContext context
    )
  {
    Object value = getWrappedValue(context);
    
    if (value != null)
    {
      return value;
    }
    else
    {
      return _default.getValue(context);
    }
  }
  
  protected Object getWrappedValue(
    UIXRenderingContext context
    )
  {
    return _wrapped.getValue(context);
  }

  private BoundValue _wrapped;
  private BoundValue _default;
}
