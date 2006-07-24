/*
 * Copyright  1999-2006 The Apache Software Foundation.
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
 * BoundValue that uses a Boolean BoundValue to determine which of two values
 * to return.
 * <p>
 *@version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/bind/IfBoundValue.java#0 $) $Date: 10-nov-2005.18:56:39 $
 *@author The Oracle ADF Faces Team
 */
public class IfBoundValue implements BoundValue
{  
  public IfBoundValue(
    BoundValue testBoundValue,
    Object     trueValue,
    Object     falseValue
    )
  {
    this(testBoundValue,
         (trueValue != null)  ? new FixedBoundValue(trueValue) : null,
         (falseValue != null) ? new FixedBoundValue(falseValue) : null);
  }

  public IfBoundValue(
    BoundValue testBoundValue,
    BoundValue trueBoundValue,
    Object     falseValue
    )
  {
    this(testBoundValue,
         trueBoundValue,
         (falseValue != null) ? new FixedBoundValue(falseValue) : null);
  }

  public IfBoundValue(
    BoundValue testBoundValue,
    Object     trueValue,
    BoundValue falseBoundValue
    )
  {
    this(testBoundValue,
         (trueValue != null)  ? new FixedBoundValue(trueValue) : null,
         falseBoundValue);
  }


  public IfBoundValue(
    BoundValue testBoundValue,
    BoundValue trueBoundValue,
    BoundValue falseBoundValue
    )
  {
    if (testBoundValue == null)
      throw new IllegalArgumentException("test BoundValue required");

    if (trueBoundValue == null)
      trueBoundValue = FixedBoundValue.NULL_VALUE;

    if (falseBoundValue == null)
      falseBoundValue = FixedBoundValue.NULL_VALUE;

    _testValue  = testBoundValue;  
    _trueValue  = trueBoundValue;
    _falseValue = falseBoundValue;
  }
    
  
  /**
   * Calculates the current state of the model.
   */
  public Object getValue(
    RenderingContext context
    )
  {
    return ((Boolean.TRUE.equals(_testValue.getValue(context)))
              ? _trueValue.getValue(context)
              : _falseValue.getValue(context));
  }

  private BoundValue _testValue;  
  private BoundValue _trueValue;
  private BoundValue _falseValue;
}
