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
package org.apache.myfaces.trinidadinternal.uinode;

import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;

/**
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
class ValueBindingBoundValue implements BoundValue
{
  public ValueBindingBoundValue(ValueBinding binding)
  {
    if (binding == null)
      throw new NullPointerException();

    _binding = binding;
  }

  /**
   * @todo Better way to retrieve FacesContext
   */
  public Object getValue(UIXRenderingContext rContext)
  {
    FacesContext fContext = FacesContext.getCurrentInstance();
    return _binding.getValue(fContext);
  }

  private final ValueBinding _binding;
}
