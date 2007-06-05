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
package org.apache.myfaces.trinidadinternal.binding;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;
import javax.faces.el.PropertyNotFoundException;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.bean.util.StateUtils;

/**
 * Base implementation of a ValueBinding that wraps a second binding.
 * ValueBindingAdapters are always read-only.
 *
 */
abstract public class ValueBindingAdapter extends ValueBinding
  implements StateHolder
{
  protected ValueBindingAdapter(ValueBinding base)
  {
    _base = base;
  }

  @Override
  public Object getValue(FacesContext context)
  {
    return _base.getValue(context);
  }

  @Override
  public void setValue(FacesContext context, Object value)
  {
    throw new PropertyNotFoundException("Can't set value");
  }
  
  @Override
  public boolean isReadOnly(FacesContext context)
  {
    return true;
  }

  @Override
  public Class<?> getType(FacesContext context)
  {
    return _base.getType(context);
  }

  public Object saveState(FacesContext context)
  {
    return StateUtils.saveStateHolder(context, _base);
  }

  public void restoreState(FacesContext context, Object state)
  {
    _base = (ValueBinding) StateUtils.restoreStateHolder(context, state);
  }

  public void setTransient(boolean isTransient)
  {
    if (isTransient)
      throw new UnsupportedOperationException();
  }

  public boolean isTransient()
  {
    return false;
  }

  private ValueBinding _base;
}
