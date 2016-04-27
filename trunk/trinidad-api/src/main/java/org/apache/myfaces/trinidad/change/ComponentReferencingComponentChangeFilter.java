/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.myfaces.trinidad.change;

import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.util.ComponentReference;

/**
 * ComponentChangeFilter implementation that keeps reference to a Component.
 * 
 * It is not appropriate for listeners to hold references to UIComponent instances beyond the request scope. This filter
 * implementation converts the supplied component into a ComponentReference giving two advantages
 * 1. Listeners can hold on to such filters beyond request scope
 * 2. Takes care of serialization of the referenced component
 * 
 * One of the uses of this class is to keep a reference to a root of a component subtree, and then filter
 * ComponentChanges based on whether the target component of the ComponentChange belongs to the subtree.
 * 
 * @param <T> The type of UIComponent that this filter can keep references to
 * @see org.apache.myfaces.trinidad.util.ComponentReference
 */
public abstract class ComponentReferencingComponentChangeFilter<T extends UIComponent> extends ComponentChangeFilter
{
  public ComponentReferencingComponentChangeFilter(T component)
  {
    if (component == null)
      throw new NullPointerException();
    
    _compRef = ComponentReference.newUIComponentReference(component);
  }

  protected final T getComponent()
  {
    return _compRef.getComponent();
  }

  private final ComponentReference<T> _compRef;

  private static final long serialVersionUID = 1L;
}