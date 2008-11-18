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
package org.apache.myfaces.trinidad.change;

import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.change.ComponentChange;

/**
 * An ChangeManager implementation that is all a no-op.
 *
 */
public class NullChangeManager extends ChangeManager
{
  /**
   * {@inheritDoc}
   */
  @Override
  public void addComponentChange(
    FacesContext facesContext,
    UIComponent uiComponent,
    ComponentChange change)
  {
    // do nothing
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public Iterator<ComponentChange> getComponentChanges(FacesContext facesContext,
                             UIComponent uiComponent)
  {
    return null;
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public Iterator<String> getComponentIdsWithChanges(FacesContext facesContext)
  {
    return null;
  }
}