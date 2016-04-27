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
package org.apache.myfaces.trinidad.event;

import javax.faces.component.UIComponent;

import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;

/** 
 * Event delivered when a value has been processed within UIXEditableValue.updateModel
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/event/ValueUpdatedEvent.java#0 $) $Date: 23-dev-2013.19:08:59 $
 */
public class ValueUpdatedEvent extends FacesEvent
{
  
  public ValueUpdatedEvent(UIComponent source)
  {
    super(source);
  }
  
  @Override
  public void processListener(FacesListener listener)
  {
    ((ValueUpdatedListener) listener).processValueUpdated(this);
  }

  @Override
  public boolean isAppropriateListener(FacesListener listener)
  {
    return (listener instanceof ValueUpdatedListener);
  }
  
  private static final long serialVersionUID = 1L;
}
