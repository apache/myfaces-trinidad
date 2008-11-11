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
package org.apache.myfaces.trinidad.sandbox.event;

import javax.faces.component.UIComponent;
import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;

/**
 * Event used with the partialTrigger component
 * 
 * @version $Name$ ($Revision$) $Date$
 * @author Andrew Robinson
 */
public class PartialTriggerEvent extends FacesEvent
{
  /**
   * @param uiComponent the source component of the event 
   */
  public PartialTriggerEvent(UIComponent uiComponent)
  {
    super(uiComponent);
  }

  /**
   * @see javax.faces.event.FacesEvent#isAppropriateListener(javax.faces.event.FacesListener)
   */
  @Override
  public boolean isAppropriateListener(FacesListener faceslistener)
  {
    return faceslistener instanceof PartialTriggerListener;
  }

  /**
   * @see javax.faces.event.FacesEvent#processListener(javax.faces.event.FacesListener)
   */
  @Override
  public void processListener(FacesListener faceslistener)
  {
    ((PartialTriggerListener)faceslistener).onPartialTrigger(this);
  }
  
  private static final long serialVersionUID = 1L;
}
