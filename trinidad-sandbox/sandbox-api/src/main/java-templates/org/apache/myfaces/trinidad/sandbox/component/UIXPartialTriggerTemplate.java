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
package org.apache.myfaces.trinidad.sandbox.component;

import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.component.UIXComponentBase;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.sandbox.event.PartialTriggerEvent;

/**
 * This component queues
 */
public abstract class UIXPartialTriggerTemplate extends UIXComponentBase
{
/**/  public abstract String[] getPartialTargets();
/**/  public abstract boolean isImmediate();

  /**
   * @see org.apache.myfaces.trinidad.sandbox.component.UIXComponentBase#queueEvent(javax.faces.event.FacesEvent)
   */
  @Override
  public void queueEvent(FacesEvent event)
  {
    PartialTriggerEvent triggerEvent = new PartialTriggerEvent(this);
    
    triggerEvent.setPhaseId(isImmediate() ? PhaseId.ANY_PHASE :
      event.getPhaseId());
    
    super.queueEvent(triggerEvent);
    super.queueEvent(event);
  }
  
  /**
   * @see org.apache.myfaces.trinidad.sandbox.component.UIXComponentBase#broadcast(javax.faces.event.FacesEvent)
   */
  @Override
  public void broadcast(FacesEvent event) throws AbortProcessingException
  {
    String[] targets = getPartialTargets();
    if (targets != null)
    {
      RequestContext arc = RequestContext.getCurrentInstance();
      if (arc != null)
      {
        arc.addPartialTargets(this, targets);
      }
    }
    super.broadcast(event);
  }
}
