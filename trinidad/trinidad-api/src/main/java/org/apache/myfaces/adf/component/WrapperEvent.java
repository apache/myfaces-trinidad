/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adf.component;
import javax.faces.component.UIComponent;
import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;
import javax.faces.event.PhaseId;

/**
 * This class wraps a FacesEvent
 * @author The Oracle ADF Faces Team
 */
class WrapperEvent extends FacesEvent
{
  public WrapperEvent(UIComponent source, FacesEvent wrappedEvent)
  {
    super(source);
    // Event can't be null 
    if (wrappedEvent == null)
      throw new NullPointerException("wrappedEvent");

    _event = wrappedEvent;
  }

  public PhaseId getPhaseId()
  {
    return _event.getPhaseId();
  }

  public void setPhaseId(PhaseId phaseId)
  {
    _event.setPhaseId(phaseId);
  }

  public void processListener(FacesListener listener)
  {
    // This event is never delivered to a listener
    throw new IllegalStateException();
  }

  public boolean isAppropriateListener(FacesListener listener)
  {
    // This event is never delivered to a listener
    return false;
  }

  public FacesEvent getEvent()
  {
    return _event;
  }
  
  private final FacesEvent _event;
}
