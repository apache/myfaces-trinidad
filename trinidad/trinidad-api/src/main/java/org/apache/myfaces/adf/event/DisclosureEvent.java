/*
 * Copyright  2003-2006 The Apache Software Foundation.
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

package org.apache.myfaces.adf.event;

import javax.faces.component.UIComponent;

import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;


/** 
 * Event delivered when expanding or collapsing a component.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/event/DisclosureEvent.java#0 $) $Date: 10-nov-2005.19:09:00 $
 * @author The Oracle ADF Faces Team
 */
public class DisclosureEvent extends FacesEvent
{
  public DisclosureEvent(UIComponent source, boolean expanded)
  {
    super(source);
    _expanded = expanded;
  }

  public boolean isExpanded()
  {
    return _expanded;
  }

  public void processListener(FacesListener listener)
  {
    ((DisclosureListener) listener).processDisclosure(this);
  }

  public boolean isAppropriateListener(FacesListener listener)
  {
    return (listener instanceof DisclosureListener);
  }
  
  public int hashCode()
  {
    int expandedHC = (_expanded ? Boolean.TRUE.hashCode() 
                                : Boolean.FALSE.hashCode());
    return (expandedHC << 4) ^
           (getPhaseId().hashCode() << 8) ^
           (getComponent().hashCode());
  }
  
  public boolean equals(
   Object o)
  {
    if (o instanceof DisclosureEvent)
    {
      DisclosureEvent that = (DisclosureEvent)o;
      return (this._expanded == that._expanded &&
              this.getComponent().equals(that.getComponent()) &&
              this.getPhaseId().equals(that.getPhaseId()));
    }
    
    return false;
  }

  public String toString()
  {
    StringBuffer sb = new StringBuffer();
    sb.append(getClass().getName());
    sb.append("[phaseId=");
    sb.append(getPhaseId());
    sb.append(",component=");
    sb.append(getComponent());
    sb.append(",expanded=");
    sb.append(isExpanded());
    sb.append(']');
    return sb.toString();
  }

  private final boolean _expanded;
}

