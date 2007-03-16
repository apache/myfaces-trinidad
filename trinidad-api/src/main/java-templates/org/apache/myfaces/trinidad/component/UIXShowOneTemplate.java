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
package org.apache.myfaces.trinidad.component;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.event.DisclosureEvent;

/**
 * Base class for ShowOne component.
 * @version $Name:  $ ($Revision$) $Date$
 */
abstract public class UIXShowOneTemplate extends UIXComponentBase
{
	
  @Override
  @SuppressWarnings("unchecked")
  public void queueEvent(FacesEvent e)
  {
    //  Care only if it is a DisclosureEvent, and only if its source is one of
    //  its immediate children, for... one could bubble up from one of its grand
    //  children that could be a ShowDetail.
    if ( (e instanceof DisclosureEvent) &&
         (this == e.getComponent().getParent()) )
    {
      //  Care only if the incoming event was from the to-be-disclosed
      //  showDetailItem
      if (((DisclosureEvent) e).isExpanded())
      {
        UIXShowDetail toBeUnDisclosedChild = null;
        List<UIComponent> children = getChildren();
        for(UIComponent child : children)
        {
          if (child instanceof UIXShowDetail)
          {
            toBeUnDisclosedChild =  (UIXShowDetail) child;
            if (toBeUnDisclosedChild.isDisclosed())
              break;
          }
        }
        
        //  Override the phaseId that would be already set on this event
        //  (coming off of the to-be-disclosed showDetailItem), because the
        //  phase-id should actually be determined by the 'immediate' attribute
        //  on the to-be-undisclosed showDetailItem
        if (toBeUnDisclosedChild.isImmediate())
        {
          e.setPhaseId(PhaseId.ANY_PHASE);
        }
        else
        {
          e.setPhaseId(PhaseId.INVOKE_APPLICATION);
        }
        //  Now queue the event for the to-be-undisclosed showDetailItem
        //  Note that this is always delivered earlier than the one that is
        //  already queued for to-be-disclosed showDetailItem.
        (new DisclosureEvent(toBeUnDisclosedChild, false)).queue();
      }
    }
    super.queueEvent(e);
  }

}
