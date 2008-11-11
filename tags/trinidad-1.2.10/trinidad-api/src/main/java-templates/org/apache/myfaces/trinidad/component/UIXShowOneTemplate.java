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

import java.io.IOException;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
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
    // Care only if it is a DisclosureEvent, and only if its source is one of
    // its immediate children, for... one could bubble up from one of its grand
    // children that could be a ShowDetail.
    FacesEvent unwrappedEvent = e;
    while (unwrappedEvent instanceof WrapperEvent) // e.g. DisclosureEvent in a tr:iterator
    {
      unwrappedEvent = ((WrapperEvent)unwrappedEvent).getEvent();
    }

    if (unwrappedEvent instanceof DisclosureEvent)
    {
      UIComponent parent = unwrappedEvent.getComponent().getParent();
      while (parent != null && parent instanceof FlattenedComponent)
      {
        parent = parent.getParent();
      }

      if (this == parent)
      {
        // Care only if the incoming event was from the to-be-disclosed showDetailItem.
        DisclosureEvent disclosureEvent = (DisclosureEvent)unwrappedEvent;
        if (disclosureEvent.isExpanded())
        {
          FacesContext context = FacesContext.getCurrentInstance();
          String disclosedClientId = disclosureEvent.getComponent().getClientId(context);

          // Visit all of the flattened children:
          try
          {
            UIXComponent.processFlattenedChildren(
              context,
              _undisclosureCallback,
              getChildren(),
              new UndisclosureCallbackState(e, disclosedClientId));
          }
          catch (IOException ioe)
          {
            // This exception is not expected since no IO is used in this visitor's implementation.
            ioe.printStackTrace();
          }
        }
      }
    }
    super.queueEvent(e);
  }

  /**
   * State passed to the UndisclosureCallback.
   */
  private static class UndisclosureCallbackState
  {
    public UndisclosureCallbackState(
      FacesEvent facesEvent,
      String     clientIdBeingDisclosed)
    {
      this.facesEvent = facesEvent;
      this.clientIdBeingDisclosed = clientIdBeingDisclosed;
    }

    protected final FacesEvent facesEvent;
    protected final String clientIdBeingDisclosed;
  }

  /**
   * Visitor for each flattened child, using the information in the UndisclosureCallbackState.
   */
  private class UndisclosureCallback implements ComponentProcessor<UndisclosureCallbackState>
  {
    public void processComponent(
      FacesContext               facesContext,
      ComponentProcessingContext processContext,
      UIComponent                child,
      UndisclosureCallbackState  callbackState)
      throws IOException
    {
      String clientIdBeingDisclosed = callbackState.clientIdBeingDisclosed;

      // Search for the other item(s) to undisclose (make sure to skip the clientIdBeingDisclosed):
      String childClientId = child.getClientId(facesContext);
      if (!clientIdBeingDisclosed.equals(childClientId))
      {
        UIXShowDetail toBeUnDisclosedChild = (UIXShowDetail)child;
        if (toBeUnDisclosedChild.isDisclosed())
        {
          // Override the phaseId that would be already set on this event
          // (coming off of the to-be-disclosed showDetailItem), because the
          // phase-id should actually be determined by the 'immediate' attribute
          // on the to-be-undisclosed showDetailItem
          if (toBeUnDisclosedChild.isImmediate())
          {
            callbackState.facesEvent.setPhaseId(PhaseId.ANY_PHASE);
          }
          else
          {
            callbackState.facesEvent.setPhaseId(PhaseId.INVOKE_APPLICATION);
          }

          // Now queue the event for the to-be-undisclosed showDetailItem
          // Note that this is always delivered earlier than the one that is
          // already queued for to-be-disclosed showDetailItem.
          (new DisclosureEvent(toBeUnDisclosedChild, false)).queue();
        }
      }
    }
  }

  private final UndisclosureCallback _undisclosureCallback = new UndisclosureCallback();
}
