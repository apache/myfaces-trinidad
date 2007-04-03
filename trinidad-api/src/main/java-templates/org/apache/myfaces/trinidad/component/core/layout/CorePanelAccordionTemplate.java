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
package org.apache.myfaces.trinidad.component.core.layout;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;
import java.util.List;

import org.apache.myfaces.trinidad.event.DisclosureEvent;
import org.apache.myfaces.trinidad.component.UIXShowDetail;


abstract public class CorePanelAccordion extends UIXPanel
{
    /**
   * Queues an event recursively to the root component.
   * @param event
   * @throws javax.faces.event.AbortProcessingException
   */
	@Override
  public void queueEvent(FacesEvent event)
    throws AbortProcessingException
  {

    // Handle the DisclosureEvent specifically
    if (event instanceof DisclosureEvent && !this.isDiscloseMany())
    {
          List li = this.getChildren();

          for(int i=0; i<li.size(); i++) {
            UIComponent comp = (UIComponent) li.get(i);
            if(comp instanceof UIXShowDetail) {
                ((UIXShowDetail) comp).setDisclosed(false);
            }
          }
    }
    super.queueEvent(event);
  }

}