/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adf.component;

import javax.faces.component.ActionSource;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

/**
 * Base class for progress components
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java-templates/oracle/adf/view/faces/component/UIXProgressTemplate.java#0 $) $Date: 10-nov-2005.19:07:43 $
 * @author The Oracle ADF Faces Team
 */
abstract public class UIXProgressTemplate
                extends UIXComponentBase
                implements ActionSource
{
  public void queueEvent(FacesEvent e)
  {
    if (e.getSource() == this && e instanceof ActionEvent)
    {
      if (isImmediate())
        e.setPhaseId(PhaseId.ANY_PHASE);
      else
        e.setPhaseId(PhaseId.INVOKE_APPLICATION);
    }

    super.queueEvent(e);
  }

  public void broadcast(FacesEvent event) throws AbortProcessingException
  {
    super.broadcast(event);

    // Notify the specified action listener method (if any),
    // and the default action listener
    if (event instanceof ActionEvent)
    {
      FacesContext context = getFacesContext();
      MethodBinding mb = getActionListener();
      if (mb != null)
        mb.invoke(context, new Object[] { event });

      ActionListener defaultActionListener =
        context.getApplication().getActionListener();
      if (defaultActionListener != null)
      {
        defaultActionListener.processAction((ActionEvent) event);
      }
    }
  }
}
