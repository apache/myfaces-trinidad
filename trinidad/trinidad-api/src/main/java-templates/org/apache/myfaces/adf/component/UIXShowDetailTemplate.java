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
package org.apache.myfaces.adf.component;

import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

import org.apache.myfaces.adf.event.DisclosureEvent;

/**
 * Base class for ShowDetail component.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java-templates/oracle/adf/view/faces/component/UIXShowDetailTemplate.java#0 $) $Date: 10-nov-2005.19:07:47 $
 * @author The Oracle ADF Faces Team
 */
abstract public class UIXShowDetailTemplate extends UIXComponentBase
{
/**/ // Abstract methods implemented by code gen
/**/  abstract public boolean isDisclosed();
/**/  abstract public void setDisclosed(boolean setDisclosed);
/**/  abstract public boolean isImmediate();
/**/  abstract public MethodBinding getDisclosureListener();

  public void processDecodes(FacesContext context)
  {
    // If we're not disclosed, only process ourselves
    if (!isDisclosed())
    {
      if (isRendered())
        decode(context);
    }
    else
      super.processDecodes(context);
  }

  public void processValidators(FacesContext context)
  {
    if (isDisclosed())
      super.processValidators(context);
  }

  public void processUpdates(FacesContext context)
  {
    if (isDisclosed())
      super.processUpdates(context);
  }

  public void broadcast(FacesEvent event) throws AbortProcessingException
  {
    // Perform standard superclass processing
    super.broadcast(event);

    if (event instanceof DisclosureEvent)
    {
      // Expand or collapse this showDetail
      boolean isDisclosed = ((DisclosureEvent) event).isExpanded();
      setDisclosed(isDisclosed);
      //pu: Implicitly record a Change for 'disclosed' attribute
      addAttributeChange("disclosed",
                         isDisclosed ? Boolean.TRUE : Boolean.FALSE);
      if (isImmediate())
        getFacesContext().renderResponse();

      // Notify the specified disclosure listener method (if any)
      __broadcast(event, getDisclosureListener());
    }
  }

  /**
   * @todo Should a non-immediate ShowDetail fire after UPDATE_MODEL_VALUES,
   * or INVOKE_APPLICATION?  Or should we only support non-immediate
   * ShowDetails?
   */
  public void queueEvent(FacesEvent e)
  {
    if ((e instanceof DisclosureEvent) && (e.getSource() == this))
    {
      if (isImmediate())
      {
        e.setPhaseId(PhaseId.ANY_PHASE);
      }
      else
      {
        e.setPhaseId(PhaseId.INVOKE_APPLICATION);
      }
    }

    super.queueEvent(e);
  }

}
