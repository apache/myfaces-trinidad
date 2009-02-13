package org.apache.myfaces.trinidadinternal.lifecycle;

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import javax.faces.component.ContextCallback;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;


public class UpdateModelValuesExecutor implements PhaseExecutor
{

  private ContextCallback contextCallback = new UpdateModelValuesCallback();

  public boolean execute(FacesContext facesContext)
  {
    String[] partialTargets = PartialLifecycleUtils.getPartialTargets(facesContext);
    if (partialTargets != null && facesContext.getViewRoot() instanceof UIViewRoot)
    {
      UIViewRoot viewRoot = (UIViewRoot) facesContext.getViewRoot();
      if (!viewRoot.notifyListeners(facesContext, PhaseId.UPDATE_MODEL_VALUES, viewRoot.getBeforePhaseListener(), true))
      {
        for (String clientId : partialTargets)
        {
          facesContext.getViewRoot().invokeOnComponent(facesContext, clientId, contextCallback);
        }
        viewRoot.broadcastForPhase(PhaseId.UPDATE_MODEL_VALUES);
      }
      viewRoot.clearEvents(facesContext);
      viewRoot.notifyListeners(facesContext, PhaseId.UPDATE_MODEL_VALUES, viewRoot.getBeforePhaseListener(), false);
    } else
    {
      facesContext.getViewRoot().processUpdates(facesContext);
    }
    return false;
  }

  public PhaseId getPhase()
  {
    return PhaseId.UPDATE_MODEL_VALUES;
  }
}
