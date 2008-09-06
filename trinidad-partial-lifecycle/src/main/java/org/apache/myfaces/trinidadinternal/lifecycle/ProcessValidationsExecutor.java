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


public class ProcessValidationsExecutor implements PhaseExecutor
{

  private ContextCallback contextCallback = new ProcessValidationsCallback();

  public boolean execute(FacesContext facesContext)
  {
    String[] partialTargets = PartialLivecycleUtils.getPartialTargets(facesContext);
    if (partialTargets != null)
    {
      for (String clientId : partialTargets)
      {
        facesContext.getViewRoot().invokeOnComponent(facesContext, clientId, contextCallback);
      }
      //UIViewRoot viewRoot = facesContext.getViewRoot();
      // TODO broadcast Events UIViewRoot has no public method
      //viewRoot.broadcastEventsForPhase(facesContext, PROCESS_VALIDATIONS);
    } else
    {
      facesContext.getViewRoot().processValidators(facesContext);
    }
    return false;
  }

  public PhaseId getPhase()
  {
    return PhaseId.PROCESS_VALIDATIONS;
  }
}
