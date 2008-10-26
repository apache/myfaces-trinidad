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


import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreResponseStateManager;

import javax.faces.component.ContextCallback;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Implements the lifecycle as described in Spec. 1.0 PFD Chapter 2
 * <p/>
 * Apply request values phase (JSF Spec 2.2.2)
 */
class ApplyRequestValuesExecutor implements PhaseExecutor
{
  private static final TrinidadLogger LOG = TrinidadLogger.createTrinidadLogger(ApplyRequestValuesExecutor.class);
  private ContextCallback processDecodesCallback = new ApplyRequestValuesCallback();
  private ContextCallback decodeCallback = new DecodeCallback();

  public boolean execute(FacesContext facesContext)
  {

    String[] partialTargets = PartialLifecycleUtils.getPartialTargets(facesContext);
    if (!(facesContext.getViewRoot() instanceof UIViewRoot)) {
      LOG.severe("ViewRoot is not instanceof " + UIViewRoot.class.getName() + " skipping partial lifecycle");
    }
    if (partialTargets != null && facesContext.getViewRoot() instanceof UIViewRoot)
    {
      Map<String, String> parameterMap = facesContext.getExternalContext().getRequestParameterMap();
      UIViewRoot viewRoot = (UIViewRoot) facesContext.getViewRoot();

      if (!viewRoot.notifyListeners(facesContext, PhaseId.APPLY_REQUEST_VALUES,
          viewRoot.getBeforePhaseListener(), true))
      {
        // TODO partialRequest from inside Tree or Table

        // TODO navigationTree

        for (String partialTarget : partialTargets)
        {
          viewRoot.invokeOnComponent(facesContext, partialTarget, processDecodesCallback);
        }

        // TODO how to check that source and form already included in a partialTarget
        String sourceName = parameterMap.get("source");
        if (sourceName != null && !Arrays.asList(partialTargets).contains(sourceName))
        {
          // TODO source inside a partialTarget
          boolean found = viewRoot.invokeOnComponent(facesContext, sourceName, decodeCallback);
          if (!found)
          {
            LOG.warning("No source UIComponent found for '" + sourceName + "'");
          }
        }

        String formName = parameterMap.get(CoreResponseStateManager.FORM_FIELD_NAME);
        if (formName != null && !Arrays.asList(partialTargets).contains(formName))
        {
          // TODO form inside partialTarget
          // SubForm set submitted by queueEvent
          boolean found = viewRoot.invokeOnComponent(facesContext, formName, decodeCallback);
          if (!found)
          {
            LOG.warning("No form UIComponent found for '" + formName + "'");
          }
        }
        viewRoot.broadcastForPhase(PhaseId.APPLY_REQUEST_VALUES);
      }
      viewRoot.clearEvents(facesContext);
      viewRoot.notifyListeners(facesContext, PhaseId.APPLY_REQUEST_VALUES, viewRoot.getBeforePhaseListener(), false);

    } else
    {
      facesContext.getViewRoot().processDecodes(facesContext);
      // after a normal decode check for partialTargets
      final RequestContext requestContext = RequestContext.getCurrentInstance();
      if (!(facesContext.getResponseComplete() || facesContext.getRenderResponse())
          && requestContext.isPartialRequest(facesContext))
      {
        UIComponent source = facesContext.getViewRoot()
            .findComponent(facesContext.getExternalContext().getRequestParameterMap().get("source"));
        if (source != null)
        {
          List<String> list = new ArrayList<String>();
          Set<UIComponent> components = requestContext.getPartialTargets(source);
          for (UIComponent component : components)
          {
            list.add(component.getClientId(facesContext));
          }
          if (list.size() > 0)
          {
            PartialLifecycleUtils.setPartialTargets(facesContext, list.toArray(new String[list.size()]));
          }
        }
      }
    }
    return false;
  }

  public PhaseId getPhase()
  {
    return PhaseId.APPLY_REQUEST_VALUES;
  }
}
