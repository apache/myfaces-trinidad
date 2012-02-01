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
package org.apache.myfaces.trinidad.component.html;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Map;
import java.util.Queue;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXComponentBase;
import org.apache.myfaces.trinidad.context.ComponentContextManager;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.SuspendedContextChanges;


public abstract class HtmlBodyTemplate
  extends UIXComponentBase
{
  /**
   * Suspends any context changes before allowing invokeOnComponent or visitTree calls to continue,
   * allowing components to undo any context changes during a re-entrant call.
   * @param facesContext the faces context
   * @see ComponentContextManager#suspend(FacesContext)
   */
  @Override
  protected void setupVisitingContext(FacesContext facesContext)
  {
    ComponentContextManager ctxMgr = RequestContext.getCurrentInstance()
      .getComponentContextManager();

    // Suspend any current component context during a visit tree for re-entrant
    // component tree processing
    SuspendedContextChanges suspendedChanges = ctxMgr.suspend(facesContext);

    Map<String, Object> reqMap = facesContext.getExternalContext().getRequestMap();
    @SuppressWarnings("unchecked")
    Queue<SuspendedContextChanges> suspendedChangesQueue = (Queue<SuspendedContextChanges>)
      reqMap.get(_SUSPENDED_CHANGES_KEY);
    if (suspendedChanges == null)
    {
      suspendedChangesQueue = Collections.asLifoQueue(new ArrayDeque<SuspendedContextChanges>());
      reqMap.put(_SUSPENDED_CHANGES_KEY, suspendedChangesQueue);
    }

    suspendedChangesQueue.offer(suspendedChanges);

    super.setupVisitingContext(facesContext);
  }

  /**
   * Re-applies the suspended context changes.
   * @param facesContext the faces context
   * @see #setupVisitingContext(FacesContext)
   * @see ComponentContextManager#resume(FacesContext, SuspendedContextChanges)
   */
  @Override
  protected void tearDownVisitingContext(FacesContext facesContext)
  {
    super.tearDownVisitingContext(facesContext);

    ComponentContextManager ctxMgr = RequestContext.getCurrentInstance()
      .getComponentContextManager();
    Map<String, Object> reqMap = facesContext.getExternalContext().getRequestMap();
    @SuppressWarnings("unchecked")
    Queue<SuspendedContextChanges> suspendedChangesQueue = (Queue<SuspendedContextChanges>)
      reqMap.get(_SUSPENDED_CHANGES_KEY);
    SuspendedContextChanges changes = suspendedChangesQueue.poll();
    ctxMgr.resume(facesContext, changes);
  }

  private final static String _SUSPENDED_CHANGES_KEY = HtmlBody.class.getName() +
                                                       ".SUSPENDED_CHANGES";
}