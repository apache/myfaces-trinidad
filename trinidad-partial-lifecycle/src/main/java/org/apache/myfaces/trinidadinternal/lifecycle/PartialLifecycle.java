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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import javax.faces.FacesException;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import javax.faces.lifecycle.Lifecycle;
import java.util.ArrayList;
import java.util.List;


public class PartialLifecycle extends Lifecycle
{

  private static final TrinidadLogger LOG = TrinidadLogger.createTrinidadLogger(PartialLifecycle.class);

  private PhaseExecutor[] lifecycleExecutors;
  private PhaseExecutor renderExecutor;

  private final List<PhaseListener> phaseListenerList = new ArrayList<PhaseListener>();

  /**
   * Lazy cache for returning phaseListenerList as an Array.
   */
  private PhaseListener[] phaseListenerArray = null;

  public PartialLifecycle()
  {
    // hide from public access
    lifecycleExecutors = new PhaseExecutor[]{
        new RestoreViewExecutor(),
        new ApplyRequestValuesExecutor(),
        new ProcessValidationsExecutor(),
        new UpdateModelValuesExecutor(),
        new InvokeApplicationExecutor()
    };

    renderExecutor = new RenderResponseExecutor();
  }

  public void execute(FacesContext facesContext) throws FacesException
  {

    PhaseListenerManager phaseListenerMgr = new PhaseListenerManager(this, facesContext, getPhaseListeners());

    for (PhaseExecutor executor : lifecycleExecutors)
    {
      LOG.info("Execute Phase " + executor.getPhase());
      if (executePhase(facesContext, executor, phaseListenerMgr))
      {
        return;
      }
    }
  }

  private boolean executePhase(FacesContext facesContext, PhaseExecutor executor, PhaseListenerManager phaseListenerMgr)
      throws FacesException
  {

    boolean skipFurtherProcessing = false;
    if (LOG.isFinest())
    {
      LOG.finest("entering " + executor.getPhase() + " in " + this.getClass().getName());
    }


    try
    {
      phaseListenerMgr.informPhaseListenersBefore(executor.getPhase());

      if (isResponseComplete(facesContext, executor.getPhase(), true))
      {
        // have to return right away
        return true;
      }
      if (shouldRenderResponse(facesContext, executor.getPhase(), true))
      {
        skipFurtherProcessing = true;
      }

      if (executor.execute(facesContext))
      {
        return true;
      }
    } finally
    {
      phaseListenerMgr.informPhaseListenersAfter(executor.getPhase());
    }


    if (isResponseComplete(facesContext, executor.getPhase(), false)
        || shouldRenderResponse(facesContext, executor.getPhase(), false))
    {
      // since this phase is completed we don't need to return right away even if the response is completed
      skipFurtherProcessing = true;
    }

    if (!skipFurtherProcessing && LOG.isFinest())
    {
      LOG.finest("exiting " + executor.getPhase() + " in " + this.getClass().getName());
    }

    return skipFurtherProcessing;
  }

  public void render(FacesContext facesContext) throws FacesException
  {
    // if the response is complete we should not be invoking the phase listeners

    if (isResponseComplete(facesContext, renderExecutor.getPhase(), true))
    {
      return;
    }
    if (LOG.isFinest())
    {
      LOG.finest("entering " + renderExecutor.getPhase() + " in " + this.getClass().getName());
    }

    PhaseListenerManager phaseListenerMgr = new PhaseListenerManager(this, facesContext, getPhaseListeners());

    try
    {
      phaseListenerMgr.informPhaseListenersBefore(renderExecutor.getPhase());
      // also possible that one of the listeners completed the response
      if (isResponseComplete(facesContext, renderExecutor.getPhase(), true))
      {
        return;
      }
      renderExecutor.execute(facesContext);
    } finally
    {
      phaseListenerMgr.informPhaseListenersAfter(renderExecutor.getPhase());
    }

    if (LOG.isFinest())
    {
      LOG.finest("exiting " + renderExecutor.getPhase() + " in " + this.getClass().getName());
    }
  }

  private boolean isResponseComplete(FacesContext facesContext, PhaseId phase, boolean before)
  {
    boolean flag = false;
    if (facesContext.getResponseComplete())
    {
      if (LOG.isFine())
      {
        LOG.fine("exiting from lifecycle.execute in " + phase
            + " because getResponseComplete is true from one of the "
            + (before ? "before" : "after") + " listeners");
      }
      flag = true;
    }
    return flag;
  }

  private boolean shouldRenderResponse(FacesContext facesContext, PhaseId phase, boolean before)
  {
    boolean flag = false;
    if (facesContext.getRenderResponse())
    {
      if (LOG.isFinest())
      {
        LOG.finest("exiting from lifecycle.execute in " + phase
            + " because getRenderResponse is true from one of the "
            + (before ? "before" : "after") + " listeners");
      }
      flag = true;
    }
    return flag;
  }

  public void addPhaseListener(PhaseListener phaseListener)
  {
    if (phaseListener == null)
    {
      throw new NullPointerException("PhaseListener must not be null.");
    }
    synchronized (phaseListenerList)
    {
      phaseListenerList.add(phaseListener);
      phaseListenerArray = null; // reset lazy cache array
    }
  }

  public void removePhaseListener(PhaseListener phaseListener)
  {
    if (phaseListener == null)
    {
      throw new NullPointerException("PhaseListener must not be null.");
    }
    synchronized (phaseListenerList)
    {
      phaseListenerList.remove(phaseListener);
      phaseListenerArray = null; // reset lazy cache array
    }
  }

  public PhaseListener[] getPhaseListeners()
  {
    synchronized (phaseListenerList)
    {
      // (re)build lazy cache array if necessary
      if (phaseListenerArray == null)
      {
        phaseListenerArray = phaseListenerList.toArray(new PhaseListener[phaseListenerList.size()]);
      }
      return phaseListenerArray;
    }
  }
}
