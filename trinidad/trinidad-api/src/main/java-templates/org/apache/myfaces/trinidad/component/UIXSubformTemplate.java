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

import javax.faces.component.NamingContainer;
import javax.faces.context.FacesContext;

import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseId;

/**
 * Base class for Subform component.
 * <p>
 * @version $Name:  $ ($Revision$) $Date$
 */
abstract public class UIXSubformTemplate extends UIXComponentBase
                                        implements NamingContainer
{
/**/ // Abstract methods implemented by code gen
/**/  abstract public boolean isSubmitted();
/**/  abstract public void setSubmitted(boolean submitted);
/**/  abstract public boolean isDefault();

  @Override
  public void queueEvent(FacesEvent event)
  {
    // If the event is being queued for anything *after* APPLY_REQUEST_VALUES,
    // then this subform is active.
    if (PhaseId.APPLY_REQUEST_VALUES.compareTo(event.getPhaseId()) < 0)
    {
      _storeSomethingSubmitted(FacesContext.getCurrentInstance());
      setSubmitted(true);
    }

    super.queueEvent(event);
  }

  @Override
  public void processDecodes(FacesContext context)
  {
    setSubmitted(false);
    super.processDecodes(context);
  }

  @Override
  public void processValidators(FacesContext context)
  {
    if (!isSubmitted() && isDefault() && !_isSomethingSubmitted(context))
      setSubmitted(true);

    if (isSubmitted())
      super.processValidators(context);
  }

  @Override
  public void processUpdates(FacesContext context)
  {
    if (isSubmitted())
      super.processUpdates(context);
  }

  @SuppressWarnings("unchecked")
  static private void _storeSomethingSubmitted(FacesContext context)
  {
    context.getExternalContext().getRequestMap().put(_SOMETHING_SUBMITTED,
                                                     Boolean.TRUE);
  }

  static private boolean _isSomethingSubmitted(FacesContext context)
  {
    return Boolean.TRUE.equals(context.getExternalContext().
                               getRequestMap().get(_SOMETHING_SUBMITTED));
  }

  static private final String _SOMETHING_SUBMITTED =
    "org.apache.myfaces.trinidad.component.UIXSubformSubmitted";
}
