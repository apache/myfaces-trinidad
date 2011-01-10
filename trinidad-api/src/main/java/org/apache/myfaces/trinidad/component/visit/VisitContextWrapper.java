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

package org.apache.myfaces.trinidad.component.visit;

import java.util.Collection;
import java.util.Set;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;


/**
 * A port of the JSF 2 <code>VisitContextWrapper</code> class so that we may be able to use
 * its functionality in JSF 1.2 with Trinidad 1.2
 */
public abstract class VisitContextWrapper
  extends VisitContext
{
  /**
   * Get the wrapped visit context
   */
  public abstract VisitContext getWrapped();

  @Override
  public FacesContext getFacesContext()
  {
    return getWrapped().getFacesContext();
  }

  @Override
  public PhaseId getPhaseId()
  {
    return getWrapped().getPhaseId();
  }

  @Override
  public Set<VisitHint> getHints()
  {
    return getWrapped().getHints();
  }

  @Override
  public Collection<String> getIdsToVisit()
  {
    return getWrapped().getIdsToVisit();
  }

  @Override
  public Collection<String> getSubtreeIdsToVisit(
    UIComponent component)
  {
    return getWrapped().getSubtreeIdsToVisit(component);
  }

  @Override
  public VisitResult invokeVisitCallback(
    UIComponent   component,
    VisitCallback callback)
  {
    return getWrapped().invokeVisitCallback(component, callback);
  }
}
