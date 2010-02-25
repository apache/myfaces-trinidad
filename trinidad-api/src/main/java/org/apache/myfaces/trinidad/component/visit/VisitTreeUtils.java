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
import java.util.Collections;

import java.util.Set;

import javax.faces.FactoryFinder;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitContextFactory;
import javax.faces.component.visit.VisitHint;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.context.RequestContext;


/**
 * Utility methods to make usage of the visit tree functionality more user friendly.
 */
public final class VisitTreeUtils
{
  private VisitTreeUtils() {}

  /**
   * <p>Creates a VisitContext instance for use with
   * {@link org.apache.myfaces.trinidad.component.UIXComponent#visitTree UIComponent.visitTree()}.</p>
   *
   * @param context the FacesContext for the current request
   * @param ids the client ids of the components to visit.  If null,
   *   all components will be visited.
   * @param hints the VisitHints to apply to the visit
   * @return a VisitContext instance that is initialized with the
   *   specified ids and hints.
   */
  public static VisitContext createVisitContext(
    FacesContext context,
    Collection<String> ids,
    Set<VisitHint> hints)
  {
    VisitContextFactory factory = (VisitContextFactory)
                                  FactoryFinder.getFactory(FactoryFinder.VISIT_CONTEXT_FACTORY);
    
    return factory.getVisitContext(context, ids, hints);
  }

  /**
   * Visit a single component in the component tree starting from the view root.
   * <p>Method assumes the {@link RequestContext} is available and a view root must be set on the faces context.</p>
   *
   * @param facesContext the faces context
   * @param clientId the client ID of the component to visit
   * @param visitCallback the callback to be invoked if the component is found
   * @return true if a component was visited
   */
  public static boolean visitSingleComponent(
    FacesContext  facesContext,
    String        clientId,
    VisitCallback visitCallback)
  {
    VisitContext visitContext = createVisitContext(facesContext, Collections.singleton(clientId), null);
    return UIXComponent.visitTree(visitContext, facesContext.getViewRoot(), visitCallback);
  }
}
