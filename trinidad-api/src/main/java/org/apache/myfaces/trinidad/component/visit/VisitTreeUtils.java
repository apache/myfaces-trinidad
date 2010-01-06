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

import java.util.Collections;

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
    VisitContext visitContext = RequestContext.getCurrentInstance().createVisitContext(facesContext,
      Collections.singleton(clientId), null, null);
    return UIXComponent.visitTree(visitContext, facesContext.getViewRoot(), visitCallback);
  }
}
