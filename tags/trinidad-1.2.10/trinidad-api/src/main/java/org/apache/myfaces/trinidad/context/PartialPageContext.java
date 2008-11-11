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
package org.apache.myfaces.trinidad.context;

import java.util.Iterator;


/**
 * Context object which is used to track the targets of a partial
 * page render during the Render Response phase.
 * Clients never need to explicitly create PartialPageContext
 * objects, but can retrieve them from a RenderingContext instance.
 * For general access to Partial Page Rendering during all phases,
 * see APIs on the RequestContext API.
 * <p>
 * During the partial rendering pass, some Renderer implementations
 * may modify the set of partial targets that are rendered.
 * (For example, the FormRenderer adds a partial target for its
 * shared hidden fields.)
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/ppr/PartialPageContext.java#0 $) $Date: 10-nov-2005.19:02:58 $
 */
abstract public class PartialPageContext
{
  protected PartialPageContext()
  {
  }

  /**
   * Tests whether the specified id is the client id of a UIComponent that
   * should be rendered as part of the partial rendering pass.
   */
  abstract public boolean isPartialTarget(String id);

  /**
   * Returns the set of partial targets for this rendering pass.
   */
  abstract public Iterator<String> getPartialTargets();

  /**
   * Tests whether the specified partial target has been rendered.
   */
  abstract public boolean isPartialTargetRendered(String id);

  /**
   * Adds a new partial target to render.
   * <p>
   * This method may be called during the partial rendering pass to
   * add to the set of partial targets, but only if the pass has
   * not yet been completed.
   * @param id The id of the partial target to render
   */
  abstract public void addPartialTarget(String id);

  /**
   * Returns true if we are inside of a partial target.
   */
  abstract public boolean isInsidePartialTarget();

  /**
   * Adds a partial target that has already been rendered;  this
   * is needed if the "clientId" of a component does not match
   * up to the top element (or elements).
   */
  abstract public void addRenderedPartialTarget(String id);

  abstract public Iterator<String> getRenderedPartialTargets();
}
