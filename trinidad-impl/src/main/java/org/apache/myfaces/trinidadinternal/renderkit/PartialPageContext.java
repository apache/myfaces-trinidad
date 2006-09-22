/*
 * Copyright  2001-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidadinternal.renderkit;

import java.util.Iterator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Context object which is used to track the targets of a partial
 * page render during the partial page rendering pass.
 * Clients never need to explicitly create PartialPageContext
 * objects.
 * <p>
 * During the partial rendering pass, some Renderer implementations
 * may modify the set of partial targets that are rendered.
 * (For example, the FormRenderer adds a partial target for its
 * shared hidden fields if any children of the form are rendered.)
 * After the partial render pass, getPartialTargets() can be
 * called to determine the actual set of partial targets that were
 * rendered.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/ppr/PartialPageContext.java#0 $) $Date: 10-nov-2005.19:02:58 $
 * @author The Oracle ADF Faces Team
 */
abstract public class PartialPageContext
{
  protected PartialPageContext()
  {
  }

  abstract public void finish();

  /**
   * Returns the set of partial targets for this rendering pass.
   */
  abstract public Iterator<String> getPartialTargets();

  /**
   * Tests whether the specified id is the client id of a UIComponent that
   * should be rendered as part of the partial rendering pass.
   */
  abstract public boolean isPartialTarget(String id);

  /**
   * Tests whether the specified partial target has been rendered.
   */
  abstract public boolean isPartialTargetRendered(String id);

  /**
   * Tests whether any targets were rendered in this pass.
   */
  abstract public boolean hasRenderedTargets();

  /**
   * Adds a new partial target to render.
   * <p>
   * This method may be called during the partial rendering pass to
   * add to the set of partial targets, but only if the pass has
   * not yet been completed.  Clients should first check to see
   * whether the partial rendering pass has finished by calling
   * isPartialPassComplete() before calling this method.
   *
   * @param id The id of the partial target to render
   * @see #isPartialPassComplete
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

  /**
   * Tests whether all of the partial targets for this tree have been rendered.
   */
  abstract public boolean isPartialPassComplete();

  /**
   * Notifies the PartialPageContext that the specified partial target is
   * about to be rendered.
   * <p>
   * This method is called automatically by ADF Faces during the partial
   * rendering pass when a partial target is about to be rendered.
   * Clients should never need to call this method.
   *
   * @param context the current FacesContext
   * @param id The ID of the partial target that is about to be rendered
   * @see #popRenderedPartialTarget
   */
  abstract public void pushRenderedPartialTarget(
    String id);

  /**
   * Notifies the PartialPageContext that the current partial target
   * has finished rendering.
   * <p>
   * This method is called automatically by ADF Faces during the partial
   * rendering pass when a partial target has finished rendering.
   * Clients should never need to call this method.
   *
   * @param context the current FacesContext
   */
  abstract public void popRenderedPartialTarget();
}
