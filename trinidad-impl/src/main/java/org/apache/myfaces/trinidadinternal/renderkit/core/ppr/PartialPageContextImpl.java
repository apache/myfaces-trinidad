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
package org.apache.myfaces.trinidadinternal.renderkit.core.ppr;

import java.util.Iterator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;
import org.apache.myfaces.trinidad.context.PartialPageContext;

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
 */
public class PartialPageContextImpl extends PartialPageContext
{
  PartialPageContextImpl()
  {
    _targets = new HashMap<String, Boolean>();
    _renderedTargets = new HashSet<String>();

    // Pre-allocate the rendered stack
    _currentTargetStack = new Stack<String>();
  }

  /**
   * Creates a PartialPageContext to use to render the partial targets with
   * the specified ids.
   */
  public PartialPageContextImpl(
    RequestContext reqContext)
  {
    this();

    // Components may add themselves to the partialTargets list in earlier
    // phases (we don't get here until render response). If so, the IDs have
    // been kept on the RequestContext. We'll grab them now and add them to the
    // target list.
    RequestContextImpl requestContext =
      (RequestContextImpl) reqContext;
    Iterator<String> targetIter = requestContext.getPartialTargets();
    while (targetIter.hasNext())
      _targets.put(targetIter.next(), Boolean.FALSE);

    if (_targets.isEmpty())
    {
      _LOG.fine("PPR is about to render without any targets");
    }
  }

  /**
   * Returns the set of partial targets for this rendering pass.
   */
  @Override
  public Iterator<String> getPartialTargets()
  {
    return _targets.keySet().iterator();
  }



  /**
   * Tests whether the specified id is the client id of a UIComponent that
   * should be rendered as part of the partial rendering pass.
   */
  @Override
  public boolean isPartialTarget(String id)
  {
    return (id != null) && _targets.containsKey(id);
  }

  /**
   * Tests whether the specified partial target has been rendered.
   */
  @Override
  public boolean isPartialTargetRendered(String id)
  {
    return _renderedTargets.contains(id);
  }

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
  @Override
  public void addPartialTarget(String id)
  {
    _targets.put(id, Boolean.FALSE);
  }

  /**
   * Returns true if we are inside of a partial target.
   */
  @Override
  public boolean isInsidePartialTarget()
  {
    return _getCurrentPartialTarget() != null;
  }

  /**
   * Adds a partial target that has already been rendered;  this
   * is needed if the "clientId" of a component does not match
   * up to the top element (or elements).
   */
  @Override
  public void addRenderedPartialTarget(String id)
  {
    _renderedTargets.add(id);
  }

  @Override
  public Iterator<String> getRenderedPartialTargets()
  {
    return _renderedTargets.iterator();
  }

  /**
   * Notifies the PartialPageContext that the specified partial target is
   * about to be rendered.
   * <p>
   * This method is called automatically by Trinidad during the partial
   * rendering pass when a partial target is about to be rendered.
   * Clients should never need to call this method.
   *
   * @param context the current FacesContext
   * @param id The ID of the partial target that is about to be rendered
   * @see #popRenderedPartialTarget
   */
  public void pushRenderedPartialTarget(
    String id
    )
  {
    if (_LOG.isFine())
    {
      if (!_targets.containsKey(id))
        _LOG.fine("Rendering partial target {0}, which was not requested", id);
    }

    _targets.put(id, Boolean.TRUE);
    _currentTargetStack.push(id);

    if (_LOG.isFiner())
    {
      _LOG.finer("Pushed rendered PPR target " + id);
    }
  }

  /**
   * Notifies the PartialPageContext that the current partial target
   * has finished rendering.
   * <p>
   * This method is called automatically by Trinidad during the partial
   * rendering pass when a partial target has finished rendering.
   * Clients should never need to call this method.
   *
   * @param context the current FacesContext
   */
  public void popRenderedPartialTarget()
  {
    _currentTargetStack.pop();
  }


  /**
   * Returns the ID of the partial target that is currently being
   * rendered, if any.
   */
  private String _getCurrentPartialTarget()
  {
    if (_currentTargetStack.empty())
      return null;

    return _currentTargetStack.peek();
  }

  private Map<String, Boolean> _targets;
  private Set<String> _renderedTargets;

  // The stack of partial targets that are currently being rendered
  // -= Simon Lessard =-
  // FIXME: java.util.Stack... enough said... ArrayList or LinkedList please
  private Stack<String> _currentTargetStack;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(PartialPageContextImpl.class);
}
