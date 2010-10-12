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

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.Iterator;
import java.util.Queue;

import javax.faces.context.FacesContext;


/**
 * Default, internal, implementation of the {@link ComponentContextManager} for use from the
 * {@link RequestContext} class.
 */
final class ComponentContextManagerImpl
  extends ComponentContextManager
{
  public void pushChange(ComponentContextChange change)
  {
    if (_stack == null)
    {
      _stack = Collections.asLifoQueue(new ArrayDeque<ComponentContextChange>());
    }

    _stack.offer(change);
  }

  public ComponentContextChange popChange()
    throws IllegalStateException
  {
    if (_stack == null)
    {
      throw new IllegalStateException("No changes to pop");
    }

    ComponentContextChange change = _stack.poll();
    if (_stack.isEmpty())
    {
      _stack = null;
    }

    return change;
  }

  public ComponentContextChange peekChange()
  {
    return _stack == null ? null : _stack.peek();
  }

  public SuspendedContextChanges suspend(FacesContext facesContext)
  {
    if (_stack == null)
    {
      return new SuspendedContextChangesImpl(new ArrayDeque<ComponentContextChange>(0));
    }

    ArrayDeque<ComponentContextChange> q = new ArrayDeque<ComponentContextChange>(_stack.size());
    for (ComponentContextChange change : _stack)
    {
      change.suspend(facesContext);
      q.offer(change);
    }

    _stack = null;

    return new SuspendedContextChangesImpl(q);
  }

  public SuspendedContextChanges partialSuspend(
    FacesContext    facesContext,
    SuspendCallback callback)
  {
    if (_stack == null)
    {
      return new SuspendedContextChangesImpl(new ArrayDeque<ComponentContextChange>(0));
    }

    ArrayDeque<ComponentContextChange> q = new ArrayDeque<ComponentContextChange>(_stack.size());
    for (Iterator<ComponentContextChange> iter = _stack.iterator();
         iter.hasNext(); )
    {
      ComponentContextChange change = iter.next();
      SuspendCallback.SuspendResult result = callback.getSuspendResult(change);

      if (result == SuspendCallback.SuspendResult.STOP)
      {
        break;
      }

      iter.remove();
      change.suspend(facesContext);
      q.offer(change);

      if (result == SuspendCallback.SuspendResult.STOP_AFTER_CURRENT)
      {
        break;
      }
    }

    if (_stack.isEmpty())
    {
      _stack = null;
    }

    return new SuspendedContextChangesImpl(q);
  }

  public Iterator<ComponentContextChange> resume(
    FacesContext            facesContext,
    SuspendedContextChanges suspendedChanges)
  {
    assert suspendedChanges instanceof SuspendedContextChangesImpl :
      "Invalid suspend changes";

    SuspendedContextChangesImpl suspendedChangesImpl =
      (SuspendedContextChangesImpl)suspendedChanges;

    if (_stack == null)
    {
      _stack = Collections.asLifoQueue(new ArrayDeque<ComponentContextChange>());
    }

    for (Iterator<ComponentContextChange> iter =
           suspendedChangesImpl._suspendedStack.descendingIterator();
         iter.hasNext(); )
    {
      ComponentContextChange change = iter.next();
      change.resume(facesContext);
      _stack.offer(change);
    }

    return suspendedChangesImpl._suspendedStack.descendingIterator();
  }

  private static class SuspendedContextChangesImpl
    extends SuspendedContextChanges
  {
    SuspendedContextChangesImpl(
      Deque<ComponentContextChange> suspendedStack)
    {
      _suspendedStack = suspendedStack;
    }

    private Deque<ComponentContextChange> _suspendedStack;
  }

  private Queue<ComponentContextChange> _stack;
}