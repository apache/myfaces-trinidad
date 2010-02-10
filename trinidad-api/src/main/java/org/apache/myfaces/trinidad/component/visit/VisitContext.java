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

import java.util.AbstractCollection;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;

/**
 *
 * <p>A context object that is used to hold
 * state relating to a component tree visit.</p>
 *
 * <p>Component tree visits are initiated by calling
 * {@link org.apache.myfaces.trinidad.component.UIXComponent#visitTree(VisitContext,UIComponent,VisitCallback)
 * UIComponent.visitTree()},
 * at which point both a {@link VisitContext} and a {@link VisitCallback}
 * must be provided.
 *
 * @see org.apache.myfaces.trinidad.component.UIXComponent#visitTree(VisitContext,UIComponent,VisitCallback) UIXComponent.visitTree()
 * @see VisitCallback
 *
 */
abstract public class VisitContext
{
  // Design notes: The VisitContext contract could be defined
  // as an interface.  However, there is the potential that we
  // may need to add new methods in the future, so leaving as 
  // an abstract class in order to have room to grow.
  // 
  // Since we are an abstract class rather than an interface,
  // we could provide implementations of of some of the simpler
  // methods (eg. getFacesContext() and getHints()) to avoid 
  // duplicating this code in VisitContext implementations.
  // However, doing so would mean that "wrapping" VisitContext
  // implementations would be forced to pick up such implementations,
  // so going with a pure contract (no implementation).

  /**
   * <p>This unmodifiable Collection is returned by 
   * VisitContext.getIdsToVisit() and getSubtreeIdsToVisit( in cases where all ids
   * should be visited.</p>
   * <p>To simplify logic for visitTree() implementations, this Collection
   * always return {@code false} for {@code isEmpty}.  All other methods 
   * throw {@code UnsupportedOperationException}.</p>
   */
  // Note: We cannot use Collections.emptyList() as that returns
  // a shared instance - we want to unique instance to allow for
  // identity tests.
  static public final Collection<String> ALL_IDS = 
    new AbstractCollection<String>()
    {
      @Override
      public Iterator<String> iterator()
      {
        throw new UnsupportedOperationException(
            "VisitContext.ALL_IDS does not support this operation");
      }

      @Override
      public int size()
      {
        throw new UnsupportedOperationException(
                    "VisitContext.ALL_IDS does not support this operation");
      }

      @Override
      public boolean isEmpty()
      {
        return false;
      }
    };

  /**
   * <p>Returns the FacesContext for the current request.</p>
   */
  abstract public FacesContext getFacesContext();
  
  /**
   * <p>Returns the PhaseId, if any that, that this visit is ocurring under
   * @return the current PhaseId
   */
  public abstract PhaseId getPhaseId();

  /**
   * <p>
   * Returns the ids of the components to visit.
   * </p>
   * <p>
   * In the case of a full tree visit, this method returns the
   * ALL_IDS collection.  Otherwise, if a partial visit is being
   * performed, returns a modifiable collection containing the
   * client ids of the components that should be visited.
   * </p>
   * @return {@code VisitContext.ALL_IDS}, or a modifiable 
   * Collection of client ids.
   */
  abstract public Collection<String> getIdsToVisit();

  /**
   * <p>
   * Given a NamingContainer component, returns the client ids of 
   * any components underneath the NamingContainer that should be
   * visited.
   * </p>
   * <p>
   * This method is called by NamingContainer visitTree() implementations 
   * to determine whether the NamingContainer contains components to be 
   * visited.  In the case where no such components exist, the 
   * NamingContainer can short-circuit the tree visit and avoid 
   * descending into child subtrees.
   * </p>
   * <p>
   * In addition, iterating components such as UIData may be able to
   * use the returned ids to determine which iterated states (ie. rows)
   * need to be visited.  This allows the visit traversal to be
   * contstrained such only those rows that contain visit targets
   * need to be traversed.
   * </p>
   * @param component a NamingContainer component
   * @return an unmodifiable Collection containing the client ids of 
   *   any components underneath the NamingContainer that are known to be
   *   targets of the tree visit.  If no such components exist, returns 
   *   an empty Collection.  If all components underneath the 
   *   NamingContainer should be visited, returns the
   *   {@code VisitContext.ALL_IDS} collection.
   * @throws IllegalArgumentException if {@code component} is not
   *  an instance of NamingContainer
   */
  abstract public Collection<String> getSubtreeIdsToVisit(UIComponent component);

  /**
   * <p>Called by {@link org.apache.myfaces.trinidad.component.UIXComponent#visitTree UIXComponent.visitTree()}
   * to visit a single component.</p>
   *
   * @param component the component to visit
   * @param callback the VisitCallback to call
   * @return a VisitResult value that indicates whether to continue
   *   visiting the component's subtree, skip visiting the component's
   *   subtree or abort the visit altogether.
   */
  abstract public VisitResult invokeVisitCallback(UIComponent component, 
                                                  VisitCallback callback); 

  /**
   * <p>Returns hints that influence the behavior of the tree visit.</p>
   *
   * <p>Interested parties, such as 
   * {@link org.apache.myfaces.trinidad.component.UIXComponent#visitTree UIComponent.visitTree()} implementations,
   * may check to see whether a particular hint is present by calling
   * {@code VisitContext.getHints().contains()}, passing in one of the
   * hints defined by {@link VisitHint}.
   *   
   * @return a non-empty, unmodifiable collection of VisitHints
   */
  abstract public Set<VisitHint> getHints();
}
