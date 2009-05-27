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
package org.apache.myfaces.trinidad.change;

import java.io.Serializable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.CollectionUtils;
import org.apache.myfaces.trinidad.util.ComponentUtils;

import org.w3c.dom.Document;


/**
 * A ChangeManager implementation that manages
 *  persisting the added Changes at the session. This means
 *  the lifetime of Changes added such is within the session scope.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/change/SessionChangeManager.java#0 $) $Date: 10-nov-2005.19:06:35 $
 */
public class SessionChangeManager extends BaseChangeManager
{
  /**
   * {@inheritDoc}
   * @param facesContext The FacesContext instance for the current request.
   */
  @Override
  public void applyComponentChangesForCurrentView(FacesContext facesContext)
  {
    _applyComponentChanges(facesContext, null);
  }

  /**
   * {@inheritDoc}
   */
   @Override
  public void applyComponentChangesForSubtree(
    FacesContext facesContext,
    NamingContainer root
    )
  {
    String rootId = null;
    
    if (root != null)
    {
      if (!(root instanceof UIComponent))
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "INVALID_TYPE", root));
      }
      
      rootId = ComponentUtils.getScopedIdForComponent((UIComponent)root, null);
    }

    _applyComponentChanges(facesContext, rootId);
  }

  /**
   * Adds a ComponentChange and registers against the supplied component.
   * Changes added thus live at Session scope.
   * Use applyComponentChangesForCurrentView() to apply these changes.
   * @param facesContext The FacesContext instance for the current request.
   * @param targetComponent The target component against which this change needs 
   * to be registered and applied later on.
   * @param componentChange The ComponentChange to add.
   */
  protected void addComponentChangeImpl(
    FacesContext facesContext,
    UIComponent targetComponent,
    ComponentChange componentChange)
  {
    String viewId = facesContext.getViewRoot().getViewId();
    
    // get the ComponentChanges for the current viewId
    ChangesForView changesForView = _getChangesForView(facesContext, viewId, true);

    // get the absolute scopedId for the target component so that we have a unique identifier
    // to compare
    String scopedIdForTargetComponent = 
                                     ComponentUtils.getScopedIdForComponent(targetComponent, null);

    // try to collapse AttributeComponentChanges, handling component movement so that
    // we can collapse any attribute change on the same component
    if (componentChange instanceof AttributeComponentChange)
    {
      AttributeComponentChange attributeChange = (AttributeComponentChange)componentChange;
      String attributeName = attributeChange.getAttributeName();
 
      // would really rather use a Deque here and iterate backwards, which would also make
      // handling the rename changes easier
      Iterator<QualifiedComponentChange> changes =
                                            changesForView.getComponentChangesForView().iterator();
      
      // list of changes that have renamed the scoped id of this component.  We need to
      // handle this aliasing when traversing through the changes looking for matches
      Iterator<MoveChildComponentChange> renameChanges =
                                       changesForView.getRenameChanges(scopedIdForTargetComponent);
      
      // we need to look through the rename list to map from the current names to
      // the new names
      MoveChildComponentChange nextRenameChange;
      String currTargetScopedId;
      
      if (renameChanges.hasNext())
      {
        // we have at least one rename change, so get it and find the name that this
        // component was originally known by
        nextRenameChange = renameChanges.next();
        currTargetScopedId = nextRenameChange.getSourceScopedId();
      }
      else
      {
        nextRenameChange = null;
        currTargetScopedId = scopedIdForTargetComponent;
      }
      
      // loop forward through the changes looking for AttributeChanges to collapse
      while (changes.hasNext())
      {
        QualifiedComponentChange currQualifiedChange = changes.next();
        
        if (currQualifiedChange.getComponentChange() == nextRenameChange)
        {
          // we got a match, so update the scoped id we should be looking for
          currTargetScopedId = nextRenameChange.getDestinationScopedId();
          
          nextRenameChange = (renameChanges.hasNext())
                               ? renameChanges.next()
                               : null;
        }
        else if (currQualifiedChange.getTargetComponentScopedId().equals(currTargetScopedId))
        {
          // found a change on the same component.  Check if it's an AttributeChange
          ComponentChange currChange = currQualifiedChange.getComponentChange();
          
          if (currChange instanceof AttributeComponentChange)
          {
            AttributeComponentChange currAttributeChange = (AttributeComponentChange)currChange;
            
            // Check if the AttributeChange is for the same attribute
            if (attributeName.equals(currAttributeChange.getAttributeName()))
            {
              // the old AttributeChange is for the same attribute, so remove it since the
              // new AttributeChange would eclipse it anyway.
              changes.remove();
              break;
            }
          }
        }
      }
    }

    QualifiedComponentChange newQualifiedChange = new QualifiedComponentChange(
                                                                      scopedIdForTargetComponent,
                                                                      componentChange);
    
    changesForView.addChange(newQualifiedChange);
  }

  /** 
   * We don't support DocumentChange persistence
   */
  @Override
  protected Document getDocument(FacesContext context)
  {
    return null;
  }
  
  /**
   * Implementation shared by applyComponentChangesForCurrentView() and
   * applyComponentChangesForSubtree().
   * @param facesContext The FacesContext instance for this request.
   * @param rootId The scoped id of theNamingContainer that contains the 
   * component subtree to which ComponentChanges should be applied.  If null, 
   * all changes are applied.
   */
  private void _applyComponentChanges(
    FacesContext facesContext,
    String       rootId
    )
  {
    UIViewRoot viewRoot = facesContext.getViewRoot();
    
    // retrieve the ComponentChanges for this current viewid
    ChangesForView changesForView = _getChangesForView(facesContext, viewRoot.getViewId(), false);
    
    // loop through the viewId's changes, applying the changes
    for (QualifiedComponentChange qualifiedChange : changesForView.getComponentChangesForView())
    {
      if (!_acceptChange(qualifiedChange, rootId))
        continue;

      UIComponent targetComponent = 
        viewRoot.findComponent(qualifiedChange.getTargetComponentScopedId());
      
      // Possible that the target component no more exists in the view
      if (targetComponent != null)
      {
        ComponentChange componentChange = qualifiedChange.getComponentChange();
        componentChange.changeComponent(targetComponent);
      }
      else
      {
        _LOG.info(this.getClass().getName(),
                  "applyComponentChangesForCurrentView",
                  "TARGET_COMPONENT_MISSING_CHANGE_FAILED",
                  qualifiedChange.getTargetComponentScopedId());
      }
    }    
  }

  /**
   * Tests whether the specified change should be applied based on the
   * specified root id.  If root id is null, all changes are accepted/applied.
   * If the root id is non-null, only changes which target ids underneath
   * the root id are accepted/applied.
   * 
   * @param qualifiedChange the change to test
   * @param rootId the scoped id of the NamingContainer for which we
   *   are applying changes
   * @return true if rootId is null, or if the qualifiedChange targets a
   *   component underneath the NamingContainer identified by the rootId.
   */
  private boolean _acceptChange(
    QualifiedComponentChange qualifiedChange,
    String rootId
    )
  {
    boolean accept = true;

    if (rootId != null)
    {
      String id = qualifiedChange.getTargetComponentScopedId();
      accept = (id.startsWith(rootId) && (id.length() != rootId.length()));    
    }
    
    return accept;
  }

  /**
   * Gets the in-order list of component changes for the given view.
   * @param facesContext The FacesContext instance for this request.
   * @param viewId The id of the view for which changes are required.
   * @param createIfNecessary Indicates whether the underlying datastructures
   * that store the component changes needs to be created if absent.
   * @return The ChangesForView object containing information about the changes for the specified
   * viewId, including in-order list of component changes for the supplied view. This
   * will be in the same order in which the component changes were added through
   * calls to <code>addComponentChange()</code>.
   */
  private ChangesForView _getChangesForView(
    FacesContext facesContext,
    String viewId,
    boolean createIfNecessary)
  {
    Object session = facesContext.getExternalContext().getSession(true);
    
    // Key is view id and value is list of component changes for that view
    ConcurrentHashMap<String, ChangesForView> componentChangesMapForSession;
    
    synchronized(session)
    {
      Map<String, Object> sessMap = 
        facesContext.getExternalContext().getSessionMap();
      
      componentChangesMapForSession = (ConcurrentHashMap<String, ChangesForView>)
                                       sessMap.get(_COMPONENT_CHANGES_MAP_FOR_SESSION_KEY);
      
      if (componentChangesMapForSession == null)
      {
        if (!createIfNecessary)
          return _EMPTY_CHANGES;
  
        componentChangesMapForSession = new ConcurrentHashMap<String, ChangesForView>();
        sessMap.put(_COMPONENT_CHANGES_MAP_FOR_SESSION_KEY, 
                    componentChangesMapForSession);
      }
    }

    if (!componentChangesMapForSession.containsKey(viewId))
    {
      if (!createIfNecessary)
        return _EMPTY_CHANGES;
      
      componentChangesMapForSession.putIfAbsent(viewId, new ChangesForView(true));
    }
    
    return componentChangesMapForSession.get(viewId);
  }
  
  /**
   * Tracks the component changes for a particular view as well as all the movement
   * changes so that component aliasing can be tracked
   */
  private static final class ChangesForView implements Serializable
  {
    protected ChangesForView(boolean rw)
    {      
      if (rw)
      {
        _componentChangesForView = new ConcurrentLinkedQueue<QualifiedComponentChange>();
        _renameChanges = new CopyOnWriteArrayList<MoveChildComponentChange>();
      }
      else
      {
        _componentChangesForView = CollectionUtils.emptyQueue();
        _renameChanges = null;
      }
    }
    
    /** 
     * Returns the QualifiedComponentChanges for this viewId
     */
    protected Iterable<QualifiedComponentChange> getComponentChangesForView()
    {
      return _componentChangesForView;
    }
    
    /** 
     * Adds a change to the QualifiedComponentChanges for this viewId, handling
     * MoveChildComponentChanges specially to handle cases where the clientId
     * of a component changes as a result of a rename operation
     */
    protected void addChange(QualifiedComponentChange qualifiedChange)
    {
      _componentChangesForView.add(qualifiedChange);
      
      ComponentChange componentChange = qualifiedChange.getComponentChange();
      
      if (componentChange instanceof MoveChildComponentChange)
      {
        // we only need to remove moves that actually changed the absolute scoped id of the
        // component
        MoveChildComponentChange moveComponentChange = (MoveChildComponentChange)componentChange;
        
        if (!moveComponentChange.getSourceScopedId().equals(moveComponentChange.getDestinationScopedId()))
        {
          _renameChanges.add(moveComponentChange);
        }
      }
    }

    /**
     * Returns the Iterator of rename changes that affect the current scoped id in ComponentChange order
     * @return
     */
    protected Iterator<MoveChildComponentChange> getRenameChanges(String targetScopedId)
    {
      if (_renameChanges != null)
      {
        String currTargetScopedId = targetScopedId;
        List renameChanges = null;
        
        // iterate from the back of the List determining the MoveChildComponentChange
        // that are aliased to this scoped id
        ListIterator<MoveChildComponentChange> moveChanges =
                                                _renameChanges.listIterator(_renameChanges.size());
        
        while (moveChanges.hasPrevious())
        {
          MoveChildComponentChange currMoveChange = moveChanges.previous();
          
          if (currTargetScopedId.equals(currMoveChange.getDestinationScopedId()))
          {
            // lazily create the list the first time we need it
            if (renameChanges == null)
              renameChanges = new ArrayList<MoveChildComponentChange>();
            
            renameChanges.add(currMoveChange);
            
            // get the new id to search for
            currTargetScopedId = currMoveChange.getSourceScopedId();
          }
        }
        
        if (renameChanges != null)
        {
          if (renameChanges.size() > 1)
          {
            // reverse the list to match the order that we will see these items when traversing
            // the changes from the forward direction
            Collections.reverse(renameChanges);
          }
          
          return renameChanges.iterator();
        }  
      }
      
      return CollectionUtils.emptyIterator();
    }
    
    private final Queue<QualifiedComponentChange> _componentChangesForView;
    private final List<MoveChildComponentChange> _renameChanges;

    private static final long serialVersionUID = 1L;
  }
  
  private static final ChangesForView _EMPTY_CHANGES = new ChangesForView(false);
    
  private static class QualifiedComponentChange implements Serializable
  {
    public QualifiedComponentChange(String targetComponentScopedId,
                                    ComponentChange componentChange)
    {
      // NO-TRANS : Class is private and inner, no translated message required
      if (targetComponentScopedId == null || componentChange == null)
        throw new IllegalArgumentException("Target component scoped id and " +
                                           "component change is required");
      
      _targetComponentScopedId = targetComponentScopedId;
      _componentChange = componentChange;
    }
    
    public String getTargetComponentScopedId()
    {
      return _targetComponentScopedId;
    }

    public ComponentChange getComponentChange()
    {
      return _componentChange;
    }

    private final String _targetComponentScopedId;
    private final ComponentChange _componentChange;

    private static final long serialVersionUID = 1L;
  }
  
  private static final String _COMPONENT_CHANGES_MAP_FOR_SESSION_KEY =
    "org.apache.myfaces.trinidadinternal.ComponentChangesMapForSession";
    
  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(SessionChangeManager.class);
}
