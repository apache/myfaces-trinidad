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

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.CollectionUtils;
import org.apache.myfaces.trinidad.util.ComponentUtils;

import org.apache.myfaces.trinidad.webapp.UIXComponentELTag;

import org.w3c.dom.Document;


/**
 * A ChangeManager implementation that manages persisting the added Changes at the session. 
 * This means the lifetime of Changes added such is within the session scope. If any of the changes
 * are managed as state changes and restored by JSF state saving mechanism, the SessionChangeManager
 * will not re-apply such changes. For example: AttributeComponentChanges are not applied during
 * a postback unless its target component happened to be a result of any move/add operation, this is
 * because attribute changes are handled by state manager during postback for common cases.
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
      
      rootId = ComponentUtils.getScopedIdForComponent((UIComponent)root, facesContext.getViewRoot());
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
    // get the absolute scopedId for the target component so that we have a unique identifier
    // to compare
    String scopedIdForTargetComponent = 
      ComponentUtils.getScopedIdForComponent(targetComponent, facesContext.getViewRoot());

    // try to collapse AttributeComponentChanges, handling component movement so that
    // we can collapse any attribute change on the same component
    if (componentChange instanceof AttributeComponentChange)
    {
      AttributeComponentChange attributeChange = (AttributeComponentChange)componentChange;
      _extractAttributeChange(facesContext, 
                              scopedIdForTargetComponent, 
                              attributeChange);
    }

    _insertComponentChange(facesContext, scopedIdForTargetComponent, componentChange);
  }
  
  /**
   * @inheritDoc
   */
  @Override
  public AttributeComponentChange replaceAttributeChangeIfPresent(FacesContext facesContext,
    UIComponent uiComponent,
    AttributeComponentChange attributeComponentChange)
  {    
    // get the absolute scopedId for the target component so that we have a unique identifier
    // to compare
    String scopedIdForTargetComponent = 
      ComponentUtils.getScopedIdForComponent(uiComponent, null);
    
    // check if we have an existing attribute change for the same attribute name, 
    // if found, remove it
    AttributeComponentChange replaced = 
      _extractAttributeChange(facesContext, 
                              scopedIdForTargetComponent, 
                              attributeComponentChange);
    
    // if found, we insert the new change instance
    if (replaced != null)
    {
      _insertComponentChange(facesContext, scopedIdForTargetComponent, attributeComponentChange);
    }
    
    return replaced;
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
   * Check if we have an existing attribute change for the same attribute name: 
   * - if not found, return null
   * - if found, remove and return the old change instance
   * 
   * @param facesContext
   * @param uiComponent
   * @param attributeChange
   * @return the old change instance, null if not found
   */
  private AttributeComponentChange _extractAttributeChange(
    FacesContext facesContext,
    String scopedIdForTargetComponent,
    AttributeComponentChange attributeChange)
  {
    AttributeComponentChange extracted = null;
    
    String viewId = facesContext.getViewRoot().getViewId();
    
    // get the ComponentChanges for the current viewId
    ChangesForView changesForView = _getChangesForView(facesContext, viewId, true);

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
            extracted = currAttributeChange;
            break;
          }
        }
      }
    }

    return extracted;    
  }

  /**
   * insert a component change for a specific component
   * 
   * @param facesContext
   * @param scopedIdForTargetComponent
   * @param componentChange
   */
  private void _insertComponentChange(FacesContext facesContext,
                                      String scopedIdForTargetComponent,
                                      ComponentChange componentChange) 
  {
    String viewId = facesContext.getViewRoot().getViewId();
    
    // get the ComponentChanges for the current viewId
    ChangesForView changesForView = _getChangesForView(facesContext, viewId, true);

    QualifiedComponentChange newQualifiedChange = 
      new QualifiedComponentChange(scopedIdForTargetComponent,
                                   componentChange);
    
    changesForView.addChange(newQualifiedChange);
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
      // If change target for the qualified change is not inside of the specified root, skip
      if (!_acceptChange(qualifiedChange, rootId))
        continue;
      
      String targetComponentScopedId = qualifiedChange.getTargetComponentScopedId();

      UIComponent targetComponent = viewRoot.findComponent(targetComponentScopedId);
      
      // Possible that the target component no more exists in the view, if yes, skip
      if (targetComponent == null)
      {
        _LOG.info(this.getClass().getName(),
                  "applyComponentChangesForCurrentView",
                  "TARGET_COMPONENT_MISSING_CHANGE_FAILED",
                  targetComponentScopedId);
        continue;
      }

      ComponentChange componentChange = qualifiedChange.getComponentChange();

      // We do not apply attribute changes if it is a postback, because we expect that
      // 1. Users calling ChangeManager.addComponentChange() would also apply the change right at
      //  that point in time (maybe by calling ComponentChange.changeComponent() method).
      // 2. If #1 was done, JSF state manager will consider this a state change and will store and
      //  restore it during subsequent postbacks, so there is no need for applying attribute changes
      //  for postback cases. There are few exceptions where the state management will not help, for
      //  which we force the attribute changes even when it is a postback.
      if ( componentChange instanceof AttributeComponentChange &&
           _isStateRestored(facesContext) &&
           !_isAttributeChangeForced(facesContext, targetComponentScopedId) )
            continue;

      // Apply the change
      componentChange.changeComponent(targetComponent);
      
      // Now that the change is applied, we can identify if the components altered by the currently
      //  applied change needs forced application of any further changes regardless of request 
      //  being a postback.
      if (componentChange instanceof MoveChildComponentChange)
      {
        String destinationScopedId = 
          ((MoveChildComponentChange)componentChange).getDestinationScopedId();
        _addAttributeChangeForced(facesContext, destinationScopedId);
      }
      else if (componentChange instanceof SetFacetChildComponentChange)
      {
        String facetName = ((SetFacetChildComponentChange)componentChange).getFacetName();
        UIComponent facetComponent = targetComponent.getFacet(facetName);
        String facetScopedId = 
          ComponentUtils.getScopedIdForComponent(facetComponent, facesContext.getViewRoot());
        if (facetComponent != null)
          _addAttributeChangeForced(facesContext, facetScopedId);
      }
      else if (componentChange instanceof AddChildComponentChange)
      {
        // Get the added component from AddComponentChange, this component is actually re-created 
        //  from the proxy, and not the actual added component. 
        //  Bit hacky but this is only way to get Id.
        String addedComponentId = ((AddChildComponentChange)componentChange).getComponent().getId();
        // Now get the actual added component finding from the parent to which it was added to
        UIComponent addedComponent = 
          ComponentUtils.findRelativeComponent(targetComponent, addedComponentId);
        String addedChildComponentScopedId = 
          ComponentUtils.getScopedIdForComponent(addedComponent, facesContext.getViewRoot());
        if (addedComponent != null)
          _addAttributeChangeForced(facesContext, addedChildComponentScopedId);
      }
    }  
  }
  
  /**
   * Is the state restored by JSF state manager in this request. This is usually true if this is a
   *  postback request. Additionally check if the document tag created a document component, because
   *  if this is the case, we are sure that there was no state restoration.
   */
  private boolean _isStateRestored(FacesContext facesContext)
  {
    boolean docCompCreated = Boolean.TRUE.equals(facesContext.getExternalContext().
                                   getRequestMap().get(UIXComponentELTag.DOCUMENT_CREATED_KEY));
    return (docCompCreated) ? false : RequestContext.getCurrentInstance().isPostback();
  }

  /**
   * For a given scopedId records that any further AttributeComponentChanges must be applied always.
   */
  private void _addAttributeChangeForced(FacesContext facesContext, String targetScopedId)
  {
    if (targetScopedId == null)
      return;
    
    Object session = facesContext.getExternalContext().getSession(true);
    
    synchronized(session)
    {
      Map<String, Object> sessMap = facesContext.getExternalContext().getSessionMap();
      
      CopyOnWriteArrayList<String> forceChangesTargetList = (CopyOnWriteArrayList<String>)
                                       sessMap.get(_ATTRIBUTE_CHANGE_FORCED_COMPONENTS);
      
      if (forceChangesTargetList == null)
      {
        forceChangesTargetList = new CopyOnWriteArrayList<String>();
        sessMap.put(_ATTRIBUTE_CHANGE_FORCED_COMPONENTS, forceChangesTargetList);
      }
      
      forceChangesTargetList.addIfAbsent(targetScopedId);
    }
  }
  
  /**
   * Given a scopedId for the component, returns whether all AttributeComponentChange should be 
   * forced on this component.
   */
  private boolean _isAttributeChangeForced(FacesContext facesContext, String targetScopedId)
  {
    if (targetScopedId == null)
      return false;
    
    Object session = facesContext.getExternalContext().getSession(true);
    
    synchronized(session)
    {
      Map<String, Object> sessMap = facesContext.getExternalContext().getSessionMap();
      
      List<String> forceChangesTargetList = (List<String>)
                                       sessMap.get(_ATTRIBUTE_CHANGE_FORCED_COMPONENTS);
      
      if (forceChangesTargetList == null || forceChangesTargetList.isEmpty())
        return false;

      return forceChangesTargetList.contains(targetScopedId);
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
  
  private static final String _ATTRIBUTE_CHANGE_FORCED_COMPONENTS =
    "org.apache.myfaces.trinidadinternal.AttributeChangeForcedComponents";
    
  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(SessionChangeManager.class);
}
