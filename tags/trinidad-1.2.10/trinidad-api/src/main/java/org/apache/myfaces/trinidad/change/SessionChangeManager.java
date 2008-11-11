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

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
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
    UIViewRoot viewRoot = facesContext.getViewRoot();
    
    List<QualifiedComponentChange> componentChangesForView = 
      _getComponentChangesForView(facesContext, viewRoot.getViewId(), false);
    
    for (QualifiedComponentChange qualifiedChange : componentChangesForView)
    {
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
    
    List<QualifiedComponentChange> componentChangesForView = 
      _getComponentChangesForView(facesContext, viewId, true);

    String scopedIdForTargetComponent = 
      ComponentUtils.getScopedIdForComponent(targetComponent, null);

    componentChangesForView.add(
      new QualifiedComponentChange(scopedIdForTargetComponent, 
                                   componentChange));
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
   * Gets the in-order list of component changes for the given view.
   * @param facesContext The FacesContext instance for this request.
   * @param viewId The id of the view for which changes are required.
   * @param createIfNecessary Indicates whether the underlying datastructures
   * that store the component changes needs to be created if absent.
   * @return The in-order list of component changes for the supplied view. This
   * will be in the same order in which the component changes were added through
   * calls to <code>addComponentChange()</code>.
   */
  private List<QualifiedComponentChange> _getComponentChangesForView(
    FacesContext facesContext,
    String viewId,
    boolean createIfNecessary)
  {
    Object session = facesContext.getExternalContext().getSession(true);
    
    // Key is view id and value is list of component changes for that view
    ConcurrentHashMap<String, List<QualifiedComponentChange>> 
      componentChangesMapForSession;
    
    synchronized(session)
    {
      Map<String, Object> sessMap = 
        facesContext.getExternalContext().getSessionMap();
      
      componentChangesMapForSession = 
        (ConcurrentHashMap<String, List<QualifiedComponentChange>>)
          (sessMap.get(_COMPONENT_CHANGES_MAP_FOR_SESSION_KEY));
      
      if (componentChangesMapForSession == null)
      {
        if (!createIfNecessary)
          return Collections.emptyList();
  
        componentChangesMapForSession = 
          new ConcurrentHashMap<String, List<QualifiedComponentChange>>();
        sessMap.put(_COMPONENT_CHANGES_MAP_FOR_SESSION_KEY, 
                    componentChangesMapForSession);
      }
    }

    if (!componentChangesMapForSession.containsKey(viewId))
    {
      if (!createIfNecessary)
        return Collections.emptyList();
      
      // Writes are per change addition, not very frequent, using 
      // CopyOnWriteArrayList should suffice.
      componentChangesMapForSession.putIfAbsent(
        viewId,
        new CopyOnWriteArrayList<QualifiedComponentChange>());
    }
    
    return componentChangesMapForSession.get(viewId);
  }
  
  private static final class QualifiedComponentChange implements Serializable
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
