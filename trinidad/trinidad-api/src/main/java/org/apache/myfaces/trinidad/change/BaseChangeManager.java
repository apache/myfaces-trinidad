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

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.w3c.dom.Document;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Base ChangeManager implementation that manages the bookkeeping for
 * supporting both ComponentChanges and DocumentChanges.
 * subclasses must implement getComponentToChangesMapForView to implement
 * the ComponentChange support.  To support DocumentChanges,
 * <code>getDocument</code> must be implemented.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/change/BaseChangeManager.java#1 $) $Date: 11-nov-2005.14:59:41 $
 */
abstract class BaseChangeManager extends ChangeManager
{
  /**
   * {@inheritDoc}
   */
  // TODO : Maybe we need to allow adding Changes for specific viewId's
  @Override
  public void addComponentChange(
    FacesContext facesContext,
    UIComponent uiComponent,
    ComponentChange change)
  {
    if (facesContext == null || uiComponent == null || change == null)
      throw new IllegalArgumentException(_LOG.getMessage(
        "CANNOT_ADD_CHANGE_WITH_FACECONTEXT_OR_UICOMPONENT_OR_NULL"));

    // add the change to the component
    _addComponentChangeImpl(facesContext, uiComponent, change);

    if (supportsDocumentPersistence(facesContext))
    {
      DocumentChange docChange = null;

      if (change instanceof DocumentChange)
      {
        docChange = (DocumentChange)change;
      }
      else
      {
        // try to get equivalent DocumentChange from ComponentChange
        docChange = createDocumentChange(change);
      }

      if (docChange != null)
      {
        addDocumentChange(facesContext, uiComponent, docChange);
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Iterator<ComponentChange> getComponentChanges(
    FacesContext facesContext,
    UIComponent uiComponent)
  {
    if (uiComponent == null)
      return null;

    String viewId = facesContext.getViewRoot().getViewId();
    Map<String, List<ComponentChange>> componentToChangesMap =
       getComponentToChangesMapForView(facesContext, viewId, false);

    if (componentToChangesMap == null)
      return null;

    String uniqueIdForComponent = _getUniqueIdForComponent(uiComponent);
    List<ComponentChange> changesList = componentToChangesMap.get(uniqueIdForComponent);
    if (changesList == null)
      return null;

    return changesList.iterator();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Iterator<String> getComponentIdsWithChanges(FacesContext facesContext)
  {
    String viewId = facesContext.getViewRoot().getViewId();
    Map<String, List<ComponentChange>> componentToChangesMap =
       getComponentToChangesMapForView(facesContext, viewId, false);

    if (componentToChangesMap == null)
      return null;

    return componentToChangesMap.keySet().iterator();
  }

  /**
   * The Map used to store the Changes.  The Map is stored as
   * key=ComponentCompositeId, value=ChangesList (List)
   *
   * @param facesContext FacesContext for request
   * @param viewId viewID for request
   * @param createIfNecessary <code>true</code> if Map should be created if not
   *        already present
   * @return Map of componentID tokens to Lists of Changes
   */
  protected abstract Map<String, List<ComponentChange>> getComponentToChangesMapForView(
    FacesContext facesContext,
    String viewId,
    boolean createIfNecessary);

  /**
   * Implementation of adding a ComponentChange
   * @param change ComponentChange to add
   */
  private void _addComponentChangeImpl(
    FacesContext facesContext,
    UIComponent uiComponent,
    ComponentChange change)
  {
    String viewId = facesContext.getViewRoot().getViewId();

    Map<String, List<ComponentChange>> componentToChangesMap = getComponentToChangesMapForView(facesContext,
                                                                viewId,
                                                                true);

    String uniqueIdForComponent = _getUniqueIdForComponent(uiComponent);

    List<ComponentChange> changeListForComponent = 
      componentToChangesMap.get(uniqueIdForComponent);

    if (changeListForComponent == null)
    {
      changeListForComponent = new CopyOnWriteArrayList<ComponentChange>();
      componentToChangesMap.put(uniqueIdForComponent, changeListForComponent);
    }

    changeListForComponent.add(change);
  }

  // =-= bts Testing hack hook
  protected void persistDocumentChanges(
    FacesContext facesContext)
  {
    // noop
  }

  static private String _getUniqueIdForComponent(UIComponent uiComponent)
  {
    //pu: If this component were to be a NamingContainer, we treat itself as its
    //  own closest ancestor that is a NamingContainer. Matches algorithm of
    //  UIComponent.findComponent().
    UIComponent ancestor = uiComponent;
    StringBuffer uniqueIdBuffer = new StringBuffer();
    uniqueIdBuffer.append(uiComponent.getId());
    while (ancestor != null)
    {
      if (ancestor instanceof NamingContainer)
      {
        uniqueIdBuffer.insert(0,new StringBuffer().append(ancestor.getId()).
          append(NamingContainer.SEPARATOR_CHAR));
      }
      ancestor = ancestor.getParent();
    }
    return uniqueIdBuffer.toString();
  }

  /**
   * Override to return the Document to modify as part of document-based
   * persistence.
   * Subclassers adding Document-based Persistence
   * must override this method and should override
   * <code>supportsDocumentPersistence</code>
   * in order to enable  Document-based Persistence
   */
  protected abstract Document getDocument(FacesContext context);
  
  /**
   *  Returns true if we can support Document-based Persistence
   *  in this ChangeManager.  Subclassers adding Document-based Persistence
   *  should override this method and must override <code>getDocument</code>
   *  in order to enable  Document-based Persistence.
   * @param context
   * @return true if we can support Document-based Persistence
   */
  protected boolean supportsDocumentPersistence(FacesContext context)
  {
    // correct, but potentially slow implementation
    return getDocument(context) != null;
  }
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    BaseChangeManager.class);
}
