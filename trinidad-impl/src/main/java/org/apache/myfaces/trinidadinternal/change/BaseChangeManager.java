/*
 * Copyright  2005,2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidadinternal.change;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.change.ComponentChange;
import org.apache.myfaces.trinidad.change.DocumentChange;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.traversal.DocumentTraversal;
import org.w3c.dom.traversal.NodeFilter;
import org.w3c.dom.traversal.TreeWalker;


/**
 * Base ChangeManager implementation that manages the bookkeeping for
 * supporting both ComponentChanges and DocumentChanges.
 * subclasses must implement getComponentToChangesMapForView to implement
 * the ComponentChange support.  To support DocumentChanges,
 * <code>getDocument</code> must be implemented.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/change/BaseChangeManager.java#1 $) $Date: 11-nov-2005.14:59:41 $
 * @author The Oracle ADF Faces Team
 */
abstract class BaseChangeManager extends ChangeManager
{
  /**
   * {@inheritDoc}
   *
   * @todo =-=pu : Maybe we need to allow adding Changes for specific viewId's -
   *  BIBeans req. - external customizer dialog
   */
  @Override
  public void addComponentChange(
    FacesContext facesContext,
    UIComponent uiComponent,
    ComponentChange change)
  {
    if (facesContext == null || uiComponent == null || change == null)
      throw new IllegalArgumentException(
        "Cannot add a Change with either of facesContext, uiComponent or " +
        "Change being null.");

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
        _addDocumentChangeImpl(facesContext, uiComponent, docChange);
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void addDocumentChange(
    FacesContext facesContext,
    UIComponent uiComponent,
    DocumentChange change)
  {
    super.addDocumentChange(facesContext, uiComponent, change);
    _addDocumentChangeImpl(facesContext, uiComponent, change);
  }

  /**
   * {@inheritDoc}
   *
   * @todo =-=pu : Maybe we need to allow adding Changes for specific viewId's -
   *  BIBeans req. - external customizer dialog
   */
  private void _addDocumentChangeImpl(
      FacesContext facesContext,
      UIComponent uiComponent,
      DocumentChange change)
  {
    Document jspDocument = getDocument(facesContext);

    if (jspDocument == null)
      throw new IllegalStateException(
        "ChangeManager claimed to support document changes but has no JSP" + // NOTRANS
        "document"); // NOTRANS

    Node componentNode = _getComponentNode(jspDocument, uiComponent);

    if (componentNode != null)
    {
      // immediately apply the change
      change.changeDocument(componentNode);

      // call testing hack
      persistDocumentChanges(facesContext);
    }
    else
    {
      _LOG.warning("Unable to find matching JSP DOcument Node for:" + uiComponent); // NOTRANS
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
      changeListForComponent = new LinkedList<ComponentChange>();
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
    while (ancestor != null)
    {
      if (ancestor instanceof NamingContainer)
        break;
      ancestor = ancestor.getParent();
    }
    String namingContainerId = (ancestor == null) ? "":ancestor.getId();

    return namingContainerId +
           NamingContainer.SEPARATOR_CHAR +
           uiComponent.getId();
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

  /**
   * Returns the Node representing the component within the DOM document
   * @param document
   * @param uiComponent
   * @return Node representing the component within the DOM document
   */
  private Node _getComponentNode(
    Document document,
    UIComponent component)
  {
    if (document instanceof DocumentTraversal)
    {
      List<String> idPath = _getIdPath(component);

      if (!idPath.isEmpty())
      {
        Iterator<String> pathIds = idPath.iterator();

        DocumentTraversal traversalFactory = (DocumentTraversal)document;
        TreeWalker walker = traversalFactory.createTreeWalker(
                                   document.getDocumentElement(),
                                   NodeFilter.SHOW_ELEMENT,
                                   null,
                                   true);

        Node currElement = walker.getCurrentNode();
        Node stopElement = walker.getRoot();
        String neededID = pathIds.next();

        do
        {
          String currID = ((Element)currElement).getAttribute("id");

          if (currID.length() == 0)
          {
            // get the next Node is parent->children->sibling order
            currElement = _getNextNode(walker, stopElement, true);
          }
          else
          {
            // is this our id?
            if (neededID.equals(currID))
            {
              // we found the last id we needed
              if (!pathIds.hasNext())
                return currElement;

              // update the id we need
              neededID = pathIds.next();

              // don't pop back up to this parent
              stopElement = currElement;

              // enter the children
              currElement = walker.firstChild();

              // if we have no chidren, we failed
              if (currElement == null)
                break;
            }
            else
            {
              // don't search children since this ID not in path
              currElement = _getNextNode(walker, stopElement, false);
            }
          }
        }
        while (currElement != stopElement);
      }
    }

    return null;
  }

  private Node _getNextNode(
    TreeWalker walker,
    Node       stopNode,
    boolean    drillIn)
  {
    // first drill in
    Node nextNode;

    if (drillIn)
      nextNode = walker.firstChild();
    else
      nextNode = null;

    // try sibling
    if (nextNode == null)
      nextNode = walker.nextSibling();

    if (nextNode != null)
    {
      return nextNode;
    }
    else
    {
      // pop out
      Node parentNode = walker.parentNode();

      if (parentNode == stopNode)
      {
        // return the stop Node to stop looping
        return stopNode;
      }
      else
      {
        // skip the parent, since it's already been checked
        return _getNextNode(walker, stopNode, false);
      }
    }
  }

  /**
   * Returns the paths of ids from the top most NamingContainer down to
   * this component.  EmptyList will be returned if there are no ids in this
   * path
   * @param component
   * @return
   */
  private List<String> _getIdPath(UIComponent component)
  {
    String componentID = component.getId();

    // we need an ID
    if ((componentID == null) || (componentID.length() == 0))
      return Collections.emptyList();

    LinkedList<String> pathList = new LinkedList<String>();
    pathList.addFirst(componentID);

    UIComponent currAncestor = component.getParent();

    while (currAncestor != null)
    {
      //if (currAncestor instanceof NamingContainer)
      {
        String ancestorID = currAncestor.getId();

        if ((ancestorID != null) && (ancestorID.length() != 0))
        {
          // skip fake ids
          if (!ancestorID.startsWith(_FAKE_ID_PREFIX))
          {
            pathList.addFirst(ancestorID);
          }
        }
      }

      currAncestor = currAncestor.getParent();
    }

    return pathList;
  }

  private static final String _FAKE_ID_PREFIX = "_id";

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(BaseChangeManager.class);

}
