/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidad.change;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ComponentUtils;

import org.w3c.dom.Node;


/**
 * Change specialization for moving a child from one container to another.
 * MoveChildComponent should be registered on a parent component that is
 * common to the child being moved and the container component at destination.
 * In other words, while calling addComponentChange() or addDocumentChange()
 * methods on the ChangeManager to add a MoveChildComponentChange, the common
 * parent component instance must be passed as an argument. The add() utility
 * method in this class can be alternatively used to conveniently register the
 * change against the common parent. While applying this change, if a child with
 * the same id as the movable child were to be already present in the destination
 * container, the move operation is aborted.
 * @see #add(FacesContext, ChangeManager)
 * @see ChangeManager#addComponentChange(FacesContext, UIComponent, ComponentChange)
 * @see ChangeManager#addDocumentChange(FacesContext, UIComponent, DocumentChange)
 */
public final class MoveChildComponentChange 
  extends ComponentChange
  implements DocumentChange
{
  /**
   * Constructs a MoveChildComponentChange. The child will be appended to the
   * list of children of the destinationContainer.
   * @param movableChild The child component to be moved.
   * @param destinationContainer The destination component into which the child
   * component is to be moved.
   * @throws IllegalArgumentException If movableChild or destinationContainer
   * is null
   */
  public MoveChildComponentChange(
    UIComponent movableChild,
    UIComponent destinationContainer)
  {
    this(movableChild, destinationContainer, null);
  }
  
  /**
   * Constructs a MoveChildComponentChange. The child will be inserted to the 
   * list of children of the destinationContainer, before the supplied 
   * insertBeforecomponent. If the supplied insertBeforeComponent is null, the 
   * child will be appended to the list of children of the destinationContainer.
   * If the insertBeforeComponent is non-null, and if it were not to be found
   * while applying this change, the movableChild will not be moved.
   * @param movableChild The child component to be moved.
   * @param destinationContainer The destination component into which the child 
   * component is to be moved. This should not be null if the insertBeforeComponent
   * is null.
   * @param insertBeforeComponent The component before which the moved child is
   * to be inserted. This can be null, in which case the movableChild is
   * appended.
   * @throws IllegalArgumentException If movableChild is null or destinationContainer
   * and insertBeforeComponent is null, or if a parent component common to 
   * movableChild and destinationContainer could not be found.
   */
  public MoveChildComponentChange(
    UIComponent movableChild,
    UIComponent destinationContainer, 
    UIComponent insertBeforeComponent)
  {
    if (movableChild == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage("MOVABLE_CHILD_REQUIRED"));
    }
    
    /////////////////////////////
    
    FacesContext context = FacesContext.getCurrentInstance();

    // get the doc paths first and validate
    _moveCompDocPath = ComponentUtils.getDocumentLocationForComponent(context, movableChild);
    _destinationContainerDocPath = 
      ComponentUtils.getDocumentLocationForComponent(context, destinationContainer);

    if (_moveCompDocPath == null || _destinationContainerDocPath == null)
    {
      // if either components are not in a doc, component is not in the tree, error condition
      throw new IllegalArgumentException(
        _LOG.getMessage("NO_CONTAINING_DOC_FOUND", 
                        (_moveCompDocPath == null) ? movableChild : destinationContainer));
    }
    
    /////////////////////////////

    // validate destination container
    destinationContainer = _getValidatedDestinationContainer(destinationContainer, 
                                                             insertBeforeComponent);
    // find and validate the common parent
    UIComponent commonParent = _getValidatedCommonParent(context, movableChild, destinationContainer);
    _commonParentDocPath = ComponentUtils.getDocumentLocationForComponent(context, commonParent);
    
    /////////////////////////////
    
    UIComponent viewRoot = context.getViewRoot();

    // Get the scoped id's for move participants (scoped / relative to common parent)
    _moveCompScopedIdAtSource = ComponentUtils.getScopedIdForComponent(movableChild, commonParent);
    _moveCompParentScopedId = 
      ComponentUtils.getScopedIdForComponent(movableChild.getParent(), commonParent);
    _destinationContainerScopedId = ComponentUtils.getScopedIdForComponent(destinationContainer,
                                                                           commonParent);
    _commonParentScopedId = ComponentUtils.getScopedIdForComponent(commonParent, viewRoot);
    
    // cannot proceed if we could not get id for the move participants
    if (_moveCompScopedIdAtSource == null || 
        _moveCompParentScopedId == null || 
        _destinationContainerScopedId == null ||
        _commonParentScopedId == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage("MOVE_PARTICIPANTS_WITHOUT_ID"));
    }
    
    /////////////////////////////

    // get the id path upto the naming container of the common parent so that we can compute the 
    //  absolute scoped ids from the scoped ids
    String commonParentPrefix = _getScopedIdPrefix(commonParent, _commonParentScopedId);
      
    // calculate the absolute scoped ids (scoped from ViewRoot) for the movable componen at its 
    //  source so that we can handle remapping scoped ids in the SessionChangeManager    
    _moveCompAbsoluteScopedIdAtSource = (commonParentPrefix != null) ?
                                          new StringBuilder(commonParentPrefix).
                                            append(NamingContainer.SEPARATOR_CHAR).
                                            append(_moveCompScopedIdAtSource).toString()
                                          : _moveCompScopedIdAtSource;
    
    // find the logical (id in context of the document where the component is defined) scoped id 
    _moveCompAbsoluteLogicalScopedIdAtSource = 
      ComponentUtils.getLogicalScopedIdForComponent(movableChild, viewRoot);
    
    /////////////////////////////
    
    // calculate the absolute scoped ids of the moveable component at destination after move
    String destinationContainerPrefix = _getScopedIdPrefix(destinationContainer, 
                                                           _destinationContainerScopedId);
    StringBuilder moveCompAtDestinationScopedIdBuilder = new StringBuilder();
    
    if (commonParentPrefix != null)
    {
      moveCompAtDestinationScopedIdBuilder.append(commonParentPrefix).
        append(NamingContainer.SEPARATOR_CHAR);
    }
    
    if (destinationContainerPrefix != null)
    {
      moveCompAtDestinationScopedIdBuilder.append(destinationContainerPrefix).
        append(NamingContainer.SEPARATOR_CHAR);
    }
    
    String movableChildId = movableChild.getId();
    _moveCompAbsoluteScopedIdAtDestination = 
      moveCompAtDestinationScopedIdBuilder.append(movableChildId).toString();
    String destinationLogicalPrefix = 
      _getScopedIdPrefix(destinationContainer,
                         ComponentUtils.getLogicalScopedIdForComponent(destinationContainer, 
                                                                       viewRoot));
    
    // find the logical id
    _moveCompAbsoluteLogicalScopedIdAtDestination = (destinationLogicalPrefix != null) ? 
                                                      new StringBuilder(destinationLogicalPrefix).
                                                        append(NamingContainer.SEPARATOR_CHAR).
                                                        append(movableChildId).toString()
                                                      : movableChildId;

    /////////////////////////////

    // For insertBeforeComponent, we do not care to obtain scoped id.
    _insertBeforeCompId = (insertBeforeComponent == null) ? null : insertBeforeComponent.getId();
  }
  
  /**
   * Convenience method to add this MoveChildComponentChange to the supplied
   * ChangeManager. The change will be registered against a parent component
   * that is common to the child being moved and the container component at
   * destination.
   * @param facesContext The FacesContext instance for the current request
   * @param changeManager The ChangeManager instance on which this
   * MoveChildComponentChange is to be added.
   * @return The common parent component against which this 
   * MoveChildComponentChange was registered.
   */
  public UIComponent add(
    FacesContext facesContext, 
    ChangeManager changeManager) 
  {
    UIComponent commonParent = facesContext.getViewRoot().findComponent(_commonParentScopedId);
    
    if (commonParent == null)
    {
      _LOG.warning("COMMON_PARENT_NOT_FOUND", _commonParentScopedId);
      return null;
    }
    
    // Register a move change against the common parent
    changeManager.addComponentChange(facesContext, commonParent, this);
    
    return commonParent;
  }
   
  /**
   * Apply this change to the specified component.
   * @param changeTargetComponent The component that is a common parent to the 
   * movable child and the destination container.
   * @throws IllegalArgumentException If the supplied changeTargetComponent
   * is null.
   */
  @Override
  public void changeComponent(UIComponent changeTargetComponent)
  {
    if (changeTargetComponent == null)
      throw new IllegalArgumentException(
        _LOG.getMessage("COMPONENT_REQUIRED"));
    
    // 1. Check for destination container component 
    UIComponent destinationContainer = 
      changeTargetComponent.findComponent(_destinationContainerScopedId);
    if(destinationContainer == null)
    {
      _LOG.warning("DESTINATION_CONTAINER_NOT_FOUND", _destinationContainerScopedId);
      return;
    }
    
    // 2. Find movableChild, gather the possible duplicates (theoritically only three) and keep a
    //  single copy among them.
    //  Duplicates are possible because 
    //  a) taghandlers re-create the component that was in the jspx file in
    //    their original location, no matter whether it was moved/removed due to 
    //    aplication of a different ComponentChange. Such components could now 
    //    be considered duplicates, because they are newly created from their vanilla definition
    //    in the jspx document, and would not have any further ComponentChanges applied on them. 
    //    There should be one such duplicate.
    //  b) Apart from adding the MoveComponentChange, we expect developers to apply the change in 
    //    order to reflect in the same request cycle (i.e. developers call 
    //    ComponentChange.changeComponent()). Consequently, the component tree contains a moved 
    //    child at the destination. Such components must be preserved, because they have 
    //    incorporated any subsequent ComponentChanges on them. There should be one such moved
    //    component.
    //  c) We would have moved/added components due to previous customization an earlier application 
    //    of ComponentChange, that could still be in the view tree. There should be one such zombie/ 
    //    duplicate.
    UIComponent sourceParent = 
      changeTargetComponent.findComponent(_moveCompParentScopedId);
    
    UIComponent foundChild = 
      changeTargetComponent.findComponent(_moveCompScopedIdAtSource);

    // To flag if a child was already found in a destination container (maybe due to previous move)    
    boolean isChildIdAtDestination = false;
    
    UIComponent movableChild = null;
    int movableChildIndex = 0;
    UIComponent movedChild = null;
    int movedChildIndex = 0;
    UIComponent duplicateChild = null;
    int duplicateChildIndex = 0;
    UIComponent duplicateChildParent = null;

    while (foundChild != null)
    {
      // 2.a. If the parent matches, this could be the component that JSF-Runtime re-created
      //  and added because it is in the physical document
      if (foundChild.getParent().equals(sourceParent))
      {
        movableChild = foundChild;
        movableChildIndex = sourceParent.getChildren().indexOf(movableChild);
      }
      // 2.b.a. We could possibly find the child at its destination, because apart from
      //  adding the change, the change was applied in previous request, and the move
      //  could have been within the same naming container umbrella. In this case
      //  we do not want to move anything and the movable child is considered as a 
      //  duplicate and candidate for removal.
      else if (foundChild.getParent().equals(destinationContainer))
      {
        isChildIdAtDestination = true;
        movedChild = foundChild;
        movedChildIndex = destinationContainer.getChildren().indexOf(movedChild);
      }
      // 2.c. Possible dup from subsequent MoveChildComponentChange in the sequence of multiple
      //  moves of the component in this same request. For example, if the move is from A->B->C,
      //  and if we are currently dealing with move from A->B, the component that was added at
      //  position C (in addition to adding the move change to changemanager) will now be dup.
      else
      {
        duplicateChild = foundChild;
        duplicateChildIndex = foundChild.getParent().getChildren().indexOf(foundChild);
        duplicateChildParent = foundChild.getParent();
      }

      // Invariably, remove the found component from the tree. We remove the
      //  movableChild also, otherwise, findComponent blind loops on this same 
      //  component if movableChild and duplicates are within same immediate
      //  NamingContainer.
      foundChild.getParent().getChildren().remove(foundChild);

      // Try and find the next potential copy of the component to move
      foundChild = changeTargetComponent.findComponent(_moveCompScopedIdAtSource);
    }
    
    //  We need to re-attach the dup for now, the dupes will be eliminated gradually while applying
    //  the successive move change involving the same component.
    if (duplicateChild != null)
    {
      duplicateChildParent.getChildren().add(duplicateChildIndex, duplicateChild);
    }

    // Can't do anything without a movable child.    
    if(movableChild == null)
    {
      _LOG.warning("MOVABLE_CHILD_NOT_FOUND", _moveCompScopedIdAtSource);

      // Reverse any damage that we might have caused, and exit
      if (movedChild != null)
      {
        destinationContainer.getChildren().add(movedChildIndex, movedChild);
      }
      return;
    }
    
    // 2.b.b. Similar to situation in step #2.b.a, but here the move is across different naming 
    //  containers, we could not catch this earlier.
    if (!isChildIdAtDestination)
    {
      String movableChildId = movableChild.getId();
      for (UIComponent childComponent:destinationContainer.getChildren())
      {
        if (movableChildId.equals(childComponent.getId()))
        {
          isChildIdAtDestination = true;
          movedChild = childComponent;
          // Temporarily remove this child, we might add it back in step #3 below.
          movedChild.getParent().getChildren().remove(movedChild);
          break;
        }
      }
    }

    // 3. Check whether the destination container has a child with same id.
    if (isChildIdAtDestination)
    {
      _LOG.warning("MOVABLE_CHILD_SAME_ID_FOUND", _moveCompScopedIdAtSource);

      // Component type matches, this means the child is already at destination. We have removed all
      //  duplicates, and have nothing more to do in this case
      if ( (movableChild.getFamily().equals(movedChild.getFamily())) &&
             (movableChild.getRendererType().equals(movedChild.getRendererType())) )
      {
        // Add back the moved child that we removed earlier.
        destinationContainer.getChildren().add(movedChildIndex, movedChild);
      }
      else
      {
        // Duplicate child by id, but not of the same component type - a condition we cannot handle.
        // Reverse any damage that we might have caused and exit
        sourceParent.getChildren().add(movableChildIndex, movableChild);
      }
      return;
    }

    // We are now dealing with case where there were no duplicates, and a proper point-to-point
    //  move should happen. Reattach the moveable child, so that move happens atomically at the end.
    sourceParent.getChildren().add(movableChildIndex, movableChild);
    
    // 4. See if we can find the insertBeforeComponent among the destinationContainer's children
    int insertIndex = -1;
    if (_insertBeforeCompId != null)
    {
      for (UIComponent childComponent:destinationContainer.getChildren())
      {
        if (_insertBeforeCompId.equals(childComponent.getId()))
        {
          insertIndex = 
            destinationContainer.getChildren().indexOf(childComponent);
          break;
        }
      }
  
      // insertBeforeId was specified, but we cannot find the insertBefore component. Exit.
      if (insertIndex == -1)
      {
        _LOG.warning("INSERT_BEFORE_NOT_FOUND", _insertBeforeCompId);
        return;
      }
    }
    
    // 5. Atomically move the child
    if (insertIndex == -1)
      destinationContainer.getChildren().add(movableChild);
    else
      destinationContainer.getChildren().add(insertIndex, movableChild);
  }
  
  /**
   * Given the DOM Node representing a Component, apply any necessary
   * DOM changes. The node passed will be the Node that is a common parent for
   * the movable child and the destination container.
   * There is a limitation with the document change, that the movable child 
   * Node, destination container Node, and the common parent Node have to belong
   * to the same document.
   * @param changeTargetNode DOM Node that is a common parent for the movable
   * child and the destination container.
   * @throws IllegalArgumentException If changeTargeNode were to be null.
   */
  public void changeDocument(Node changeTargetNode)
  {
    if (changeTargetNode == null)
      throw new IllegalArgumentException(_LOG.getMessage("NO_NODE_SPECIFIED"));

    if ( !_moveCompDocPath.equals(_destinationContainerDocPath) ||
         !_moveCompDocPath.equals(_commonParentDocPath) )
    {
      // If all three participants are not in same doc, we cannot proceed with appling doc change.
      // Throw an exception so that ChangeManagers can handle this failure and do alternate
      //  processing (eg. add a component change given that doc change failed)
      throw new IllegalStateException(
        _LOG.getMessage("MOVE_PARTICIPANTS_NOT_IN_SAME_DOC", 
                        new Object[] {_moveCompDocPath,
                                      _destinationContainerDocPath,
                                      _commonParentDocPath}));
    }

    // Move involves four steps.
    // 1. Finding the child node, the source of move
    Node movableChildNode = 
      ChangeUtils.__findNodeByScopedId(changeTargetNode, 
                                       _moveCompScopedIdAtSource, 
                                       Integer.MAX_VALUE);
    
    if(movableChildNode == null)
    {
      _LOG.warning("MOVABLE_CHILD_NOT_FOUND", _moveCompScopedIdAtSource);
      return;
    }
    
    // 2. Finding the destination container node
    Node destinationContainerNode = 
      ChangeUtils.__findNodeByScopedId(changeTargetNode, 
                                       _destinationContainerScopedId, 
                                       Integer.MAX_VALUE);

    
    if(destinationContainerNode == null)
    {
      _LOG.warning("DESTINATION_CONTAINER_NOT_FOUND", _destinationContainerScopedId);
      return;
    }
    
    //3. Finding the neighbor at the destination
    Node insertBeforeNode = (_insertBeforeCompId == null) ? 
      null:ChangeUtils.__findNodeByScopedId(destinationContainerNode, 
                                            _insertBeforeCompId, 
                                            1);
    // insertBeforeId was specified, but corresponding component is missing.
    //  Abort the move.
    if(_insertBeforeCompId != null && insertBeforeNode == null)
    {
      _LOG.warning("INSERT_BEFORE_NOT_FOUND", _insertBeforeCompId);
      return;
    }

    //4. Atomically move the child.
    destinationContainerNode.insertBefore(movableChildNode, insertBeforeNode);
  }

  /** 
   * Returns true if adding the DocumentChange should force the JSP Document
   * to reload
   * @return true Since moving of components should force the document to reload
   */
  public boolean getForcesDocumentReload()
  {
    return true;
  }
  
  /**
   * Returns the absolute scopedId (relative to the ViewRoot) of the movable component as it is 
   *  before the move
   */
  public String getSourceScopedId()
  {
    return _moveCompAbsoluteScopedIdAtSource;
  }

  /**
   * Returns the absolute scopedId (relative to the ViewRoot) of the movable component as it would 
   *  be after the move
   */
  public String getDestinationScopedId()
  {
    return _moveCompAbsoluteScopedIdAtDestination;
  }
  
  /**
   * Returns the absolute logical scopedId of the movable component as it is before the move. 
   * 
   * The id returned here will be in context of the document where the component is defined. For 
   *  example, consider a component that is defined in a base document and is relocated to a 
   *  different component subtree as in included template (included by an UIXInclude component) 
   *  via. its facet. In this case the logical id of the move component will be in context of base  
   *  document (as if it was never relocated) and not the document that defines the including 
   *  component.
   *  
   * @see #getSourceScopedId()
   */
  public String getSourceLogicalScopedId()
  {
    return _moveCompAbsoluteLogicalScopedIdAtSource;
  }
  
  /**
   * Returns the absolute logical scopedId of the movable component as it would be after the move.
   * 
   * The id returned here will be in context of the document where the component is defined. For 
   *  example, consider a component that is defined in a base document and is relocated to a 
   *  different component subtree as in included template (included by an UIXInclude component) 
   *  via. its facet. In this case the logical id of the move component will be in context of base  
   *  document (as if it was never relocated) and not the document that defines the including 
   *  component.
   *  
   * @see #getDestinationScopedId()
   */
  public String getDestinationLogicalScopedId()
  {
    return _moveCompAbsoluteLogicalScopedIdAtDestination;
  }
  
  @Override
  public boolean equals(Object o)
  {
    if (o == this)
      return true;
    
    if (!(o instanceof MoveChildComponentChange))
      return false;
    
    MoveChildComponentChange other = (MoveChildComponentChange)o;
    
    return  _equalsOrNull(_moveCompScopedIdAtSource, other._moveCompScopedIdAtSource) &&
            _equalsOrNull(_moveCompAbsoluteScopedIdAtSource, 
                          other._moveCompAbsoluteScopedIdAtSource) &&
            _equalsOrNull(_moveCompAbsoluteLogicalScopedIdAtSource, 
                          other._moveCompAbsoluteLogicalScopedIdAtSource) &&
            _equalsOrNull(_moveCompDocPath, other._moveCompDocPath) &&
            _equalsOrNull(_moveCompParentScopedId, other._moveCompParentScopedId) &&
            _equalsOrNull(_moveCompAbsoluteScopedIdAtDestination, 
                          other._moveCompAbsoluteScopedIdAtDestination) &&
            _equalsOrNull(_moveCompAbsoluteLogicalScopedIdAtDestination, 
                          other._moveCompAbsoluteLogicalScopedIdAtDestination) &&
            _equalsOrNull(_destinationContainerScopedId, other._destinationContainerScopedId) &&
            _equalsOrNull(_destinationContainerDocPath, other._destinationContainerDocPath) &&
            _equalsOrNull(_commonParentScopedId, other._commonParentScopedId) &&
            _equalsOrNull(_commonParentDocPath, other._commonParentDocPath) &&
            _equalsOrNull(_insertBeforeCompId, other._insertBeforeCompId);
  }
  
  @Override
  public int hashCode()
  {
    return ((_moveCompScopedIdAtSource == null) ? 0 : _moveCompScopedIdAtSource.hashCode()) + 
            37 * ((_moveCompAbsoluteScopedIdAtSource == null) ? 
                  0 : _moveCompAbsoluteScopedIdAtSource.hashCode()) +
            37 * ((_moveCompAbsoluteLogicalScopedIdAtSource == null) ? 
                  0 : _moveCompAbsoluteLogicalScopedIdAtSource.hashCode()) +
            37 * ((_moveCompDocPath == null) ? 0 : _moveCompDocPath.hashCode()) +
            37 * ((_moveCompParentScopedId == null) ? 
                  0 : _moveCompParentScopedId.hashCode()) +
            37 * ((_moveCompAbsoluteScopedIdAtDestination == null) ? 
                  0 : _moveCompAbsoluteScopedIdAtDestination.hashCode()) +
            37 * ((_moveCompAbsoluteLogicalScopedIdAtDestination == null) ? 
                  0 : _moveCompAbsoluteLogicalScopedIdAtDestination.hashCode()) +
            37 * ((_destinationContainerScopedId == null) ? 
                  0 : _destinationContainerScopedId.hashCode()) +
            37 * ((_destinationContainerDocPath == null) ? 
                  0 : _destinationContainerDocPath.hashCode()) +
            37 * ((_commonParentScopedId == null) ? 0 : _commonParentScopedId.hashCode()) +
            37 * ((_commonParentDocPath == null) ? 0 : _commonParentDocPath.hashCode()) +
            37 * ((_insertBeforeCompId == null) ? 0 : _insertBeforeCompId.hashCode());
  }
      
  @Override
  public String toString()
  {
    StringBuffer sb = new StringBuffer();
    
    sb.append(super.toString());
    sb.append("[moveCompAbsoluteLogicalScopedIdAtSource=").
       append(_moveCompAbsoluteScopedIdAtSource);
    sb.append(" moveCompAbsoluteLogicalScopedIdAtDestination=").
       append(_moveCompAbsoluteLogicalScopedIdAtDestination);
    sb.append(" moveCompAbsoluteScopedIdAtSource=").append(_moveCompAbsoluteScopedIdAtSource);
    sb.append(" moveCompAbsoluteScopedIdAtDestination=").
       append(_moveCompAbsoluteScopedIdAtDestination);
    sb.append(" insertBeforeCompId=").append(_insertBeforeCompId);
    sb.append(" commonParentScopedId=").append(_commonParentScopedId);
    sb.append(" moveCompDocPath=").append(_moveCompDocPath);
    sb.append(" destinationContainerDocPath=").append(_destinationContainerDocPath);
    
    return sb.append("]").toString();
  }

  /**
   * Returns best common parent of the two supplied components in a subtree to which this move 
   * change can be added.
   * - If the supplied components belong to same document, we try to get a common parent that is of
   *   type UIXComponent and belongs to the same document, this is to be able to support applying 
   *   document change.
   * - If they do not belong to same document, we just return the closest common UIComponent parent, 
   *   this should suffice to apply component change.
   * 
   * @throws IllegalArgumentException if we are not able to find the common parent for the supplied
   *          components
   */
  private UIComponent _getValidatedCommonParent(
    FacesContext context,
    UIComponent componentToMove,
    UIComponent parentAtDestination) 
  {
    // Calculate the depth of each node.
    int firstDepth = _computeDepth(componentToMove);
    int secondDepth = _computeDepth(parentAtDestination);
           
    // Move the deeper of the two components to its ancestor at the same depth
    // as the shallower.
    if (secondDepth > firstDepth)
    {
      parentAtDestination = _getAncestor(parentAtDestination, secondDepth - firstDepth);
    }
    else if(secondDepth < firstDepth)
    {
      componentToMove = _getAncestor(componentToMove, firstDepth - secondDepth);
    }

    // Crawl up until we find the shared ancestor.
    while (componentToMove != null && (componentToMove != parentAtDestination))
    {
      componentToMove = componentToMove.getParent();
      parentAtDestination = parentAtDestination.getParent();
    }
    
    UIComponent commonParent = componentToMove;
    
    // try to find a better ancestor complying these two conditions
    //  1. The common parent is a UIXComponent instance - only UIXComponents have tags
    //  2. The common parent belongs to same document that the other two participating components 
    //     belong to
    if (_moveCompDocPath.equals(_destinationContainerDocPath))
    {
      commonParent = _getUIXComponentAncestorInDoc(context, commonParent);
    }
    
    // we cannot proceed if we could not find the common parent
    if (commonParent == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage("COMMON_PARENT_NOT_FOUND"));
    }
    
    return commonParent;
  }
  
  /**
   * For a supplied base component, returns the closest UIXComponent ancestor that is in same 
   * document as base component. If no such component is found, returns the supplied base component.
   */
  private UIComponent _getUIXComponentAncestorInDoc(
    FacesContext context, 
    UIComponent baseComp)
  {
    UIComponent ancestor = baseComp;
    
    while (ancestor != null)
    {
      if (ancestor instanceof UIXComponent &&
          _moveCompDocPath.equals(ComponentUtils.getDocumentLocationForComponent(context, 
                                                                                 ancestor)))
      {
        return ancestor;
      }

      ancestor = ancestor.getParent();
    }
    
    return baseComp;
  }
  
  /**
   * Trims the supplied scoped id of the supplied component until its immediate naming container
   *  and returns.
   */
  private String _getScopedIdPrefix(UIComponent component, String scopedId)
  {
    if (component instanceof NamingContainer)
      return scopedId;
    else
    {
      // remove the component's id from the end
      int separatorIndex = scopedId.lastIndexOf(NamingContainer.SEPARATOR_CHAR);
      
      if (separatorIndex >= 0)
        return scopedId.substring(0, separatorIndex);
      else
      {
        // component was at top level
        return null;
      }
    }
  }
  
  private boolean _equalsOrNull(Object obj1, Object obj2)
  {
    return (obj1 == null) ? (obj2 == null) : obj1.equals(obj2);
  }

  /**
   * Returns the destination container if passed non-null after doing needed validations. If null 
   * destinationContainer is passed, determines it from the supplied insertBeforeComponent. 
   */
  private static UIComponent _getValidatedDestinationContainer(
    UIComponent destinationContainer, 
    UIComponent insertBeforeComponent)
  {
    if (insertBeforeComponent != null)
    {
      UIComponent parent = insertBeforeComponent.getParent();
      
      if (destinationContainer == null)
      {
        destinationContainer = parent;
      }
      
      // if container was supplied, it better be parent of component to move next to
      else if (destinationContainer != parent)
      {
        throw new IllegalArgumentException(
          _LOG.getMessage("DESTINATION_CONTAINER_NOT_INSERTBEFORES_PARENT"));
      }
    }
    else if (destinationContainer == null)
    {
      throw new IllegalArgumentException(_LOG.getMessage("DESTINATION_CONTAINER_REQUIRED"));
    }
    
    return destinationContainer;
  }
  
  /**
   * Returns the depth of a UIComponent in the tree. 
   * @param comp the UIComponent whose depth has to be calculated
   * @return the depth of the passed in UIComponent
   */
  private static int _computeDepth(UIComponent comp) 
  {
    int i = 0;
    while((comp = comp.getParent()) != null) 
    {
      i++;
    }
    return i;
  }

  /**
   * Returns the nth ancestor of the passed in component.
   * @param component The UIComponent whose nth ancestor has to be found
   * @param level Indicates how many levels to go up from the component
   * @return The nth ancestor of the component
   */
  private static UIComponent _getAncestor(UIComponent component, int level) 
  {
    assert(level >= 0);
    
    while(level > 0)
    {
      component = component.getParent();
      level--;
    }
    return component;
  }
  
  // *ScopedId -> the id of the component scoped / relative to the common parent
  // *AbsoluteScopedId -> the id of the component scoped / relative to the ViewRoot
  // *AbsoluteLogicalScopedId -> id of the component scoped / relative to the ViewRoot in context
  //    of the document in which it is originally defined. For more details, see documentation in 
  //    getSourceLogicalScopedId() 
  
  // for component to be moved
  private final String _moveCompScopedIdAtSource;
  private final String _moveCompAbsoluteScopedIdAtSource;
  private final String _moveCompAbsoluteLogicalScopedIdAtSource;
  private final String _moveCompDocPath;
  private final String _moveCompParentScopedId;

  // for component at destination after the move
  private final String _moveCompAbsoluteScopedIdAtDestination;
  private final String _moveCompAbsoluteLogicalScopedIdAtDestination;
  
  // for new parent at destination
  private final String _destinationContainerScopedId;
  private final String _destinationContainerDocPath;

  // for parent common to the source and new parent at destination
  private final String _commonParentScopedId;
  private final String _commonParentDocPath;

  // for immediate sibling of moved component at destination
  private final String _insertBeforeCompId;

  private static final long serialVersionUID = 1L;

  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(MoveChildComponentChange.class);
}
