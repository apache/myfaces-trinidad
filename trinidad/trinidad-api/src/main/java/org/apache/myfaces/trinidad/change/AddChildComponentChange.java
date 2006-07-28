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

package org.apache.myfaces.trinidad.change;

import java.util.List;

import javax.faces.component.UIComponent;

/**
 * Change specialization for adding a child component.
 * While applying this Change, the child component is re-created and added to
 *  the list of children.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/AddChildComponentChange.java#0 $) $Date: 10-nov-2005.19:09:54 $
 * @author The Oracle ADF Faces Team
 */
public class AddChildComponentChange extends AddComponentChange 
{
  /**
   * Constructs an AddChildChange that appends the specified child component.
   * @param childComponent The child component that is to be appended.
   * @throws IllegalArgumentException if specified childComponent is null.
   */
  public AddChildComponentChange(UIComponent childComponent)
  {
    this(null,childComponent);
  }
  
  /**
   * Constructs an AddChildChange with the specified child component and the
   *  the identifier of the neighbour. If the neighbour is not found
   *  when applying this Change, or is <code>null<code>< the child is
   *  appended to the end of the list of children.
  *  @param insertBeforeId The identifier of the sibling before which this new 
  *         child is to be inserted.
   * @param childComponent The child component that is to be added.
   * @throws IllegalArgumentException if specified childComponent is null
   */
  public AddChildComponentChange(
    String insertBeforeId,
    UIComponent childComponent)
  {
    super(childComponent);
   
    _insertBeforeId = insertBeforeId;
  }
  
  /**
   * Returns the identifier of the sibling before which this new child needs to
   *  be inserted.
   */
  public String getInsertBeforeId()
  {
    return _insertBeforeId;
  }
  
  /**
   * {@inheritDoc}
   */
  public void changeComponent(UIComponent uiComponent)
  {
    UIComponent child = getComponent();
    
    if (child == null)
      return;
      
    String newChildId = child.getId();
    List children = uiComponent.getChildren();
    
    //pu: If there were to be a child already with the ID same as the
    //  to-be-added child, remove it and get the new one added.
    UIComponent removableChild = ChangeUtils.getChildForId(uiComponent, newChildId);
  
    if (removableChild != null)
      children.remove(removableChild);
    
    if (_insertBeforeId == null)
    {
      // append the child
      children.add(child); 
    }
    else
    {
      int index = _getChildIndex(uiComponent, _insertBeforeId);
      children.add(index, child);
    }
  }
  
  private static int _getChildIndex(UIComponent parent, String childId)
  {
    int numChildren = parent.getChildCount();
    if (childId == null)
      return numChildren;
    UIComponent child = ChangeUtils.getChildForId(parent, childId);
    if (child == null)
    {
      return numChildren;
    }
    else
    {
      return parent.getChildren().indexOf(child);
    }
  }

  private final String _insertBeforeId;
}
