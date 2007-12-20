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
package org.apache.myfaces.trinidad.component;

import java.util.Collections;

import java.util.List;

import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.ModelUtils;
import org.apache.myfaces.trinidad.model.TreeModel;


/**
 * Base class for components that take a TreeModel, which is a hierarchical model.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/component/UIXHierarchy.java#0 $) $Date: 10-nov-2005.19:09:52 $
 */
abstract public class UIXHierarchy extends UIXCollection implements CollectionComponent
{
  /**
   * Create a Page component with the given render-type
   */
  protected UIXHierarchy(String rendererType)
  {
    super(rendererType);
  }


  protected UIXHierarchy()
  {
    this(null);
  }

  @Override
  public CollectionModel createCollectionModel(CollectionModel current, Object value)
  {
    TreeModel model = ModelUtils.toTreeModel(value);
    model.setRowKey(null);    
    return model;
  }

  /**
   * Gets the index of the first visible row in this tree
   * @return zero-based index. not implemented yet.
   */
  // TODO implement this
  public int getFirst()
  {
    return 0;
  }

  /**
   * Gets the maximum number of rows that this tree should show at a time.
   * @return not implemented yet.
   */
  // TODO implement this
  public int getRows()
  {
    return 0;
  }  
  
  /**
  * Treats the current element as a parent element and steps into the children.
  * A new path is constructed by appending the null value to the old path.
  * The rowData becomes null.
  * It is legal to call this method only if {@link #isContainer}
  * returns true.
  * @see TreeModel#enterContainer
  */
  public final void enterContainer()
  {
    preRowDataChange();
    getTreeModel().enterContainer();
    postRowDataChange();
  }


 /**
  * Changes the rowData to be the parent rowData.
  * A new path is constructed by removing the last rowKey from the old path.
  * The element that is identified by the new path is made current.
  * @see TreeModel#exitContainer
  */
  public final void exitContainer()
  {
    preRowDataChange();
    getTreeModel().exitContainer();
    postRowDataChange();
  }

  /**
   * Checks to see if the current element is a container of other elements.
   * @see TreeModel#isContainer
   * @return true if the current element contains other elements.
   */
  public final boolean isContainer()
  {
    return getTreeModel().isContainer();
  }

  /**
   * Checks to see if the container is empty.
   * @see TreeModel#isContainerEmpty
   * @return true if the current container element has no children.
   */
  public boolean isContainerEmpty()
  {
    
    return getTreeModel().isContainerEmpty();
  }

  /**
   * Gets the depth of the current row in this tree hierarchy
   * @see TreeModel#getDepth()
   * @return zero for any root rows.
   */
  public int getDepth()
  {
    return getTreeModel().getDepth();
  }

  /**
   * Gets the depth of the current row in this tree hierarchy
   * @see TreeModel#getDepth(Object)
   * @return zero for any root rows.
   */
  public int getDepth(Object rowKey)
  {
    return getTreeModel().getDepth(rowKey);
  }

  /**
   * Gets the rowKey of the current row's container.
   * @see TreeModel#getContainerRowKey
   */
  public Object getContainerRowKey()
  {
    return getTreeModel().getContainerRowKey();
  }

  /**
   * Gets the rowKey of the given row's container.
   * @see TreeModel#getContainerRowKey(Object)
   */
  public Object getContainerRowKey(Object childKey)
  {
    return getTreeModel().getContainerRowKey(childKey);
  }
  
  /**
   * Gets the all the rowKeys of the ancestors of the given child row.
   * @see TreeModel#getAllAncestorContainerRowKeys(Object)
   */
  public List<Object> getAllAncestorContainerRowKeys(Object childRowKey)
  {
    return getTreeModel().getAllAncestorContainerRowKeys(childRowKey);
  }

  /**
   * Gets the TreeModel that this tree is displaying.
   */
  protected final TreeModel getTreeModel()
  {
    TreeModel model = (TreeModel) getCollectionModel();
    return model;
  } 
  
  @Override
  protected List<UIComponent> getStamps()
  {
    UIComponent nodeStamp = getFacet("nodeStamp");
    if (nodeStamp != null)
      return Collections.singletonList(nodeStamp);
    else
      return Collections.emptyList();
  }  

  abstract public Object getFocusRowKey();
}
