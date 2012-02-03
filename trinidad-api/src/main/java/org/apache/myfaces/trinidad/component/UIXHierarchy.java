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

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import java.util.Map;
import java.util.Set;

import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.LocalRowKeyIndex;
import org.apache.myfaces.trinidad.model.ModelUtils;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.TreeLocalRowKeyIndex;
import org.apache.myfaces.trinidad.model.TreeModel;
import org.apache.myfaces.trinidad.util.StringUtils;


/**
 * Base class for components that take a TreeModel, which is a hierarchical model.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/component/UIXHierarchy.java#0 $) $Date: 10-nov-2005.19:09:52 $
 */
abstract public class UIXHierarchy extends UIXCollection implements CollectionComponent, LocalRowKeyIndex, 
             TreeLocalRowKeyIndex
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

  //
  //  TreeLocalRowKeyIndex implementation
  //

  /**
   * Indicates whether data for a child model (children of the current node) is 
   * locally available. 
   * @see TreeModel#isChildCollectionLocallyAvailable()
   * @return true if child data is locally available
   */
  public boolean isChildCollectionLocallyAvailable()
  {
    return getTreeModel().isChildCollectionLocallyAvailable();
  }

  /**
   * Indicates whether child data for the node with the given index is
   * locally available.   
   * @see TreeModel#isChildCollectionLocallyAvailable(int)
   * @param index row index to check
   * @return true if child data is available, false otherwise
   */
  public boolean isChildCollectionLocallyAvailable(int index)
  {
    return getTreeModel().isChildCollectionLocallyAvailable(index);
  }

  /**
   * Indicates whether child data for the node with the given row key is
   * locally available.   
   * @see TreeModel#isChildCollectionLocallyAvailable(Object)
   * @param rowKey row key to check
   * @return true if child data is available, false otherwise
   */
  public boolean isChildCollectionLocallyAvailable(Object rowKey)
  {
    return getTreeModel().isChildCollectionLocallyAvailable(rowKey);
  }

  /**
   * Check if a range of rows is locally available starting from a row index.  The range
   * can include child nodes in any expanded nodes within the range.
   * @param startIndex staring index for the range  
   * @param rowCount number of rows in the range
   * @param disclosedRowKeys set of expanded nodes which may fall within the range to check for
   * availability
   * @return <code>true</code> if range of rows is locally available <code>flase</code> otherwise
   * @see TreeModel#areRowsLocallyAvailable(int, int, RowKeySet)
   */
  public boolean areRowsLocallyAvailable(int startIndex, int rowCount,
                                         RowKeySet disclosedRowKeys)
  {
    return getTreeModel().areRowsLocallyAvailable(startIndex, rowCount, disclosedRowKeys);
  }

  /**
   * Check if a range of rows is locally available starting from a row key.   The range
   * can include child nodes in any expanded nodes within the range.
   * @param startRowKey staring row key for the range  
   * @param rowCount number of rows in the range
   * @param disclosedRowKeys set of expanded nodes which may fall within the range to check for
   * availability
   * @return <code>true</code> if range of rows is locally available <code>flase</code> otherwise
   * @see TreeModel#areRowsLocallyAvailable(Object, int, RowKeySet)
   */
  public boolean areRowsLocallyAvailable(Object startRowKey, int rowCount,
                                         RowKeySet disclosedRowKeys)
  {
    return getTreeModel().areRowsLocallyAvailable(startRowKey, rowCount, disclosedRowKeys);
  }

  /**
   * Check if a range of rows is locally available starting from current position.   The range
   * can include child nodes  in any expanded nodes within the range.
   * @param rowCount number of rows in the range
   * @param disclosedRowKeys set of expanded nodes which may fall within the range to check for
   * availability
   * @return <code>true</code> if range of rows is locally available <code>flase</code> otherwise
   * @see TreeModel#areRowsLocallyAvailable(int , RowKeySet)
   */
  public boolean areRowsLocallyAvailable(int rowCount,
                                         RowKeySet disclosedRowKeys)
  {
    return getTreeModel().areRowsLocallyAvailable(rowCount, disclosedRowKeys);
  }


  /**
   * Enhances the varStatusMap created by the super class to include:<ul>
   * <li>"hierarchicalIndex" - returns an array containing the row indices of heirarchy of the currrent row, for e.g. [0,1,2]
   *      This attribute is expensive to compute because of moving currency to calculate the row index for 
   *      each parent collection in the tree hierarchy.
   * </li>
   * <li>"hierarchicalLabel" - returns a string label representing the hierarchy of that row, for e.g. 1.1, 1.1.1. 
   *      The labels are 1 based vs 0 based for rowIndex. 
   *      This attribute is expensive to compute because of moving currency to calculate the row index for 
   *      each parent collection in the tree hierarchy.
   * </li>
   * </ul>
   */
  @Override
  protected Map<String, Object> createVarStatusMap()
  {
    final Map<String, Object> map = super.createVarStatusMap();
    return new AbstractMap<String, Object>()
    {
      @Override
      public Object get(Object key)
      {
        if ("hierarchicalIndex".equals(key)) 
        {
          return _getHierarchicalIndex().toArray();
        }
        if("hierarchicalLabel".equals(key)) 
        {
          List<Integer> rowIndices = _getHierarchicalIndex();
          if(rowIndices.size() == 0)
            return "";
          
          Integer[] indexArray = new Integer[rowIndices.size()];
          
          for(int i = 0; i < rowIndices.size(); i++)
          {
            indexArray[i] = Integer.valueOf(rowIndices.get(i).intValue()+1);
          }
          return StringUtils.join(indexArray, '.');
        }
        return map.get(key);
      }
      
      /**
       * Returns an array of row indexes for the hierarchy of that row
       */
      private List<Integer> _getHierarchicalIndex()
      {
        Object rowKey = getRowKey();
        if(rowKey == null)
          return Collections.emptyList();
        
        TreeModel treeModel = getTreeModel();
        List<Integer> rowIndices = new ArrayList<Integer>();
        
        // Use model APIs for moving currency(setRowIndex/Key) vs component API to avoid performance issue 
        // associated with stamp state saving
        try
        {
          rowIndices.add(treeModel.getRowIndex());
          
          Object childRowKey = treeModel.getContainerRowKey(rowKey);
          while(childRowKey != null)
          {
            treeModel.setRowKey(childRowKey);
            rowIndices.add(treeModel.getRowIndex());
            childRowKey = treeModel.getContainerRowKey(childRowKey);
          }
          Collections.reverse(rowIndices);
        }
        finally
        {
          // make sure that we restore our currency to the original state
          treeModel.setRowKey(rowKey);
        }
        return rowIndices;
      }

      @Override
      public Set<Map.Entry<String, Object>> entrySet()
      {
        return map.entrySet();
      }
    };
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
