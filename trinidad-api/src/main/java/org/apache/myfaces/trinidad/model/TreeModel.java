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
package org.apache.myfaces.trinidad.model;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;


/**
 * The data model used by Trinidad Tree components.  A TreeModel is
 * responsible for understanding how to iterate through an
 * object graph, enter and leave containers, and identify
 * rows of objects within each container.  Within any one
 * container, a TreeModel looks just like a CollectionModel,
 * which extends the JSF DataModel class.  (So, to understand
 * this class, start by learning how DataModel works).
 * <p>
 * <h3>Entering and exiting containers</h3>
 * <p>
 * TreeModel extends CollectionModel to add support for container rows,
 * which are entered and exited with enterContainer() and exitContainer()
 * methods.  Within a container, row indices (get/setRowIndex())
 * are relative to the container.  However, row keys - get/setRowKey(),
 * from the CollectionModel API - are always global for the entire
 * tree model, so it is sufficient to call setRowKey() to enter and
 * exit all the needed parents.
 * <p>
 * <h3>Lazy loading of contents</h3>
 * <p>
 * When a tree or treeTable iterates through the model,
 * it will generally seek to see if a given node is a
 * container - with the <code>isContainer()</code> method -
 * and also see if the node is empty (and therefore
 * not expandable) with the <code>isContainerEmpty()</code>
 * method.  The default implementation of that latter
 * method involves entering the child and seeing how
 * many children it has.  As a result, by default,
 * you will see one more level of content being
 * requested than is actually visible on screen.  To
 * avoid this, provide a custom override of <code>
 * isContainerEmpty()</code> to return a value
 * without actually entering the container.  It
 * is acceptable for this method to return a "false negative" -
 * to return false when there might actually not be any
 * contents - if that is the most efficient approach possible.
 * <p>
 * The <code>ChildPropertyTreeModel</code> class is a useful
 * basic subclass, but largely requires that you have the
 * entire object model fully loaded.  If you require
 * lazy loading, you'll likely need a custom implementation.
 * <p>
 * <h3>Further documentation</h3>
 * <p>
 * Rows in the TreeModel may (recursively) contain other rows.
 * To figure out if the current row is a container, call the
 * {@link #isContainer} method.
 * If a row is a container, use the {@link #enterContainer} method
 * to access its child rows. Once the {@link #enterContainer} method is called
 * all the CollectionModel API's methods (like {@link #getRowCount}) 
 * operate on the child collection.
 * To return back to the parent row, use the {@link #exitContainer} method.
 * <P>
 * Given the following tree structure:
 * <pre>
 * |-Root1 (rowKey="r1", rowIndex=0)
 * |  |-Folder1 (rowKey="r1f1", rowIndex=0)
 * |  |  |-Node1 (rowKey="r1f1n1", rowIndex=0)
 * |  |  |-Node2 (rowKey="r1f1n2", rowIndex=1)
 * |  |  |-Node3 (rowKey="r1f1n3", rowIndex=2)
 * |  |
 * |  |-Folder2 (rowKey="r1f2", rowIndex=1)
 * |     |-Node4 (rowKey="r1f2n1", rowIndex=0)
 * |
 * |-Root2 (rowKey="r2", rowIndex=1)
 * |-Root3 (rowKey="r3", rowIndex=2)
 * |-Root4 (rowKey="r4", rowIndex=3)
 * </pre>
 * To point the tree to the root collection call:
 * <code>setRowKey(null)</code>.<br>
 * Now, <code>getRowCount()</code> returns 4.<br>
 * <code>setRowIndex(1);getRowData()</code> returns <code>Root2</code>.<br>
 * <code>setRowKey("r4");getRowData()</code> returns <code>Root4</code>.
 * <P>
 * To access <code>Node4</code> use:
 * <pre>
 * setRowIndex(0); // isContainer()==true
 * enterContainer(); // enter Root1, getRowCount()==2
 * setRowIndex(1); // isContainer()==true
 * enterContainer(); // enter Folder2, getRowCount()==1
 * setRowIndex(0); // isContainer()==false
 * getRowData();
 * </pre>
 * Or, more simply:
 * <pre>
 * setRowKey("r1f2n1");
 * getRowData();
 * </pre>
 * At this point, to get at <code>Node3</code> use:
 * <pre>
 * exitContainer(); // exit Folder2, Root1 is now the current row.
 * setRowIndex(0);
 * enterContainer(); // enter Folder1, getRowCount()==3
 * setRowIndex(2);
 * getRowData();
 * </pre>
 * Or, more simply:
 * <pre>
 * setRowKey("r1f1n3");
 * getRowData();
 * </pre>
 */
public abstract class TreeModel extends CollectionModel
{

  /**
   * Tests to see if the row identified by getRowData() is a container element.
   * Use {@link #isContainerEmpty} to see if the current container element actually
   * has children, or is an empty container.
   * @return true if the current element may contain children.
   */
  public abstract boolean isContainer();

  /**
   * Tests to see if the current container element actually has children.
   * This could be more efficient than calling
   * {@link #enterContainer} followed by {@link #getRowCount}.
   * This method is permitted to return false even if the container is actually
   * empty.
   * This method should only be called if {@link #isContainer} returns true.
   * @return true if the current container element has no children. If there
   * is any doubt as to whether or not the container has children, this method
   * should return false.
   */
  public boolean isContainerEmpty()
  {
    if (!isContainer())
      return true;

    enterContainer();
    try
    {
      int kids = getRowCount();
      if (kids < 0)
      {
        setRowIndex(0);
        return !isRowAvailable();
      }
      return (kids == 0);
    }
    finally
    {
      exitContainer();
    }
  }
  
  /**
   * This Collection changes to reflect the children of the current rowData,
   * and the current rowData changes to be null.
   * The current rowIndex becomes -1. This method should only be called
   * if {@link #isContainer()} returns true.
   * {@link #getRowCount} can be used to get the number of children. 
   */
  public abstract void enterContainer();
  
  /**
   * Pops back up to the parent collection.
   * The current rowData becomes the rowData of the parent.
   * This Collection will change to include the children of the new rowData.
   */
  public abstract void exitContainer();
  
  /**
   * Gets the rowKey of the current row's container row.
   * This implementation calls {@link #getContainerRowKey(Object)} with
   * the current rowKey.
   */
  public final Object getContainerRowKey()
  {
    Object key = getRowKey();
    Object parentKey = getContainerRowKey(key);
    return parentKey;
  }

  /**
   * Gets the rowkey of each container, starting from the top most
   * container, down to the container of the given child rowKey.
   * The root container (which always has the null rowKey) is not included in
   * this list. The given childRowKey is not included in this list.
   * <p>
   * Given the following tree structure:
   * <pre>
   * |-Root1 (rowKey="r1")
   * |  |-Folder1 (rowKey="r1f1")
   * |  |  |-Node1 (rowKey="r1f1n1")
   * </pre>
   * Calling <code>getAllAncestorContainerRowKeys("r1f1n1")</code>
   * returns a List of two items:"r1" and "r1f1", in that order.
   * 
   * @param childRowKey identifies the child row. 
   * @return An empty list is returned if the child row is a root row and
   * has no parent containers. Each item in this list is a rowKey
   * and is of type {@link Object}.
   * The first rowKey (in this list) is the top most container. The last
   * rowKey is the immediate container of the given childRowKey.
   */
  public List<Object> getAllAncestorContainerRowKeys(Object childRowKey)
  {
    if (childRowKey == null)
      return Collections.emptyList();

    int size = getDepth(childRowKey);
    if (size <= 0)
      return Collections.emptyList();
      
    Object[] keys = new Object[size];
    for(int i=size-1; i>=0; i--)
    {
      childRowKey = getContainerRowKey(childRowKey);
      assert childRowKey != null;
      keys[i] = childRowKey;
    }
    return Collections.unmodifiableList(Arrays.asList(keys));
  }
  
  /**
   * Gets the rowKey of a given child row's container row. 
   * <pre>
   * |-Root1 (rowKey="r1", containerRowKey=null)
   * |  |-Folder1 (rowKey="r1f1", containerRowKey="r1")
   * |  |  |-Node1 (rowKey="r1f1n1", containerRowKey="r1f1")
   * |  |  |-Node2 (rowKey="r1f1n2", containerRowKey="r1f1")
   * </pre>
   * @param childRowKey the rowKey of the child row.
   * @return the rowKey of the container, or null if the child is a root row.
   */
  public abstract Object getContainerRowKey(Object childRowKey);
  
  /**
   * Gets the depth of the current row within this tree hierarchy.
   * <br>
   * This implementation simply calls {@link #getDepth(Object)} with
   * the current rowKey.
   */
  public final int getDepth()
  {
    Object key = getRowKey();
    return getDepth(key);
  }
  
  /**
   * Gets the depth of the given row within the tree hierarchy.
   * The depth is a measure of how far the given row is from its top-level
   * container row.
   * Root-level rows have a depth of zero. All the immediate children of each
   * root row have a depth of one.
   * <pre>
   * |-Root1 (depth=0)
   * |  |-Folder1 (depth=1)
   * |  |  |-Node1 (depth=2)
   * |  |  |-Node2 (depth=2)
   * |  |  |-Node3 (depth=2)
   * |  |-Folder2 (depth=1)
   * |-Root2 (depth=0)
   * </pre>
   */
  public int getDepth(Object rowKey)
  {
    Object key = rowKey;
    int depth = 0;
    while(true)
    {
      key = getContainerRowKey(key);
      if (key == null)
        break;
      depth++;
    }
    return depth;
  }
}
