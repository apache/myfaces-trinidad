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

import java.util.Collections;
import java.util.List;
import javax.faces.model.DataModel;

/**
 * The data model that is used by the Trinidad Table and Iterator components.
 * This extends the Faces DataModel class and adds on support for
 * rowKeys and sorting.  Ordinary DataModels are still supported,
 * and will automatically be wrapped into CollectionModels, but
 * without the added functionality.
 * <p>
 * <h3>Row key support</h3>
 * <p>
 * In the Faces DataModel, rows are identified entirely by
 * index.  This causes major problems if the underlying data
 * changes from one request to the next - a user request
 * to delete one row may delete a different row because a
 * row got added by another user, etc.  To work around
 * this, CollectionModel is based around row keys instead
 * of indices.  An implementation of CollectionModel must
 * implement getRowKey()  and setRowKey(), and handle
 * conversion from integer indices to row keys.  A trivial
 * implementation might simply use Integer objects as 
 * the row keys, but a better version could use a unique ID
 * in the row.
 * <p>
 */
public abstract class CollectionModel extends DataModel
  implements RowKeyIndex
{

  /**
   * Gets the rowKey of the current row.
   * rowKeys are safer to use than row indices because rowKeys are
   * unaffected by mutations to this collection.
   * rowKeys should have efficient implementations of 
   * {@link Object#equals} and {@link Object#hashCode} as they will be used
   * as keys in hashtables. rowKeys should also be Serializable, so that the
   * application can run under all JSF state-saving schemes.
   * @return this key should be Serializable and immutable.
   * @see #setRowKey
   */
  public abstract Object getRowKey();

  /**
   * Finds the row with the matching key and makes it current
   * @param key the rowKey, previously obtained from {@link #getRowKey}.
   */
  public abstract void setRowKey(Object key);

  
  /**
   * Checks to see if the row at the given index is available.
   * This method makes the given row current and calls
   * {@link #isRowAvailable()}.
   * Finally, the row that was current before this method was called
   * is made current again.
   * @param rowIndex the index of the row to check.
   * @return true if data for the row exists.
   */
  public boolean isRowAvailable(int rowIndex)
  {
    int oldIndex = getRowIndex();
    try
    {
      setRowIndex(rowIndex);
      return isRowAvailable();
    }
    finally
    {
      setRowIndex(oldIndex);
    }
  }

  /**
   * Gets the rowData at the given index.
   * This method makes the given row current and calls
   * {@link #getRowData()}.
   * Finally, the row that was current before this method was called
   * is made current again.
   * @param rowIndex the index of the row to get data from.
   * @return the data for the given row.
   */
  public Object getRowData(int rowIndex)
  {
    int oldIndex = getRowIndex();
    try
    {
      setRowIndex(rowIndex);
      return getRowData();
    }
    finally
    {
      setRowIndex(oldIndex);
    }
  }

  /**
   * Return true if this collection is sortable by the given property.
   * This implementation always returns false;
   */
  public boolean isSortable(String property)
  {
    return false;
  }

  /**
   * Gets the criteria that this collection is sorted by.
   * This method should never return null.
   * This implementation always returns an empty List.
   * @return each element in this List is of type SortCriterion. 
   * An empty list is returned if this collection is not sorted.
   * @see SortCriterion
   */
  public List<SortCriterion> getSortCriteria()
  {
    return Collections.emptyList();
  }

  /**
   * Sorts this collection by the given criteria.
   * @param criteria Each element in this List must be of type SortCriterion.
   * The empty list may be used to cancel any sort order. null should be treated
   * the same as an empty list.
   * @see SortCriterion
   */
  public void setSortCriteria(List<SortCriterion> criteria)
  {
  }

}
