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
  implements RowKeyIndex, LocalRowKeyIndex
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
   * @see CollectionModel#isRowAvailable()
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
   * Check for an available row by row key. 
   * This method makes the given row current and calls
   * {@link #isRowAvailable()}.
   * Finally, the row that was current before this method was called
   * is made current again.
   * @see CollectionModel#isRowAvailable()
   * @param rowKey the row key for the row to check.
   * @return true if data for the row exists otherwise return false if the
   * row data does not exist or the rowKey is null
   */
  public boolean isRowAvailable(Object rowKey)
  {
    Object oldKey = getRowKey();
    try
    {
      setRowKey(rowKey);
      return isRowAvailable();
    }
    finally
    {
      setRowKey(oldKey);
    }
  }

  /**
   * Gets the rowData at the given index.
   * This method makes the given row current and calls
   * {@link #getRowData()}.
   * Finally, the row that was current before this method was called
   * is made current again.
   * @see CollectionModel#getRowData()
   * @param rowIndex the index of the row to get data from.
   * @return the data for the given row.  The {@link #getRowData()} call
   * may throw <code>IllegalArgumentException</code> if the rowIndex is
   * less than zero or grater than row count or row data at the given index
   * is not available
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
   * Gets the rowData at the given row key.
   * This method makes the given row current and calls
   * {@link #getRowData()}.
   * Finally, the row that was current before this method was called
   * is made current again.
   * @see CollectionModel#getRowData()
   * @param rowKey the row key of the row to get data from.
   * @return the data for the given row. The {@link #getRowData()} implementation
   * may throw <code>IllegalArgumentException</code> if the rowKey is
   * null or data for the given rowKey is unavailable
   */
  public Object getRowData(Object rowKey)
  {
    Object oldKey = getRowKey();
    try
    {
      setRowKey(rowKey);
      return getRowData();
    }
    finally
    {
      setRowKey(oldKey);
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

  /**
   * Check if a range of rows is available from a starting index.
   * The current row does not change after this call
   * @param startIndex the starting index for the range
   * @param rowsToCheck number of rows to check. If rowsToCheck < 0 set 
   * startIndex = startIndex - abs(rowsToCheck) + 1.  This 
   * allows for checking for row availability from the end position. For example
   * to check for availability of n rows from the end,  call 
   * isRangeAvailable(getRowCount()-1, -n)
   * @return true if rows are available otherwise return <code>false</code>
   * if startIndex < 0 or > rowCount or rows in range are not available
   */
  public boolean areRowsAvailable(int startIndex, int rowsToCheck)
  {
    int oldIndex = getRowIndex();
    try
    {
      if (rowsToCheck < 0)
      {
        rowsToCheck = Math.abs(rowsToCheck);
        startIndex = startIndex - rowsToCheck + 1;
      }
      setRowIndex(startIndex);
      return areRowsAvailable(rowsToCheck);
    }
    finally
    {
      setRowIndex(oldIndex);
    }
  }

  /**
   * Check if a range of rows is available from a starting row key 
   * This method make the row with the given row key current and calls
   * {@link #areRowsAvailable(rowsToCheck)}.
   * The current row does not change after this call
   * @see CollectionModel#areRowsAvailable(int).
   * @param startRowKey the starting row key for the range
   * @param rowsToCheck number of rows to check
   * @return true if rows are available otherwise return
   * false if rowKey is null or rows in range are not available
   */
  public boolean areRowsAvailable(Object startRowKey, int rowsToCheck)
  {
    Object oldKey = getRowKey();
    try
    {
      setRowKey(startRowKey);
      return areRowsAvailable(rowsToCheck);      
    }
    finally
    {
      setRowKey(oldKey);
    }
  }

  /**
   * Check if a range of rows is available starting from the
   * current row. This implementation checks the start and end rows in the range
   * for availability. The current row does not change after this call
   * @param rowsToCheck number of rows to check
   * @return true if start and end rows in range are available otherwise return
   * false if start and end rows are not available or rowsToCheck <= 0 or the current
   * rowIndex < 0
   */
  public boolean areRowsAvailable(int rowsToCheck)
  {
    int startIndex = getRowIndex();
    
    if (startIndex < 0 || rowsToCheck <= 0)
      return false;
    

    long count = getRowCount();
    if (count != -1)
    {
      if (startIndex >= count)
        return false; 
      
      if (startIndex + rowsToCheck > count)
        rowsToCheck = (int)count - startIndex;
    }
    int last = startIndex + rowsToCheck - 1;
    
    try
    {
      // check start index
      if (!isRowAvailable())
        return false;
      
      // check end index
      setRowIndex(last);
      return isRowAvailable();
    }
    finally
    {
      setRowIndex(startIndex);
    }
  }

  //
  // Below is the default implemenation for the LocalRowKeyIndex interface.  
  // This implemenation delegates to the corresponding non-local APIs
  //
  
  /**
   * Check if a range of rows is locally available starting from a row index.  
   * This implementation delegates to the corresponding non-local API
   * @see  CollectionModel#areRowsAvailable(int, int)
   * @param startIndex
   * @param rowsToCheck
   * @return
   */
  public boolean areRowsLocallyAvailable(int startIndex, int rowsToCheck)
  {
    return areRowsAvailable(startIndex, rowsToCheck);
  }

  /**
   * Check if a range of rows is locally available starting from a row key.  
   * This implementation delegates to the corresponsding non-local API
   * @see CollectionModel#areRowsAvailable(Object, int)
   * @param startRowKey
   * @param rowsToCheck
   * @return
   */
  public boolean areRowsLocallyAvailable(Object startRowKey, int rowsToCheck)
  {
    return areRowsAvailable(startRowKey, rowsToCheck);
  }


  /**
   * Given a row index, check if the row is locally available. This 
   * implementation delegates to the non-local API
   * @see CollectionModel#isRowAvailable(int)
   * @param rowIndex
   * @return  true if row is available; false otherwise.
   */
  public boolean isRowLocallyAvailable(int rowIndex)
  {
    return isRowAvailable(rowIndex);
  }

  /**
   * Given a row key, check if the row is locally available. This
   * implementation delegates to the non-local API
   * @see CollectionModel#isRowAvailable(Object)
   * @param rowKey
   * @return  true if row is available; false otherwise.
   */
  public boolean isRowLocallyAvailable(Object rowKey)
  {
    return isRowAvailable(rowKey);
  }

  /**
   * Convenient API to return a row count estimate.  This implementation
   * always returns exact row count
   * @see CollectionModel#getRowCount
   * @return estimated row count
   */
  public int getEstimatedRowCount()
  {
    return getRowCount();
  }

  /**
   * Helper API to determine if the row count returned from {@link #getEstimatedRowCount} 
   * is EXACT, or an ESTIMATE.  This implemetation always returns exact row count
   * @see CollectionModel#getRowCount
   */
  public LocalRowKeyIndex.Confidence getEstimatedRowCountConfidence()
  {
    return LocalRowKeyIndex.Confidence.EXACT;
  }

  /**
   * Clears the row with the given index from local cache.
   * This is a do nothing implementaion which delegates to the
   * correcsponding range based api
   * @see #clearCachedRows(int, int)
   * @param index row index for the row to remove from cache
   */
  public void clearCachedRow(int index)
  {
    clearCachedRows(index, 1);
  }

  /**
   * Clears the row with the given row key from local cache.
   * This is a do nothing implementaion which delegates to the
   * correcsponding range based api
   * @see #clearCachedRows(Object, int)
   * @param rowKey row key for the row to remove from cache
   */
  public void clearCachedRow(Object rowKey)
  {
    clearCachedRows(rowKey, 1);
  }

  /**
   * Clears a range of rows from local cache starting from a row index.
   * This is a do nothing implemenation.
   * @see #clearLocalCache
   * @param startingIndex starting row index to clear the local cache from
   * @param rowsToClear number of rows to clear
   */
  public void clearCachedRows(int startingIndex, int rowsToClear)
  {
    clearLocalCache();
  }

  /**
   * Clears a range of rows from local cache starting from a row key
   * This is a do nothing implemenation.
   * @see #clearLocalCache
   * @param startingRowKey starting row key to clear the local cache from
   * @param rowsToClear number of rows to clear
   */
  public void clearCachedRows(Object startingRowKey, int rowsToClear)
  {
    clearLocalCache();
  }

  /**
   * Clears the local cache.
   * This is a do nothing implementation
   */
  public void clearLocalCache()
  {
    // do nothing 
  }

  /**
   * Returns the row caching strategy used by this implemenation. Default
   * implementation indicates no caching supported
   * @see LocalRowKeyIndex.LocalCachingStrategy
   * @return caching strategy none
   */
  public LocalRowKeyIndex.LocalCachingStrategy getCachingStrategy()
  {
    return LocalRowKeyIndex.LocalCachingStrategy.NONE;
  }
  
}
