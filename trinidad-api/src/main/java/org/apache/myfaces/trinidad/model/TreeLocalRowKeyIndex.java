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
package org.apache.myfaces.trinidad.model;

/**
 * Defines a set of "local" APIs for a TreeModel.
 * The "local" APIs allow a client to query the tree model and determine if a 
 * set of rows are locally available. "Locally available" can mean the 
 * model has the given set of rows in a local cache and can honor a fetch request
 * efficiently (for example, without performing a SQL query).
 */
public interface TreeLocalRowKeyIndex
{
  /**
   * Indicates whether data for a child model (children of the current node) is 
   * locally available. Locally available means no data fetch is required 
   * as a result of a call to  <code>enterContainer</code>. 
   * @return true if child data is locally available
   */
  public boolean isChildCollectionLocallyAvailable();

  /**
   * Indicates whether child data for the node with the given index is
   * locally available.   
   * @param index row index to check
   * @return true if child data is available, false otherwise
   */
  public boolean isChildCollectionLocallyAvailable(int index);

  /**
   * Indicates whether child data for the node with the given row key is
   * locally available.   
   * @param rowKey row key to check
   * @return true if child data is available, false otherwise
   */
  public boolean isChildCollectionLocallyAvailable(Object rowKey);

  /**
   * Check if a range of rows is locally available starting from a row index.  The range
   * can include child nodes in any expanded nodes within the range.
   * @param startIndex staring index for the range  
   * @param rowCount number of rows in the range
   * @param disclosedRowKeys set of expanded nodes which may fall within the range to check for
   * availability
   * @return <code>true</code> if range of rows is locally available <code>flase</code> otherwise
   */
  public boolean areRowsLocallyAvailable(int startIndex, int rowCount, RowKeySet disclosedRowKeys);

  /**
   * Check if a range of rows is locally available starting from a row key.   The range
   * can include child nodes in any expanded nodes within the range.
   * @param startRowKey staring row key for the range  
   * @param rowCount number of rows in the range
   * @param disclosedRowKeys set of expanded nodes which may fall within the range to check for
   * availability
   * @return <code>true</code> if range of rows is locally available <code>flase</code> otherwise
   */
  public boolean areRowsLocallyAvailable(Object startRowKey, int rowCount, RowKeySet disclosedRowKeys);
  
  /**
   * Check if a range of rows is locally available starting from current position.   The range
   * can include child nodes  in any expanded nodes within the range.
   * @param rowCount number of rows in the range
   * @param disclosedRowKeys set of expanded nodes which may fall within the range to check for
   * availability
   * @return <code>true</code> if range of rows is locally available <code>flase</code> otherwise
   */
  public boolean areRowsLocallyAvailable(int rowCount, RowKeySet disclosedRowKeys);

}
