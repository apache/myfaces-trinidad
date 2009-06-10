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

import java.util.List;

/**
 * <p>
 * Used by anybody who wants to wrap the <code>CollectionModel</code> class. 
 * </p>
 * <p>
 * This class simply delegates all the CollectionModel functionalities to the wrapped class.
 * </p>
 * The wrapped CollectionModel is returned by the <code>getCollectionModel</code> method. And that 
 * method needs to be overriden by the subclasses.
 * @see #getCollectionModel
 */
public abstract class CollectionModelDecorator
  extends CollectionModel
{
  public Object getRowKey()
  {
    return getCollectionModel().getRowKey();
  }

  public void setRowKey(Object key)
  {
    getCollectionModel().setRowKey(key);
  }

  public boolean isRowAvailable(int rowIndex)
  {
    return getCollectionModel().isRowAvailable(rowIndex);
  }

  public Object getRowData(int rowIndex)
  {
    return getCollectionModel().getRowData(rowIndex);
  }

  public boolean isSortable(String property)
  {
    return getCollectionModel().isSortable(property);
  }

  public List<SortCriterion> getSortCriteria()
  {
    return getCollectionModel().getSortCriteria();
  }

  public void setSortCriteria(List<SortCriterion> criteria)
  {
    getCollectionModel().setSortCriteria(criteria);
  }

  //
  // below are the DataModel public APIs
  //
  public boolean isRowAvailable()
  {
    return getCollectionModel().isRowAvailable();
  }

  public int getRowCount()
  {
    return getCollectionModel().getRowCount();
  }

  public Object getRowData()
  {
    return getCollectionModel().getRowData();
  }

  public int getRowIndex()
  {
    return getCollectionModel().getRowIndex();
  }

  public void setRowIndex(int i)
  {
    getCollectionModel().setRowIndex(i);
  }

  public Object getWrappedData()
  {
    return getCollectionModel().getWrappedData();
  }

  public void setWrappedData(Object object)
  {
    getCollectionModel().setWrappedData(object);
  }

  public void addDataModelListener(javax.faces.model.DataModelListener listener)
  {
    getCollectionModel().addDataModelListener(listener);
  }

  public javax.faces.model.DataModelListener[] getDataModelListeners()
  {
    return getCollectionModel().getDataModelListeners();
  }

  public void removeDataModelListener(javax.faces.model.DataModelListener listener)
  {
    getCollectionModel().removeDataModelListener(listener);
  }

  /**
   * This method returns the wrapped <code>CollectionModel</code>. 
   * 
   * @return the wrapped CollectionModel
   */
  protected abstract CollectionModel getCollectionModel();
}

