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
 * A subclass of {@link ChildPropertyTreeModel} that supports row keys by creating 
 * {@link RowKeyPropertyModel}(s) for its child models.  
 * 
 * Ooverrides the protected createChildModel method in {@link ChildPropertyTreeModel} so that it can instantiate 
 * RowKeyPropertyModels as it encounters child data.
 */
public class RowKeyPropertyTreeModel
  extends ChildPropertyTreeModel
{

  /**
   * Creates a RowKeyPropertyTreeModel  
   * 
   * @param model The underlying model. This will be converted into a {@link DataModel} if necessary
   * @param childProperty The property by which the child data can be accessed.
   * @param rowKeyProperty The property by which the row key can be accessed.
   */
  public RowKeyPropertyTreeModel(Object model, String childProperty, 
                                 String rowKeyProperty)
  {
    super (new RowKeyPropertyModel(model, rowKeyProperty), childProperty);
    _rowKeyProperty = rowKeyProperty;
  }
  
  /**
   * No-arg constructor for use with managed-beans.
   * Must call the {@link #setChildProperty},
   * {@link #setWrappedData} and {@link #setRowKeyProperty} methods after constructing this instance.
   */  
  public RowKeyPropertyTreeModel()
  {
    super();
  }

  /**
   * Overrides ChildPropertyTreeModel.createChildModel(). 
   * Converts childData into a RowKeyPropertyModel.
   * 
   * @param childData the data to convert. This can be a List or array.
   */
  @Override
  protected CollectionModel createChildModel(Object childData)
  {
    CollectionModel model = 
      new RowKeyPropertyModel(childData, _rowKeyProperty);
    model.setRowIndex(-1);
    return model;
  }
    
    
  /**
   * Gets the row key property name for this model
   * @return row key property name
   */
  public String getRowKeyProperty()
  {
    return _rowKeyProperty;
  }

  /**
   * Sets the row key property for this model
   * @param rowKeyProperty row key property to set
   */
  public void setRowKeyProperty(String rowKeyProperty)
  {
    _rowKeyProperty = rowKeyProperty;
  }

  private String _rowKeyProperty = null;

}
