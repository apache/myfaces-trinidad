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
package org.apache.myfaces.trinidaddemo.table;

import java.io.Serializable;
import javax.faces.model.DataModel;

public class DynamicModel extends DataModel implements Serializable 
{
  public DynamicModel()
  {
  }
  
  @Override
  public int getRowCount()
  {
    return _rowCount;    
  }
  
  public void setRowCount(int count)
  {
    if ((count >= -1) && (count <= 400))
      _rowCount = count;
  }
  
  public int getActualRowCount()
  {
    return _length;
  }
  
  public void setActualRowCount(int count)
  {
    if ((count >= 0) && (count <= 400))
      _length = count;
  }
  
  public int getBlockSize()
  {
    return _blockSize;
  }
  
  public void setBlockSize(int blockSize)
  {
    if (blockSize >= 0)
    {
      _blockSize = blockSize;
    }
  }
  
  @Override
  public boolean isRowAvailable()
  {
    return (_index >= 0) && (_index < _length);
  }
  
  @Override
  public Object getRowData()
  {
    return isRowAvailable() ? new Integer(_index) : null;
  }
  
  @Override
  public int getRowIndex()
  {
    return _index;
  }
  
  @Override
  public void setRowIndex(int index)
  {
    _index = index;
  }
  
  @Override
  public Object getWrappedData()
  {
    return this;
  }
  
  @Override
  public void setWrappedData(Object obj)
  {
    throw new UnsupportedOperationException();
  }
    
  private int _rowCount = -1;
  private int _length = 25;
  private int _index = -1;
  private int _blockSize = 10;
}
