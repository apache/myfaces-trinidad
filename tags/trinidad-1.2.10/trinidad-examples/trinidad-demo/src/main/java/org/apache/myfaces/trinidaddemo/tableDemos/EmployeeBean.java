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
package org.apache.myfaces.trinidaddemo.tableDemos;

import java.io.Serializable;

/**
 */
public class EmployeeBean implements Serializable
{
  private String data1;

  private String data2;

  private boolean _readOnly;

  public EmployeeBean()
  {
    _readOnly = true;
  }

  public EmployeeBean(String data1, String data2)
  {
    setData1(data1);
    setData2(data2);
    _readOnly = true;
  }

  public EmployeeBean(String data1, String data2, boolean readOnly)
  {
    setData1(data1);
    setData2(data2);
    setReadOnly(readOnly);
  }

  /**
   * @param _readOnly
   *          The _readOnly to set.
   */
  public void setReadOnly(boolean _readOnly)
  {
    this._readOnly = _readOnly;
  }

  /**
   * @return Returns the _readOnly.
   */
  public boolean getReadOnly()
  {
    return _readOnly;
  }

  /**
   * @param data1
   *          The data1 to set.
   */
  public void setData1(String data1)
  {
    this.data1 = data1;
  }

  /**
   * @return Returns the data1.
   */
  public String getData1()
  {
    return data1;
  }

  /**
   * @param data2
   *          The data2 to set.
   */
  public void setData2(String data2)
  {
    this.data2 = data2;
  }

  /**
   * @return Returns the data2.
   */
  public String getData2()
  {
    return data2;
  }
}
