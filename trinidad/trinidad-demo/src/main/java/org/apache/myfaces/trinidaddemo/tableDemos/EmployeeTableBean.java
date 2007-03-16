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
import java.util.ArrayList;
import java.util.List;

import javax.faces.event.ActionEvent;

/**
 */
public class EmployeeTableBean implements Serializable
{
  public EmployeeTableBean()
  {
    _populate();
  }

  private List<EmployeeBean> _list;

  private int _total;

  /**
   * @param _list
   *          The _list to set.
   */
  public void setList(List<EmployeeBean> list)
  {
    this._list = list;
  }

  /**
   * @return Returns the _list.
   */
  public List<EmployeeBean> getList()
  {
    return _list;
  }

  /**
   * @param _total
   *          The _total to set.
   */
  public void setTotal(int total)
  {
    this._total = total;
  }

  /**
   * @return Returns the _total.
   */
  public int getTotal()
  {
    return _total;
  }

  public void addRow(ActionEvent event)
  {
    EmployeeBean dataHolder = _list.get(_list.size() - 1);
    dataHolder.setReadOnly(true);
    dataHolder = new EmployeeBean("", "");
    dataHolder.setReadOnly(false);
    _list.add(dataHolder);
  }

  public void totalRow(ActionEvent event)
  {
    _total = 0;
    for(EmployeeBean dataHolder : _list)
    {
      if (!dataHolder.getData2().equalsIgnoreCase(""))
      {
        _total = _total + Integer.parseInt(dataHolder.getData2());
      }
    }
  }

  private void _populate()
  {
    _list = new ArrayList<EmployeeBean>();
    _list.add(new EmployeeBean("22", "22"));
    _list.add(new EmployeeBean("44", "44"));
    _list.add(new EmployeeBean("44", "44"));
    _list.add(new EmployeeBean("44", "44"));
    _list.add(new EmployeeBean("44", "44"));

  }
}
