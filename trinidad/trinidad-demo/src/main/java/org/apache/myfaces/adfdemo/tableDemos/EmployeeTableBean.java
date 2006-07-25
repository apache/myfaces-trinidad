/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
/*
 * Created on Mar 9, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.apache.myfaces.adfdemo.tableDemos;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.faces.event.ActionEvent;

/**
 * @author asghosh
 */
public class EmployeeTableBean implements Serializable
{
  public EmployeeTableBean()
  {
    _populate();
  }

  private List _list;

  private int _total;

  /**
   * @param _list
   *          The _list to set.
   */
  public void setList(List list)
  {
    this._list = list;
  }

  /**
   * @return Returns the _list.
   */
  public List getList()
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
    EmployeeBean dataHolder = (EmployeeBean) _list.get(_list.size() - 1);
    dataHolder.setReadOnly(true);
    dataHolder = new EmployeeBean("", "");
    dataHolder.setReadOnly(false);
    _list.add(dataHolder);
  }

  public void totalRow(ActionEvent event)
  {
    _total = 0;
    for (Iterator iter = _list.iterator(); iter.hasNext();)
    {
      EmployeeBean dataHolder = (EmployeeBean) iter.next();
      if (!dataHolder.getData2().equalsIgnoreCase(""))
      {
        _total = _total + Integer.parseInt(dataHolder.getData2());
      }
    }
  }

  private void _populate()
  {
    _list = new ArrayList();
    _list.add(new EmployeeBean("22", "22"));
    _list.add(new EmployeeBean("44", "44"));
    _list.add(new EmployeeBean("44", "44"));
    _list.add(new EmployeeBean("44", "44"));
    _list.add(new EmployeeBean("44", "44"));

  }
}
