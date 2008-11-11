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
package org.apache.myfaces.trinidaddemo.dialog;

import java.util.Iterator;

import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.component.UIXTable;
import org.apache.myfaces.trinidad.context.RequestContext;

public class PeriodicDialogBean
{
  public UIXTable getTable()
  {
    return _table;
  }

  public void setTable(UIXTable table)
  {
    _table = table;
  }

  public String cancel()
  {
    RequestContext.getCurrentInstance().returnFromDialog(null, null);
    return null;
  }

  @SuppressWarnings("unchecked")
  public String select()
  {
    FacesContext context = FacesContext.getCurrentInstance();
    // The tableSelectOne is marked as required; so there'd better
    // be a selected row - an exception will result here if there
    // isn't.  Is there some better code?
    Iterator<Object> iterator = _table.getSelectedRowKeys().iterator();
    Object rowKey = iterator.next();
    Object oldRowKey = _table.getRowKey();
    _table.setRowKey(rowKey);
    ValueBinding binding = context.getApplication().
      createValueBinding("#{row.symbol}");
    Object value = binding.getValue(context);
    RequestContext.getCurrentInstance().returnFromDialog(value, null);
    _table.setRowKey(oldRowKey);

    return null;
  }

  private UIXTable _table;
}
