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
package org.apache.myfaces.trinidaddemo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.component.UIXCollection;
import org.apache.myfaces.trinidad.component.UIXTree;
import org.apache.myfaces.trinidad.component.UIXTable;
import org.apache.myfaces.trinidad.model.RowKeySet;

public class TableBean
{
  @SuppressWarnings("unchecked")
  public TableBean()
  {
    _selection = Collections.EMPTY_LIST;
  }
  
  public UIComponent getTable()
  {
    return _table;
  }

  public void setTable(UIComponent hgrid)
  {
    _table = hgrid;
  }

  @SuppressWarnings("unchecked")
  public void performReport(ActionEvent action)
  {
    UIXCollection table = (UIXCollection) _table;
    final RowKeySet state;
    if (table instanceof UIXTable)
      state = ((UIXTable) table).getSelectedRowKeys();
    else
      state = ((UIXTree) table).getSelectedRowKeys();
    Iterator<Object> selection = state.iterator();
    Object oldKey = table.getRowKey();
    _selection = new ArrayList<Object>();
    while (selection.hasNext())
    {
      table.setRowKey(selection.next());
      _selection.add(table.getRowData());
    }
    table.setRowKey(oldKey);
    FacesContext context = FacesContext.getCurrentInstance();
    FacesMessage message =
      new FacesMessage("Report Performed","Report was performed on "+
                       _selection.size()+" records");
    context.addMessage(null, message);
  }

  public List<Object> getReportItems()
  {
    return _selection;
  }

  private UIComponent _table = null;
  private List<Object> _selection;
}
