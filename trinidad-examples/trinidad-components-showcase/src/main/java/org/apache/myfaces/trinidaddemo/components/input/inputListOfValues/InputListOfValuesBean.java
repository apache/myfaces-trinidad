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
package org.apache.myfaces.trinidaddemo.components.input.inputListOfValues;

import org.apache.myfaces.trinidad.component.UIXTable;
import org.apache.myfaces.trinidad.context.RequestContext;

import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;

/**
 *
 */
public class InputListOfValuesBean {
    private static PeriodicDialogBean periodicDialogBean = new PeriodicDialogBean();
    private List<InputListItem> list = new ArrayList<InputListItem>();

    public InputListOfValuesBean() {
        list.add(new InputListItem("Matt"));
        list.add(new InputListItem("John"));
        list.add(new InputListItem("Ira"));
        list.add(new InputListItem("Lucy"));
        list.add(new InputListItem("Tom"));
        list.add(new InputListItem("Jack"));
        list.add(new InputListItem("Victoria"));
        list.add(new InputListItem("Angelina"));
        list.add(new InputListItem("Mark"));
        list.add(new InputListItem("Kate"));
    }

    public List<InputListItem> getList() {
        return list;
    }

    public void setList(List<InputListItem> list) {
        this.list = list;
    }

    public PeriodicDialogBean getPeriodicDialogBean() {
        return periodicDialogBean;
    }


    public static class InputListItem {
        private String _name;

        public InputListItem(String _name) {
            this._name = _name;
        }

        public String getName() {
            return _name;
        }

        public void setName(String name){
            _name = name;
        }

    }


    public static class PeriodicDialogBean {
        private UIXTable _table = null;

        public UIXTable getTable() {
            return _table;
        }

        public void setTable(UIXTable table) {
            _table = table;
        }

        public String cancel() {
            RequestContext.getCurrentInstance().returnFromDialog(null, null);
            return null;
        }

        @SuppressWarnings("unchecked")
        public String select() {
            try{
            FacesContext context = FacesContext.getCurrentInstance();
            Iterator<Object> iterator = _table.getSelectedRowKeys().iterator();
            Object rowKey = iterator.next();
            Object oldRowKey = _table.getRowKey();
            _table.setRowKey(rowKey);
            ValueBinding binding = context.getApplication().
                    createValueBinding("#{row.name}");
            Object value = binding.getValue(context);
            RequestContext.getCurrentInstance().returnFromDialog(value, null);
            _table.setRowKey(oldRowKey);
            }
            catch (NoSuchElementException e){}

            return null;
        }


    }
}

