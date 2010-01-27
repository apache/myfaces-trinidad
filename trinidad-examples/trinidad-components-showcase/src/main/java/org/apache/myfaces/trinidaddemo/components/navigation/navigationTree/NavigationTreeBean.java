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
package org.apache.myfaces.trinidaddemo.components.navigation.navigationTree;

import org.apache.myfaces.trinidad.model.TreeModel;
import org.apache.myfaces.trinidad.model.ChildPropertyTreeModel;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.io.Serializable;
import java.beans.IntrospectionException;

/**
 *
 */
public class NavigationTreeBean implements java.io.Serializable {
    private transient TreeModel _model = null;

    public NavigationTreeBean() {
    }

    public TreeModel getModel() throws IntrospectionException {
        if (_model == null) {
            _model = new ChildPropertyTreeModel(new TreeDataDemo().getRoot(), "children");
        }
        return _model;
    }

    public String getActionString() {
        return TreeNodeBean.getAction();
    }

    public static class TreeNodeBean implements Serializable {
        private String _name = null;
        private String _gender = null;
        private static String _actionString = null;

        private List<TreeNodeBean> _children;

        public TreeNodeBean(String _name, String _gender) {
            this._name = _name;
            this._gender = _gender;
        }

        public String getName() {
            return _name;
        }

        public void setName(String text) {
            _name = text;
        }

        public void doAction() {
            _actionString = "You clicked <b>" + _name + "</b>";
        }

        public static String getAction() {
            return _actionString;
        }

        public void setChildren(List<TreeNodeBean> nodes) {
            _children = new ArrayList<TreeNodeBean>(nodes);
        }

        public List<TreeNodeBean> getChildren() {
            if (_children == null)
                return null;

            return Collections.unmodifiableList(_children);
        }

        public String getGender() {
            return _gender;
        }

        public void setGender(String gender) {
            this._gender = gender;
        }
    }

    public static class TreeDataDemo {
        private ArrayList<TreeNodeBean> root = new ArrayList<TreeNodeBean>();

        private TreeNodeBean _createNode(String name, String gender) {
            TreeNodeBean data = new TreeNodeBean(name, gender);
            return data;
        }

        public ArrayList<TreeNodeBean> getRoot() {
            return root;
        }

        public TreeDataDemo() {
            TreeNodeBean matt = _createNode("Matt", "male");
            TreeNodeBean john = _createNode("John", "male");
            TreeNodeBean ira = _createNode("Ira", "female");
            TreeNodeBean tom = _createNode("Tom", "male");
            TreeNodeBean jack = _createNode("Jack", "male");
            TreeNodeBean victoria = _createNode("Victoria", "female");
            TreeNodeBean angelina = _createNode("Angelina", "female");
            TreeNodeBean mark = _createNode("Mark", "male");
            TreeNodeBean kate = _createNode("Kate", "female");
            TreeNodeBean lucy = _createNode("Lucy", "female");
            TreeNodeBean amy = _createNode("Amy", "female");
            TreeNodeBean victor = _createNode("Victor", "male");

            root.add(matt);

            ArrayList<TreeNodeBean> list_0 = new ArrayList<TreeNodeBean>();
            list_0.add(john);
            list_0.add(victoria);
            list_0.add(kate);
            list_0.add(lucy);
            list_0.add(amy);
            list_0.add(victor);
            matt.setChildren(list_0);

            ArrayList<TreeNodeBean> list_0_0 = new ArrayList<TreeNodeBean>();
            list_0_0.add(ira);
            list_0_0.add(jack);
            john.setChildren(list_0_0);
            ArrayList<TreeNodeBean> list_0_0_0 = new ArrayList<TreeNodeBean>();
            list_0_0_0.add(tom);
            ira.setChildren(list_0_0_0);

            ArrayList<TreeNodeBean> list_0_1 = new ArrayList<TreeNodeBean>();
            list_0_1.add(angelina);
            list_0_1.add(mark);
            victoria.setChildren(list_0_1);
        }

    }
}
