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
package org.apache.myfaces.trinidaddemo.components.panel.page;

import org.apache.myfaces.trinidad.model.ViewIdPropertyMenuModel;
import org.apache.myfaces.trinidad.model.ChildPropertyTreeModel;

import java.util.List;
import java.util.ArrayList;

/**
 *
 */
public class PageBean implements java.io.Serializable {

    private ViewIdPropertyMenuModel _model = null;
    private List<Object> _aliasList = null;
    private boolean _aliasListAdded = false;
    private String demoText;

    public PageBean() {

        ArrayList<PageNavigationItem> arayList = new ArrayList<PageNavigationItem>();

        PageNavigationItem page1 = new PageNavigationItem();
        PageNavigationItem page2 = new PageNavigationItem();
        PageNavigationItem page3 = new PageNavigationItem();
        PageNavigationItem page4 = new PageNavigationItem();

        page1.setLabel("Home");
        page2.setLabel("Careers");
        page3.setLabel("ContactListBean");
        page4.setLabel("Sign In");

        // Todo Here is a layout problem (each item is rendered in its ovn menu) ... I think we face with a unsuited
        // befaviour of the page/nodeStamp facet.
        // From the visual point of view the result is a worst aspect of the menu overall.
        arayList.add(page1);
        //arayList.add(page2);
        //arayList.add(page 3);
        //arayList.add(page4);

        ChildPropertyTreeModel childProperty = new ChildPropertyTreeModel();
        childProperty.setWrappedData(arayList);
        _model = new ViewIdPropertyMenuModel();
        _model.setWrappedData(childProperty);
    }

    /**
     * @param model an instance of ViewIdPropertyMenuModel
     */
    public void setModel(ViewIdPropertyMenuModel model) {
        _model = model;
        _aliasListAdded = false;

    }

    public String getDemoText() {
        return demoText;
    }

    public String getActionString() {
        return PageNavigationItem.getAction();
    }

    public void setDemoText1(String demoText) {
        this.demoText = "Text 1";
    }

    public ViewIdPropertyMenuModel getModel() {
        if (_model != null && !_aliasListAdded) {
            _aliasListAdded = true;
            if (_aliasList != null && !_aliasList.isEmpty()) {
                int size = _aliasList.size();
                if (size % 2 == 1)
                    size = size - 1;

                for (int i = 0; i < size; i = i + 2) {
                    _model.addViewId(_aliasList.get(i).toString(),
                            _aliasList.get(i + 1).toString());
                }
            }
        }
        return _model;
    }

    public List<Object> getAliasList() {
        return _aliasList;
    }


    public static class PageNavigationItem {
        private String _label = null;
        private static String _actionString = "This is the <b>Home</b> page";

        public PageNavigationItem() {
        }

        public void setLabel(String label) {
            _label = label;
        }

        public String getLabel() {
            return _label;
        }

        public void doAction() {
            _actionString = "This is the <b>" + _label + "</b> page";
        }

        public static String getAction() {
            return _actionString;
        }
    }
}
