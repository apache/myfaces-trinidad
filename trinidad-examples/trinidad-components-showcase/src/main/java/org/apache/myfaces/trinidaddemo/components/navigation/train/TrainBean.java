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
package org.apache.myfaces.trinidaddemo.components.navigation.train;

import org.apache.myfaces.trinidad.model.ChildPropertyTreeModel;
import org.apache.myfaces.trinidad.model.ProcessMenuModel;
import org.apache.myfaces.trinidaddemo.NavigationHandlerBean;
import org.apache.myfaces.trinidaddemo.support.jsf.JsfHelper;

import java.beans.IntrospectionException;
import java.util.ArrayList;
import java.util.List;

/**
 *
 */
public class TrainBean extends ProcessMenuModel /*implements java.io.Serializable*/ {
    private ArrayList<TrainNavigationItem> arrayList = new ArrayList<TrainNavigationItem>();
    private NavigationHandlerBean navigationHandler = (NavigationHandlerBean) JsfHelper.getBean("navigationHandler");


    public TrainBean() {
        super();
        addList();      
    }

    public TrainBean(Object instance, String viewIdProperty) throws IntrospectionException {
        super(instance, viewIdProperty);
        addList();
    }

    public TrainBean(Object instance, String viewIdProperty, String maxPathKey) throws IntrospectionException {
        super(instance, viewIdProperty);
        setMaxPathKey(maxPathKey);
        addList();
    }

    public void addList() {
        TrainNavigationItem page1 = new TrainNavigationItem("General info", "trainGeneralInfo");
        page1.setViewId("/components/navigation/train/generalInfo.xhtml");
        TrainNavigationItem page2 = new TrainNavigationItem("Company info", "trainCompanyInfo");
        page2.setViewId("/components/navigation/train/companyInfo.xhtml");
        TrainNavigationItem page3 = new TrainNavigationItem("JSF survey", "trainJsfSurvey");
        page3.setViewId("/components/navigation/train/jsfSurvey.xhtml");
        TrainNavigationItem page4 = new TrainNavigationItem("Trinidad survey", "trainTrinidadSurvey");
        page4.setViewId("/components/navigation/train/trinidadSurvey.xhtml");
        TrainNavigationItem page5 = new TrainNavigationItem("You are done!", "trainYouAreDone");
        page5.setViewId("/components/navigation/train/youAreDone.xhtml");

        arrayList.add(page1);
        arrayList.add(page2);
        arrayList.add(page3);
        arrayList.add(page4);
        arrayList.add(page5);

        setViewIdProperty("viewId");

        ChildPropertyTreeModel childProperty = new ChildPropertyTreeModel();
        childProperty.setChildProperty("children");
        childProperty.setWrappedData(arrayList);

        this.setWrappedData(childProperty);
    }

    public ArrayList<TrainNavigationItem> getArrayList() {
        return arrayList;
    }


    public static class TrainNavigationItem implements java.io.Serializable {
        
        private static final long serialVersionUID = 375702448013892058L;

        private String _label = null;
        private String _viewId = null;
        private String _destination = null;
        private List<?> _children = null;
        private String _outcome = null;


        public TrainNavigationItem() {
        }

        public TrainNavigationItem(String _label, String _outcome) {
            setLabel(_label);
            setOutcome(_outcome);
        }

        public void setLabel(String label) {
            _label = label;
        }

        public String getLabel() {
            return _label;
        }

        public void setOutcome(String outcome) {
            this._outcome = outcome;
        }

        public String getOutcome() {
            return _outcome;
        }

        public void setViewId(String viewId) {
            _viewId = viewId;
        }

        public String getViewId() {
            return _viewId;
        }

        public void setDestination(String destination) {
            _destination = destination;
        }

        public String getDestination() {
            return _destination;
        }

        public List<?> getChildren() {
            return _children;
        }

        public void setChildren(List<?> children) {
            _children = children;
        }

    }
}
