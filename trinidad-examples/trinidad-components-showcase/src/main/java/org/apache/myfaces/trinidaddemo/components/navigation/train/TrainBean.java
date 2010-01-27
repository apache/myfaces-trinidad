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

import java.beans.IntrospectionException;
import java.util.ArrayList;
import java.util.List;

/**
 *
 */
public class TrainBean extends ProcessMenuModel /*implements java.io.Serializable*/ {
    private ArrayList<TrainNavigationItem> arrayList = new ArrayList<TrainNavigationItem>();


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
        TrainNavigationItem page1 = new TrainNavigationItem("First Step", "train");
        TrainNavigationItem page2 = new TrainNavigationItem("Second Step", "train2");
        TrainNavigationItem page3 = new TrainNavigationItem("Third Step", "train3");
        TrainNavigationItem page4 = new TrainNavigationItem("Fourth Step", "train4");
        TrainNavigationItem page5 = new TrainNavigationItem("Fifth Step", "train5");
        TrainNavigationItem page6 = new TrainNavigationItem("Sixth Step", "train6");
        TrainNavigationItem page7 = new TrainNavigationItem("Seventh Step", "train7");

        arrayList.add(page1);
        arrayList.add(page2);
        arrayList.add(page3);
        arrayList.add(page4);
        arrayList.add(page5);
        arrayList.add(page6);
        arrayList.add(page7);

        ChildPropertyTreeModel childProperty = new ChildPropertyTreeModel();
        childProperty.setChildProperty("children");
        childProperty.setWrappedData(arrayList);


        setViewIdProperty("viewId");
        setMaxPathKey("TRAIN_DEMO_MAX_PATH_KEY");
        setWrappedData(childProperty);
    }


    public static class TrainNavigationItem implements java.io.Serializable {
        private String _label = null;
        private String _outcome = null;
        private String _viewId = null;
        private String _destination = null;
        private List<?> _children = null;


        public TrainNavigationItem() {
        }

        public TrainNavigationItem(String _label, String _outcome) {
            this._label = _label;
            this._outcome = _outcome;
        }

        public void setLabel(String label) {
            _label = label;
        }

        public String getLabel() {
            return _label;
        }

        public void setOutcome(String outcome) {
            _outcome = outcome;
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
