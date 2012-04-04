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

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import java.util.ArrayList;
import java.io.Serializable;

/**
 *
 */
public class TrainBean extends ProcessMenuModel /*implements java.io.Serializable*/ {

    public TrainBean() {

        ArrayList<TrainNavigationItem> arrayList = new ArrayList<TrainNavigationItem>();

        arrayList.add(new TrainNavigationItem("1.  General info", "trainGeneralInfo", "/components/navigation/train/generalInfo.xhtml"));
        arrayList.add(new TrainNavigationItem("2.  Company info", "trainCompanyInfo", "/components/navigation/train/companyInfo.xhtml"));
        arrayList.add(new TrainNavigationItem("3.  JSF survey", "trainJsfSurvey", "/components/navigation/train/jsfSurvey.xhtml"));
        arrayList.add(new TrainNavigationItem("4.  Trinidad survey", "trainTrinidadSurvey", "/components/navigation/train/trinidadSurvey.xhtml"));
        arrayList.add(new TrainNavigationItem("5.  You are done!", "trainYouAreDone", "/components/navigation/train/youAreDone.xhtml"));

        setViewIdProperty("viewId");

        FacesContext fc = FacesContext.getCurrentInstance();
        NavigationHandlerBean navigationHandler = (NavigationHandlerBean) FacesContext.getCurrentInstance().getApplication().getELResolver().getValue(fc.getELContext(), null, "navigationHandler");

        if (TrainDemo.VARIANTS.MaxVisited == navigationHandler.getCurrentComponentVariantDemo().getVariantId()) {
            setMaxPathKey("TRAIN_DEMO_MAX_PATH_KEY");
        }
        
        setWrappedData(new ChildPropertyTreeModel(arrayList,null));
    }


    public class TrainNavigationItem implements Serializable {

        public TrainNavigationItem(String _label, String _outcome, String _viewId) {
            setLabel(_label);
            setOutcome(_outcome);
            setViewId (_viewId);
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


        private String _viewId = null;
        private String _label = null;
        private String _outcome = null;
        private static final long serialVersionUID = 375702448013892058L;
    }
}
