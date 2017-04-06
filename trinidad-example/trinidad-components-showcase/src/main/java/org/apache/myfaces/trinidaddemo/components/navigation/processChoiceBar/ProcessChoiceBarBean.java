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
package org.apache.myfaces.trinidaddemo.components.navigation.processChoiceBar;

import org.apache.myfaces.trinidad.model.ViewIdPropertyMenuModel;
import org.apache.myfaces.trinidad.model.ProcessUtils;
import org.apache.myfaces.trinidad.model.ChildPropertyTreeModel;

import java.beans.IntrospectionException;
import java.util.List;
import java.util.ArrayList;

/**
 *
 */
public class ProcessChoiceBarBean extends ViewIdPropertyMenuModel {

    private String _maxPathKey;
    private List<SurveyPage> _processChoiceBarPageList = new ArrayList<SurveyPage>();

    public ProcessChoiceBarBean() {
        super();
        this.setViewIdProperty("viewId");
        this.setProcessChoiceBarPageList();

    }

    public ProcessChoiceBarBean(Object instance, String viewIdProperty) throws IntrospectionException {
        super(instance, "viewId");
        setProcessChoiceBarPageList();
        this.setWrappedData(_processChoiceBarPageList);
    }


    public ProcessChoiceBarBean(Object instance, String viewIdProperty, String maxPathKey) throws IntrospectionException {
        super(instance, "viewId");
        setProcessChoiceBarPageList();
        this.setWrappedData(_processChoiceBarPageList);
        setMaxPathKey(maxPathKey);
    }

    private void setProcessChoiceBarPageList() {
        SurveyPage processChoiceBar1 = new SurveyPage("/components/navigation/processChoiceBar/processChoiceBar.xhtml", "First Step");
        processChoiceBar1.setOutcome("processChoiceBar1");
        _processChoiceBarPageList.add(processChoiceBar1);

        SurveyPage processChoiceBar2 = new SurveyPage("/components/navigation/processChoiceBar/processChoiceBar2.xhtml", "Second Step");
        processChoiceBar2.setOutcome("processChoiceBar2");
        _processChoiceBarPageList.add(processChoiceBar2);

        SurveyPage processChoiceBar3 = new SurveyPage("/components/navigation/processChoiceBar/processChoiceBar3.xhtml", "Third Step");
        processChoiceBar3.setOutcome("processChoiceBar3");
        _processChoiceBarPageList.add(processChoiceBar3);

        ChildPropertyTreeModel childProperty = new ChildPropertyTreeModel();
        childProperty.setWrappedData(_processChoiceBarPageList);
        this.setWrappedData(childProperty);
    }

    public boolean isImmediate() {
        String maxPathKey = getMaxPathKey();
        if (maxPathKey == null)
            return ProcessUtils.isImmediate(this, false);
        else {
            Object maxPath = ProcessUtils.getMaxVisitedRowKey(this, maxPathKey);
            return ProcessUtils.isImmediate(this, maxPath, false);
        }
    }

    public boolean isReadOnly() {
        String maxPathKey = getMaxPathKey();
        if (maxPathKey == null)
            return ProcessUtils.isReadOnly(this, true);
        else {
            Object maxPath = ProcessUtils.getMaxVisitedRowKey(this, maxPathKey);
            return ProcessUtils.isReadOnly(this, maxPath, true);
        }
    }

    public boolean isVisited() {
        String maxPathKey = getMaxPathKey();
        if (maxPathKey == null) {
            return ProcessUtils.isVisited(this, false);
        } else {
            Object maxPath = ProcessUtils.getMaxVisitedRowKey(this, maxPathKey);
            return ProcessUtils.isVisited(this, maxPath, false);
        }
    }

    public void clearMaxPath() {
        String maxPathKey = getMaxPathKey();
        if (maxPathKey != null)
            ProcessUtils.clearMaxPath(maxPathKey);
    }

    public void setMaxPathKey(String maxPathKey) {
        _maxPathKey = maxPathKey;
    }

    public String getMaxPathKey() {
        return _maxPathKey;
    }

    public static class SurveyPage implements java.io.Serializable {
        private String _viewId;
        private String _outcome;
        private String _label;
        private boolean _disabled;

        public SurveyPage() {
        }

        public SurveyPage(String viewId, String label) {
            setViewId(viewId);
            setLabel(label);
            //setDisabled(false);
        }

        public void setViewId(String viewId) {
            _viewId = viewId;
        }

        public String getViewId() {
            return _viewId;
        }

        public void setOutcome(String outcome) {
            _outcome = outcome;
        }


        public String getOutcome() {
            return _outcome;
        }

        public void setLabel(String label) {
            _label = label;
        }


        public String getLabel() {
            return _label;
        }


        public void setDisabled(boolean disabled) {
            _disabled = disabled;
        }

        public boolean isDisabled() {
            return _disabled;
        }
    }

}
