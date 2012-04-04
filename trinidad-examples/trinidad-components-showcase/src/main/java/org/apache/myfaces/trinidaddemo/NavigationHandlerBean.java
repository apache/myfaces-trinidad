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
package org.apache.myfaces.trinidaddemo;

import java.util.Collection;
import java.util.Map;

import org.apache.myfaces.trinidaddemo.support.util.EvalMapAdapter;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoCategoryId;
import org.apache.myfaces.trinidaddemo.support.IComponentVariantDemo;
import org.apache.myfaces.trinidaddemo.support.IComponentDemo;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoCategory;
import org.apache.myfaces.trinidaddemo.support.IFeatureDemoCategory;
import org.apache.myfaces.trinidaddemo.support.FeatureDemoCategoryId;
import org.apache.myfaces.trinidaddemo.support.IFeatureDemo;

import javax.faces.context.FacesContext;
import javax.faces.application.FacesMessage;
import javax.servlet.http.HttpServletRequest;

/**
 * Central navigation handler bean.
 */
public class NavigationHandlerBean {

    private String searchText;

    private ComponentDemoCategoryId defaultDisclosedCategoryId = ComponentDemoCategoryId.layout;
    private FeatureDemoCategoryId defaultFeatureDisclosedCategoryId = FeatureDemoCategoryId.changePersistence;

    private IComponentVariantDemo currentComponentVariantDemo;
    private IFeatureDemo currentFeatureDemo;

    /**
     * @return
     */
    public String validateSearch() {
        FacesContext fc = FacesContext.getCurrentInstance();

        if (((HttpServletRequest) fc.getExternalContext().getRequest()).getLocalName().equals("localhost")) {

            FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR,
                    "Error Msg.", "Search doesn't work offline!");

            fc.getViewRoot().setViewId("/pages/demoStart.xhtml");
            fc.addMessage(null, msg);

            return null;
            
        }
        else if ((searchText == null) || (searchText == "")) {

            FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR,
                    "Error Msg.", "The input Search field is empty!");

            fc.getViewRoot().setViewId("/pages/demoStart.xhtml");
            fc.addMessage(null, msg);

            return null;
        }

        return "searchForm";
    }

    /**
     * @return
     */
    public String showComponentDemo() {
        if (currentComponentVariantDemo != null) {
            return currentComponentVariantDemo.getEntryPagePath();
        }

        return null;
    }

    /**
     * @return
     */
    public Map<String, Object> getCategoryDisclosed() {
        return new CategoryDisclosedStateEvalMap();
    }

    /**
     * @return
     */
    public Map<String, Object> getFeatureCategoryDisclosed() {
        return new FeatureCategoryDisclosedStateEvalMap();
    }

    /**
     * @return
     */
    public Map<String, Object> getComponentDemoSelected() {
        return new ComponentDemoSelectedEvalMap();
    }

    /**
     * @return
     */
    public Map<String, Object> getFeatureDemoSelected() {
        return new FeatureDemoSelectedEvalMap();
    }

    /**
     * @return
     */
    public Map<String, Object> getComponentVariantDemoSelected() {
        return new ComponentVariantDemoSelectedEvalMap();
    }

    /**
     * @return
     */
    public IComponentDemo getCurrentComponentDemo() {
        return getCurrentComponentVariantDemo() != null ?
                ComponentDemoRegistry.getInstance().getComponentDemo(getCurrentComponentVariantDemo().getId()) :
                null;
    }

    /**
     * @return
     */
    public IFeatureDemo getCurrentFeatureDemo() {
        return currentFeatureDemo;
    }

    public void setCurrentFeatureDemo(IFeatureDemo featureDemo){
        this.currentFeatureDemo = featureDemo;    
    }

    /**
     * @return
     */
    public IComponentVariantDemo getCurrentComponentVariantDemo() {
        return currentComponentVariantDemo;
    }

    /**
     * @param componentVariantDemo
     */
    public void setCurrentComponentVariantDemo(IComponentVariantDemo componentVariantDemo) {
        this.currentComponentVariantDemo = componentVariantDemo;
    }

    /**
     * @return
     */
    public IComponentDemoCategory getCurrentCategory() {
        return getCurrentComponentVariantDemo() != null ? getCurrentComponentVariantDemo().getCategory() : null;
    }

    /**
     * @return
     */
    public Collection<IComponentDemoCategory> getDemoCategories() {
        return ComponentDemoRegistry.getInstance().getDemoCategories();
    }

    /**
     * @return
     */
    public Collection<IFeatureDemoCategory> getFeatureDemoCategories() {
        return FeatureDemoRegistry.getInstance().getDemoCategories();
    }

    /**
     * @return the searchText
     */
    public String getSearchText() {
        return searchText;
    }

    /**
     * @param searchText the searchText to set
     */
    public void setSearchText(String searchText) {
        this.searchText = searchText;
    }

    /**
     * @return
     */
    public String getCurrentPageTitle() {
        StringBuilder title = new StringBuilder();
        title.append("Trinidad Components Showcase");
        if (currentComponentVariantDemo != null) {
            title.append(" - ");
            title.append(currentComponentVariantDemo.getTitle());
            title.append("");
        }

        return title.toString();
    }

    /**
     * @return
     */
    public String getCurrentPageDescription() {
        if (currentComponentVariantDemo != null && currentComponentVariantDemo.getDescription() != null) {
            return currentComponentVariantDemo.getDescription();    
        }

        return "";
    }

    /**
     *
     */
    class CategoryDisclosedStateEvalMap extends EvalMapAdapter {

        @Override
        public Object get(Object key) {
            IComponentDemoCategory category = (IComponentDemoCategory) key;

            //if there is no concrete component demo selected yet, disclose the default category
            if (currentComponentVariantDemo == null &&
                defaultDisclosedCategoryId.equals(category.getId())) {
                    return true;
            }

            return category != null &&
                   currentComponentVariantDemo != null &&
                   category.equals(currentComponentVariantDemo.getCategory());
        }
    }

    /**
     *
     */
    class FeatureCategoryDisclosedStateEvalMap extends EvalMapAdapter {

        @Override
        public Object get(Object key) {
            IFeatureDemoCategory category = (IFeatureDemoCategory) key;

            //if there is no concrete component demo selected yet, disclose the default category
            if (currentFeatureDemo == null &&
                defaultFeatureDisclosedCategoryId.equals(category.getId())) {
                    return true;
            }

            return category != null &&
                   currentFeatureDemo != null &&
                   category.equals(currentFeatureDemo.getCategory());
        }
    }

    /**
     *
     */
    class ComponentVariantDemoSelectedEvalMap extends EvalMapAdapter {

        @Override
        public Object get(Object key) {
            IComponentVariantDemo variantDemo = (IComponentVariantDemo) key;

            return variantDemo != null &&
                    getCurrentComponentVariantDemo() != null &&
                    variantDemo.equals(getCurrentComponentVariantDemo());
        }
    }

    /**
     *
     */
    class ComponentDemoSelectedEvalMap extends EvalMapAdapter {

        @Override
        public Object get(Object key) {
            IComponentDemo componentDemo = (IComponentDemo) key;

            return componentDemo != null &&
                    getCurrentComponentDemo() != null &&
                    componentDemo.equals(getCurrentComponentDemo());
        }
    }

    class FeatureDemoSelectedEvalMap extends EvalMapAdapter {

        @Override
        public Object get(Object key) {
            IFeatureDemo featureDemo = (IFeatureDemo) key;

            return featureDemo != null &&
                    getCurrentFeatureDemo() != null &&
                    featureDemo.equals(getCurrentFeatureDemo());
        }
    }
}
