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

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemo;
import org.apache.myfaces.trinidaddemo.support.IComponentVariantDemo;
import org.apache.myfaces.trinidaddemo.support.jsf.JsfHelper;

/**
 * Phase listener that handles navigation based on URL parameters that identify a certain component demo and its variant.
 */
public class NavigationHandlerPhaseListener implements PhaseListener {

	private static final long serialVersionUID = -6002602992566868748L;

	private static final Log log = LogFactory.getLog(NavigationHandlerPhaseListener.class);

    public static final String COMPONENT_DEMO_ID_PARAM_NAME = "id";
    public static final String COMPONENT_DEMO_VARIANT_ID_PARAM_NAME = "variantId";

    public void beforePhase(PhaseEvent event) {
        FacesContext fc = event.getFacesContext();

        //ignore request to pages that don't require URL parameters
        if (fc.getViewRoot().getViewId().startsWith("/feature") ||
            fc.getViewRoot().getViewId().equals("/pages/demoStart.xhtml") ||
            fc.getViewRoot().getViewId().equals("/pages/demoSearch.xhtml")) {
                log.info("Ignore request to demoStart or demoSearch pages.");
                return;
        }

        NavigationHandlerBean navigationHandler = (NavigationHandlerBean) JsfHelper.getBean("navigationHandler");

        String componentDemoId = fc.getExternalContext().getRequestParameterMap().get(COMPONENT_DEMO_ID_PARAM_NAME);
        String variantId = fc.getExternalContext().getRequestParameterMap().get(COMPONENT_DEMO_VARIANT_ID_PARAM_NAME);

        //if the current component demo wasn't selected already and one or both URL parameters are missing,
        //add error message and redirect to demo start page
        if (navigationHandler.getCurrentComponentDemo() == null && (componentDemoId == null || variantId == null)) {
            FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR,
                "Error Msg.", "Either component demo id or variant name is missing!");

            fc.addMessage(null, msg);
            fc.getViewRoot().setViewId("/pages/demoStart.xhtml");

            log.error("Either component demo id or variant name is missing.");
            return;
        }

        if (componentDemoId == null || variantId == null) {
            return;
        }

        try {
            ComponentDemoId temp = ComponentDemoId.valueOf(componentDemoId);
            IComponentDemo componentDemo = ComponentDemoRegistry.getInstance().getComponentDemo(temp);

            IComponentVariantDemo resultingDemo = componentDemo.getVariant(variantId);
            if (resultingDemo == null) {
                FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR,
                    "Error Msg.", "Component with id: '" + componentDemoId + "' doesn't have a variant with name: '"+variantId+"'!");

                fc.addMessage(null, msg);
                fc.getViewRoot().setViewId("/pages/demoStart.xhtml");

                log.error("Component with id: '" + componentDemoId + "' doesn't have a variant with name: '"+variantId+"'!");
                return;
            }

            log.info("Navigation successfull to component demo id: '"+componentDemoId+"', variant name: '"+variantId+"'");
            navigationHandler.setCurrentComponentVariantDemo(resultingDemo);
        }
        catch (IllegalArgumentException e) {
            FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_ERROR,
                    "Error Msg.", "Component id: '" + componentDemoId + "' not found!");

            fc.addMessage(null, msg);
            fc.getViewRoot().setViewId("/pages/demoStart.xhtml");

            log.error("Component id: '" + componentDemoId + "' not found!");
        }             
    }

    public PhaseId getPhaseId() {
        return PhaseId.RENDER_RESPONSE;
    }

    public void afterPhase(PhaseEvent event) {
    }
}
