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
package org.apache.myfaces.trinidaddemo.support;

import org.apache.myfaces.trinidaddemo.NavigationHandlerBean;
import org.apache.myfaces.trinidaddemo.ComponentDemoRegistry;
import org.apache.myfaces.trinidaddemo.FeatureDemoRegistry;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.Filter;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletContext;
import javax.servlet.FilterConfig;
import javax.faces.lifecycle.LifecycleFactory;
import javax.faces.lifecycle.Lifecycle;
import javax.faces.FactoryFinder;
import javax.faces.context.FacesContextFactory;
import javax.faces.context.FacesContext;
import java.io.IOException;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 *
 */
public class PrettyUrlFilter implements Filter {

    private enum TYPES{
		component,
		feature
	}

    private static final Logger _LOG = Logger.getLogger(PrettyUrlFilter.class.getName());

    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        String requestURI = getRequestURI(request);

        FacesContext fc = getFacesContext(request, response);
        NavigationHandlerBean navigationHandler = (NavigationHandlerBean) fc.getApplication().getELResolver().getValue(fc.getELContext(), null, "navigationHandler");

        if (getDemoType(requestURI).equals(TYPES.component)){
            IComponentVariantDemo componentVariantDemo = getComponentVariantDemo(requestURI);
            navigationHandler.setCurrentComponentVariantDemo(componentVariantDemo);

            _LOG.log(Level.INFO,"Forwarding request [" + requestURI + "] to view [" + componentVariantDemo.getEntryPagePath()+"]");
            if (!response.isCommitted()) {
                request.getRequestDispatcher("/faces"+componentVariantDemo.getEntryPagePath()).forward(request, response);
            }
        }

        if (getDemoType(requestURI).equals(TYPES.feature)){
            IFeatureDemo featureDemo = getFeatureDemo(requestURI);
            navigationHandler.setCurrentFeatureDemo(featureDemo);

            _LOG.log(Level.INFO,"Forwarding request [" + requestURI + "] to view [" + featureDemo.getPagePath()+"]");
            if (!response.isCommitted()) {
                request.getRequestDispatcher("/faces"+featureDemo.getPagePath()).forward(request, response);
            }
        }
    }

    /**
     * @param request
     * @return
     */
    private String getRequestURI(final ServletRequest request) {
        String requestURI = ((HttpServletRequest) request).getRequestURI();
        String contextPath = ((HttpServletRequest) request).getContextPath();
        if (requestURI.startsWith(contextPath)) {
            requestURI = requestURI.substring(contextPath.length());
        }
        
        return requestURI;
    }

    private TYPES getDemoType(String requestURI) {
        String demoType = requestURI.substring(1,requestURI.indexOf("-"));
        
        return TYPES.valueOf(demoType);
    }
    /**
     * @param requestURI
     * @return
     */
    private IComponentVariantDemo getComponentVariantDemo(String requestURI) {
        int idPathIndex = requestURI.lastIndexOf("/");
        if (idPathIndex == -1) {
            return null;
        }

        String idPath = requestURI.substring(idPathIndex + 1);
        String[] values = idPath.split("-");

        ComponentDemoId componentDemoId = ComponentDemoId.valueOf(values[0]);
        String variantName = values[1] ;
        
        IComponentDemo componentDemo = ComponentDemoRegistry.getInstance().getComponentDemo(componentDemoId);
        IComponentVariantDemo resultingDemo = componentDemo.getVariant(variantName);

        return resultingDemo;
    }        

    private IFeatureDemo getFeatureDemo(String requestURI) {
        int idPathIndex = requestURI.lastIndexOf("/");
        if (idPathIndex == -1) {
            return null;
        }

        String idPath = requestURI.substring(idPathIndex+1);
        FeatureDemoId featureDemoId = FeatureDemoId.valueOf(idPath);

        return FeatureDemoRegistry.getInstance().getFeatureDemo(featureDemoId);

    }

    /**
     * @param request
     * @param response
     * @return
     */
    private FacesContext getFacesContext(final ServletRequest request, final ServletResponse response) {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        if (facesContext != null) {
            return facesContext;
        }

        FacesContextFactory contextFactory = (FacesContextFactory) FactoryFinder
                .getFactory(FactoryFinder.FACES_CONTEXT_FACTORY);
        LifecycleFactory lifecycleFactory = (LifecycleFactory) FactoryFinder
                .getFactory(FactoryFinder.LIFECYCLE_FACTORY);
        Lifecycle lifecycle = lifecycleFactory.getLifecycle(LifecycleFactory.DEFAULT_LIFECYCLE);

        ServletContext servletContext = ((HttpServletRequest) request).getSession().getServletContext();
        facesContext = contextFactory.getFacesContext(servletContext, request, response, lifecycle);

        return facesContext;
    }

    public void init(FilterConfig filterConfig) throws ServletException {}
    public void destroy() {}
}
