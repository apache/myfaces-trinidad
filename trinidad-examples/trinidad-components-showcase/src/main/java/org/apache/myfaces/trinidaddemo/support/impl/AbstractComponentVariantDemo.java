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
package org.apache.myfaces.trinidaddemo.support.impl;

import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoCategory;
import org.apache.myfaces.trinidaddemo.support.IComponentVariantDemo;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;

import javax.faces.context.FacesContext;

/**
 * Base implementation of the {@link IComponentVariantDemo} interface. Extend this class and implement required methods for concrete 
 * component variant demos.
 */
public abstract class AbstractComponentVariantDemo implements IComponentVariantDemo {

    private final static String DEFAULT = "default";

	private IComponentDemoVariantId variantId;
    private String variantDisplayName;

	private AbstractComponentDemo componentDemo;
	
	/**
	 * Constructor.
	 * 
	 * @param variantId the name of this component variant demo.
     * @param variantDisplayName the display name of this component variant demo.
	 * @param componentDemo the component demo owning this variant demo.
	 */
	public AbstractComponentVariantDemo(IComponentDemoVariantId variantId, String variantDisplayName,
            AbstractComponentDemo componentDemo) {
        
		this.variantId = variantId;
        this.variantDisplayName = variantDisplayName;
		this.componentDemo = componentDemo;
	}

	public ComponentDemoId getId() {
		return componentDemo.getId();
	}

    public IComponentDemoVariantId getVariantId() {
		return variantId;
	}

    public String getVariantDisplayName() {
        return variantDisplayName + getDefault();
    }

    public IComponentDemoCategory getCategory() {
		return componentDemo.getCategory();
	}

    public AbstractComponentDemo getComponentDemo(){
        return componentDemo;
    }

    public String getTitle() {
		StringBuilder builder = new StringBuilder();
		builder.append(componentDemo.getDisplayName());
        if (componentDemo.getVariants().size() > 1){
		    builder.append(" - ");
		    builder.append(getVariantDisplayName());
        }
		
		return builder.toString();
	}

    public String getDescription() {
        return ComponentVariantDemoDescriptionProvider.getDescription(FacesContext.getCurrentInstance(), this);
    }

    public String getDestination() {
        StringBuilder url = new StringBuilder();
        url.append("/component-demo/");
        url.append(getId().toString());
        url.append("-");
        url.append(getVariantId());

        return url.toString();
    }
	
	public String getBackingBeanResourcePath() {
		return null;
	}

	public boolean isStatic() {
		return getBackingBeanResourcePath() == null;
	}

    private String getDefault() {
        if (componentDemo.getDefaultVariant().equals(this))
            return " ("+DEFAULT+")";
        else
            return "";
    }
}
