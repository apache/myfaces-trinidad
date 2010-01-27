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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidaddemo.support.IComponentDemo;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoCategory;
import org.apache.myfaces.trinidaddemo.support.IComponentVariantDemo;

/**
 * Base implementation of {@link IComponentDemo} interface. Extend this class and implement required methods for concrete 
 * component demos.
 */
public abstract class AbstractComponentDemo implements IComponentDemo {

    private static final String DEFAULT_VARIANT_NAME = "Default";

	private ComponentDemoId id;
	private String displayName;
    private IComponentDemoVariantId variantId;
    private String variantDisplayName;

    private String title;

    private enum VARIANTS implements IComponentDemoVariantId {
        Default
    }
	
	private IComponentDemoCategory category;
	
	private Map<String, IComponentVariantDemo> variantsByName;
	private List<IComponentVariantDemo> variants;

    /**
     * Constructor.
     * 
     * @param id
     * @param displayName
     */
    public AbstractComponentDemo(ComponentDemoId id, String displayName) {
        this(id, displayName, VARIANTS.Default, DEFAULT_VARIANT_NAME);
    }

    /**
     * Constructor.
     * 
     * @param id
     * @param displayName
     * @param variantId
     */
    public AbstractComponentDemo(ComponentDemoId id, String displayName, IComponentDemoVariantId variantId) {
        this(id, displayName, variantId, variantId.toString());
    }

	/**
	 * Constructor.
	 * 
	 * @param id the unique id of this component demo.
	 * @param displayName the display name of this component demo.
     * @param variantId the id of the variant this component demo represents.
     * @param variantDisplayName the display name of the variant this component demo represents.
	 */
	public AbstractComponentDemo(ComponentDemoId id, String displayName,
            IComponentDemoVariantId variantId, String variantDisplayName) {

		this.id = id;
		this.displayName = displayName;
        this.variantId = variantId;
        this.variantDisplayName = variantDisplayName;
		
		this.variantsByName = new HashMap<String, IComponentVariantDemo>();
		this.variants = new ArrayList<IComponentVariantDemo>();
		
		//by default the component demo itself is a variant, might be the only one in the end
		addComponentDemoVariant(this);

        //determine the title to show at the top of the corresponding component demo page
        if (DEFAULT_VARIANT_NAME.equals(DEFAULT_VARIANT_NAME)) {
            title = getDisplayName();
        }
        else {
            title = getDisplayName() + " - " + getVariantDisplayName();
        }
	}
	
	public ComponentDemoId getId() {
		return id;
	}

    public String getDestination() {
        StringBuilder url = new StringBuilder();
        url.append("/component-demo/");
        url.append(getId().toString());
        url.append("-");
        url.append(getVariantId());       

        return url.toString();
    }

	public String getDisplayName() {
		return displayName;
	}
	
	public IComponentDemoVariantId getVariantId() {
        return variantId;
    }

    public String getVariantDisplayName() {
        return variantDisplayName;
    }

    public String getTitle() {
		return title;
	}

    public String getDescription() {
        return ComponentVariantDemoDescriptionProvider.getDescription(FacesContext.getCurrentInstance(), this);
    }

    public IComponentDemoCategory getCategory() {
		return category;
	}
	
	public void setCategory(IComponentDemoCategory category) {
		this.category = category;
	}
	
	public void addComponentDemoVariant(IComponentVariantDemo variant) {
		if (variantsByName.get(variant.getVariantId()) != null) {
			throw new IllegalArgumentException("Variant with name '"+variant.getVariantId()+"' already added to '"+getId()+"' demo!");
		}
		
		variants.add(variant);
		variantsByName.put(variant.getVariantId().toString(), variant);
	}

	public List<IComponentVariantDemo> getVariants() {
		return variants;
	}
	
	public IComponentVariantDemo getVariant(String name) {
		return variantsByName.get(name);
	}

	public boolean isSupportsMultipleVariants() {
		if (getVariants().size() > 1) {
			return true;
		}
		
		return false;
	}
	
	public String getColumnStyleClassNames() {
		if (isSupportsMultipleVariants()) {
			return "column75percent2,column25percent2";
		}
		
		return "column100percent";
	}
	
	public String getBackingBeanResourcePath() {
		return null;
	}

	public boolean isStatic() {
		return getBackingBeanResourcePath() == null;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof AbstractComponentDemo) {
			return getId() == ((AbstractComponentDemo)obj).getId();
		}
		
		return false;
	}

	@Override
	public int hashCode() {
		return getId() != null ? getId().hashCode() : super.hashCode();
	}

	@Override
	public String toString() {
		return getDisplayName();
	}
}
