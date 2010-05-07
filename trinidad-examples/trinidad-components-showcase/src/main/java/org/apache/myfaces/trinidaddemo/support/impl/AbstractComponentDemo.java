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

	private ComponentDemoId id;
	private String displayName;
    private IComponentVariantDemo defaultVariant;
	private IComponentDemoCategory category;
    private static final String tagDocPrefix = "http://myfaces.apache.org/trinidad/trinidad-api/tagdoc/tr_";
    private static final String skinDocPrefix = "http://myfaces.apache.org/trinidad/skin-selectors.html#";
	
	private Map<String, IComponentVariantDemo> variantsByName;
	private List<IComponentVariantDemo> variants;

	/**
	 * Constructor.
	 * 
	 * @param id the unique id of this component demo.
	 * @param displayName the display name of this component demo.
	 */
	public AbstractComponentDemo(ComponentDemoId id, String displayName) {

		this.id = id;
		this.displayName = displayName;

		this.variantsByName = new HashMap<String, IComponentVariantDemo>();
		this.variants = new ArrayList<IComponentVariantDemo>();

	}

    public void setDefaultVariant(IComponentDemoVariantId defaultVariantId){
        IComponentVariantDemo defVariant = getVariant(defaultVariantId.toString());

        if (defVariant == null )
            return;

        this.defaultVariant = defVariant;
    }

    public IComponentVariantDemo getDefaultVariant(){

        if (defaultVariant != null)
            return defaultVariant;
        else if (variants.size() != 0)
            return variants.get(0);
        else
            throw new UnsupportedOperationException("No demo variants declared for "+ this + " component");
    }

	public ComponentDemoId getId() {
		return id;
	}

    public String getDestination() {
        StringBuilder url = new StringBuilder();
        url.append("/component-demo/");
        url.append(getId().toString());
        url.append("-");
        url.append(getDefaultVariant().getVariantId());

        return url.toString();
    }

	public String getDisplayName() {
		return displayName;
	}

    public IComponentDemoCategory getCategory() {
		return category;
	}
	
	public void setCategory(IComponentDemoCategory category) {
		this.category = category;
	}
	
	public void addComponentDemoVariant(IComponentVariantDemo variant) {
		if (variantsByName.get(variant.getVariantId().toString()) != null) {
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
        
		return getVariants().size() > 1;
	}
	
	public String getColumnStyleClassNames() {
		if (isSupportsMultipleVariants()) {
			return "column75percent2,column25percent2";
		}
		
		return "column100percent";
	}

    public String getTagDocumentationLink(){
        return tagDocPrefix + this.getId().toString() + ".html";
    }

    public String getSkinDocumentationLink(){
        return skinDocPrefix + this.getId().toString();
    }

    public String getSummaryResourcePath() {
        return null;
    }

    public String getBackingBeanResourcePath() {
		return null;
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
