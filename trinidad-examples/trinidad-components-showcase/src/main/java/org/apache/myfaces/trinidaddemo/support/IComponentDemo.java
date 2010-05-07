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

import java.util.List;
import java.io.Serializable;

/**
 * Defines a component demo; a component demo groups together at least one or more variants of the same component.
 */
public interface IComponentDemo extends Serializable {

	/**
	 * Returns the unique identifier of this component demo, unique across the whole application.
	 *
	 * @return the unique identifier of this component demo.
	 */
	ComponentDemoId getId();

    /**
     * Returns the path which can be used to navigate to this component variant demo.
     *
     * @return the path.
     */
    String getDestination();

	/**
	 * Returns the category this component demo belongs to.
	 *
	 * @return the category or null if none is available.
	 */
	IComponentDemoCategory getCategory();    

    /**
     * Returns the path to the corresponding JSF fragment which represents the summary of this demo.
     *
     * @return the resource path, or null if none exists
     */
    String getSummaryResourcePath();

	/**
	 * If this component demo is a dynamic one, this method returns the path to the backing bean's java source file.
	 *
	 * @return the path to the backing bean's java source file.
	 */
	String getBackingBeanResourcePath();

	/**
	 * Returns the display name of this component demo.
	 * 
	 * @return the display name.
	 */
	String getDisplayName();
	
	/**
	 * Returns a list containing all available demo variants of the corresponding component.
	 * 
	 * @return the list of available variants.
	 */
	List<IComponentVariantDemo> getVariants();
	
	/**
	 * Returns the component variant demo identified by the given name.
	 * 
	 * @param name the name of the desired component variant demo.
	 * @return the component variant demo identified by the given name, or null if it's not available.
	 */
	IComponentVariantDemo getVariant(String name);
	
	/**
	 * Adds the specified component variant demo to this component demo.
	 * 
	 * @param variant the component variant demo to add.
	 */
	void addComponentDemoVariant(IComponentVariantDemo variant);
	
	/**
	 * Sets this component demo's category to be the specified one.
	 * 
	 * @param category the new category of this component demo.
	 */
	void setCategory(IComponentDemoCategory category);
	
	/**
	 * Returns a comma separated list of CSS styles classes to be applied to the columns of the component's demo dynamic part
	 * 
	 * @return  a comma separated list of CSS styles classes.
	 */
	String getColumnStyleClassNames();    

    /**
     * Returns the MyFaces tag documentation link of this component demo.
     *
     * @return the MyFaces tag documentation link
     */
    String getTagDocumentationLink();

    /**
     * Returns the MyFaces skining key documentation link of this component demo.
     *
     * @return the MyFaces skining key documentation link
     */
    String getSkinDocumentationLink();

	/**
	 * Returns true if this component supports more than one variant, otherwise returns false.
	 * 
	 * @return true if this component supports more than one variant, false if not.
	 */
	boolean isSupportsMultipleVariants();
}
