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

import java.io.Serializable;

/**
 * This interface defines a component variant demo.
 */
public interface IComponentVariantDemo extends Serializable {

	/**
	 * Returns the unique identifier of this component demo, unique across the whole application. 
	 * 
	 * @return the unique identifier of this component demo. 
	 */
	ComponentDemoId getId();
	
	/**
	 * Returns the title to be shown at the top in the corresponding JSF page. 
	 *  
	 * @return the title or null if none is available.
	 */
	String getTitle();

    /**
     * Returns a short description about this component demo variant. Used as a value for  <meta name="description"/>
     * 
     * @return the short description.
     */
    String getDescription();    

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
	 * Returns  the name of this variant.
	 * 
	 * @return the name or null if none is available.
	 */
	IComponentDemoVariantId getVariantId();

    /**
     * @return
     */
    String getVariantDisplayName();

    /**
	 * Returns the paths to the corresponding JSF pages.
	 *
	 * @return the paths or null if none is available.
	 */
	String[] getJsfResourcePaths();    

    /**
	 * Returns the entry page path to the corresponding JSF page.
	 *
	 * @return the path or null if none is available.
	 */
	String getEntryPagePath();

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
	 * Returns true of this component demo is a static one, e.g. there is no dynamic behavior associated with it and  
	 * there is no backing bean defined for it.
	 * 
	 * @return true if this component demo is static, otherwise returns false.
	 */
	boolean isStatic();
}
