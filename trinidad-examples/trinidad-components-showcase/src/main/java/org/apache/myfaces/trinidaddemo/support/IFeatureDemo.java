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
 *
 */
public interface IFeatureDemo extends Serializable {

	/**
	 * Returns the unique identifier of this component demo, unique across the whole application.
	 *
	 * @return the unique identifier of this component demo.
	 */
	FeatureDemoId getId();

	/**
	 * Returns the title to be shown at the top in the corresponding JSF page.
	 *
	 * @return the title or null if none is available.
	 */
	String getTitle();

	/**
	 * Returns the display name of this feature demo.
	 *
	 * @return the display name.
	 */
	String getDisplayName();

    /**
     * Returns the path which can be used to navigate to this feature variant demo.
     *
     * @return the path.
     */
    String getDestination();

	/**
	 * Returns the category this feature demo belongs to.
	 *
	 * @return the category or null if none is available.
	 */
	IFeatureDemoCategory getCategory();

    /**
	 * Returns the entry page path to the corresponding JSF page.
	 *
	 * @return the path or null if none is available.
	 */
	String getPagePath();

	/**
	 * Sets this feature demo's category to be the specified one.
	 *
	 * @param category the new category of this feature demo.
	 */
	void setCategory(IFeatureDemoCategory category);
     
}
