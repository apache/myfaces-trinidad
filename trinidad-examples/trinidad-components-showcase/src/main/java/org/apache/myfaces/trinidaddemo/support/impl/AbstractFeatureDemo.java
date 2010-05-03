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

import org.apache.myfaces.trinidaddemo.support.IFeatureDemo;
import org.apache.myfaces.trinidaddemo.support.FeatureDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;
import org.apache.myfaces.trinidaddemo.support.IFeatureDemoCategory;

/**
 *
 */
public abstract class AbstractFeatureDemo implements IFeatureDemo {

    private static final String DEFAULT_VARIANT_NAME = "Default";

	private FeatureDemoId id;
	private String displayName;
    private String title;
    private String pagePath;
	private IFeatureDemoCategory category;

	/**
	 * Constructor.
	 *
	 * @param id the unique id of this feature demo.
	 * @param displayName the display name of this feature demo.
     * @param pagePath th entry page path to the corresponding JSF page.
	 */
	public AbstractFeatureDemo(FeatureDemoId id, String displayName, String pagePath) {
		this.id = id;
		this.displayName = displayName;
        this.pagePath = pagePath;
        title = getDisplayName();
	}

	public FeatureDemoId getId() {
		return id;
	}

    public String getTitle() {
		return title;
	}

	public String getDisplayName() {
		return displayName;
	}

    public String getDestination() {
        StringBuilder url = new StringBuilder();
        url.append("/feature-demo/");
        url.append(getId().toString());

        return url.toString();
    }

    public IFeatureDemoCategory getCategory() {
		return category;
	}

	public void setCategory(IFeatureDemoCategory category) {
		this.category = category;
	}

    public String getPagePath(){
        return pagePath;
    }

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof AbstractFeatureDemo) {
			return getId() == ((AbstractFeatureDemo)obj).getId();
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
