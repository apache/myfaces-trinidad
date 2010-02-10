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
import java.util.List;

import org.apache.myfaces.trinidaddemo.support.ComponentDemoCategoryId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemo;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoCategory;

/**
 * Default implementation of the {@link IComponentDemoCategory} interface.
 */
public class ComponentDemoCategoryImpl implements IComponentDemoCategory {

	private static final long serialVersionUID = -7340858926729979482L;

	private ComponentDemoCategoryId id;
	private String name;

	private List<IComponentDemo> componentDemos;
	
	/**
	 * Constructor. 
	 * 
	 * @param id the unique id of this category.
	 * @param name the name of this category.
	 */
	public ComponentDemoCategoryImpl(ComponentDemoCategoryId id, String name) {
		this.id = id;
		this.name = name;

		componentDemos = new ArrayList<IComponentDemo>();
	}
	
	public void addComponentDemo(IComponentDemo componentDemo) {
		componentDemo.setCategory(this);
		componentDemos.add(componentDemo);
	}

	public List<IComponentDemo> getComponentDemos() {
		return componentDemos;
	}

	public ComponentDemoCategoryId getId() {
		return id;
	}

	public String getName() {
		return name;
	}
  
    @Override
	public boolean equals(Object obj) {
		if (obj instanceof ComponentDemoCategoryImpl) {
			return getId() == ((ComponentDemoCategoryImpl)obj).getId();
		}
		
		return false;
	}

	@Override
	public int hashCode() {
		return getId() != null ? getId().hashCode() : super.hashCode();
	}

	@Override
	public String toString() {
		return getName();
	}
}
