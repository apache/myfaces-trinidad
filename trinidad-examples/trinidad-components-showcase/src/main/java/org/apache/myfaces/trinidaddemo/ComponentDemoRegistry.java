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

import org.apache.myfaces.trinidaddemo.support.ComponentDemoCategoryId;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemo;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoCategory;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentDemoCategoryImpl;

import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Collection;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * This class represents the central point where component demos can be registered and obtained from.
 */
public class ComponentDemoRegistry {

    private static final Logger _LOG = Logger.getLogger(ComponentDemoRegistry.class.getName());

    private static ComponentDemoRegistry instance = new ComponentDemoRegistry();

    private List<IComponentDemoCategory> categories;

    private Map<ComponentDemoCategoryId, IComponentDemoCategory> categoriesRegistry;
    private Map<ComponentDemoId, IComponentDemo> componentsDemoRegistry;

    /**
     * Constructor.
     */
    private ComponentDemoRegistry() {
        categories = new ArrayList<IComponentDemoCategory>();

        categoriesRegistry = new HashMap<ComponentDemoCategoryId, IComponentDemoCategory>();
        componentsDemoRegistry = new HashMap<ComponentDemoId, IComponentDemo>();
    }

    /**
     * Gets the singleton instance of the component demos registry.
     *
     * @return a ComponentDemoRegistry.
     */
    public static ComponentDemoRegistry getInstance() {
        return instance;
    }

    /**
     * Registers the given component demo into a specific category.
     *
     * @param categoryId    the unique id of the category.
     * @param categoryName  the name of the category.
     * @param componentDemo the component demo to be registered.
     */
    public void registerComponentDemo(ComponentDemoCategoryId categoryId, String categoryName, IComponentDemo componentDemo) {
        if (componentDemo == null) {
            throw new IllegalArgumentException("Trying to register a null component demo!");
        }

        _LOG.log(Level.INFO, "Register component demo '" + componentDemo.getDisplayName() + "' in category '" + categoryName + "'");

        IComponentDemoCategory category = categoriesRegistry.get(categoryId);
        if (category == null) {
            category = new ComponentDemoCategoryImpl(categoryId, categoryName);

            categories.add(category);
            categoriesRegistry.put(categoryId, category);
        }

        category.addComponentDemo(componentDemo);
        componentsDemoRegistry.put(componentDemo.getId(), componentDemo);
    }

    /**
     * Returns the component demo identified by the given id.
     *
     * @param id the unique id of the desired component demo.
     * @return the component demo identified by the given id, or null if it's not available in this registry.
     */
    public IComponentDemo getComponentDemo(ComponentDemoId id) {
        return componentsDemoRegistry.get(id);
    }

    /**
     * Returns the category identified by the given id.
     *
     * @param id the unique id of the desired category.
     * @return the category identified by the given id, or null if it's not available in this registry.
     */
    public IComponentDemoCategory getComponentDemoCategory(ComponentDemoCategoryId id) {
        return categoriesRegistry.get(id);
    }

    /**
     * Returns the collection containing all available categories.
     *
     * @return the collection containing all available categories.
     */
    public Collection<IComponentDemoCategory> getDemoCategories() {
        return categories;
    }

    /**
     * Returns the collection containing all available component demos.
     *
     * @return the collection containing all available component demos.
     */
    public Collection<IComponentDemo> getComponentDemos() {
		return componentsDemoRegistry.values();
	}
}
