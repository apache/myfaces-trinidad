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

import org.apache.myfaces.trinidaddemo.support.impl.FeatureDemoCategoryImpl;
import org.apache.myfaces.trinidaddemo.support.IFeatureDemoCategory;
import org.apache.myfaces.trinidaddemo.support.FeatureDemoCategoryId;
import org.apache.myfaces.trinidaddemo.support.FeatureDemoId;
import org.apache.myfaces.trinidaddemo.support.IFeatureDemo;

import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Collection;

/**
 *
 */
public class FeatureDemoRegistry {

    private static final Logger _LOG = Logger.getLogger(FeatureDemoRegistry.class.getName());

    private static FeatureDemoRegistry instance = new FeatureDemoRegistry();

    private List<IFeatureDemoCategory> categories;

    private Map<FeatureDemoCategoryId, IFeatureDemoCategory> categoriesRegistry;
    private Map<FeatureDemoId, IFeatureDemo> featuresDemoRegistry;

    /**
     * Constructor.
     */
    private FeatureDemoRegistry() {
        categories = new ArrayList<IFeatureDemoCategory>();

        categoriesRegistry = new HashMap<FeatureDemoCategoryId, IFeatureDemoCategory>();
        featuresDemoRegistry = new HashMap<FeatureDemoId, IFeatureDemo>();
    }

    /**
     * Gets the singleton instance of the feature demos registry.
     *
     * @return a FeatureDemoRegistry.
     */
    public static FeatureDemoRegistry getInstance() {
        return instance;
    }

    /**
     * Registers the given feature demo into a specific category.
     *
     * @param categoryId    the unique id of the category.
     * @param categoryName  the name of the category.
     * @param featureDemo   the feature demo to be registered.
     */
    public void registerFeatureDemo(FeatureDemoCategoryId categoryId, String categoryName, IFeatureDemo featureDemo) {
        if (featureDemo == null) {
            throw new IllegalArgumentException("Trying to register a null feature demo!");
        }

        _LOG.log(Level.INFO, "Register feature demo '" + featureDemo.getDisplayName() + "' in category '" + categoryName + "'");

        IFeatureDemoCategory category = categoriesRegistry.get(categoryId);
        if (category == null) {
            category = new FeatureDemoCategoryImpl(categoryId, categoryName);

            categories.add(category);
            categoriesRegistry.put(categoryId, category);
        }

        category.addFeatureDemo(featureDemo);
        featuresDemoRegistry.put(featureDemo.getId(), featureDemo);
    }

    /**
     * Returns the feature demo identified by the given id.
     *
     * @param id the unique id of the desired feature demo.
     * @return the feature demo identified by the given id, or null if it's not available in this registry.
     */
    public IFeatureDemo getFeatureDemo(FeatureDemoId id) {
        return featuresDemoRegistry.get(id);
    }

    /**
     * Returns the category identified by the given id.
     *
     * @param id the unique id of the desired category.
     * @return the category identified by the given id, or null if it's not available in this registry.
     */
    public IFeatureDemoCategory getFeatureDemoCategory(FeatureDemoCategoryId id) {
        return categoriesRegistry.get(id);
    }

    /**
     * Returns the collection containing all available categories.
     *
     * @return the collection containing all available categories.
     */
    public Collection<IFeatureDemoCategory> getDemoCategories() {
        return categories;
    }

    /**
     * Returns the collection containing all available feature demos.
     *
     * @return the collection containing all available feature demos.
     */
    public Collection<IFeatureDemo> getFeatureDemos() {
		return featuresDemoRegistry.values();
	}
}
