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

import org.apache.myfaces.trinidaddemo.support.FeatureDemoCategoryId;
import org.apache.myfaces.trinidaddemo.feature.convertersAndValidators.clientSideConverters.ClientSideConvertersDemo;
import org.apache.myfaces.trinidaddemo.feature.convertersAndValidators.clientSideValidators.ClientSideValidatorsDemo;
import org.apache.myfaces.trinidaddemo.feature.convertersAndValidators.clientSideDateRestriction.ClientSideDateRestrictionDemo;
import org.apache.myfaces.trinidaddemo.feature.convertersAndValidators.serverSideConverters.ServerSideConvertersDemo;
import org.apache.myfaces.trinidaddemo.feature.convertersAndValidators.serverSideValidators.ServerSideValidatorsDemo;
import org.apache.myfaces.trinidaddemo.feature.convertersAndValidators.serverSideDateRestrictions.ServerSideDateRestrictionsDemo;
import org.apache.myfaces.trinidaddemo.feature.convertersAndValidators.messageCustomization.MessageCustomizationDemo;
import org.apache.myfaces.trinidaddemo.feature.changePersistence.addRemoveFacets.AddRemoveFacetsDemo;
import org.apache.myfaces.trinidaddemo.feature.changePersistence.addRemoveReorderChildren.AddRemoveReorderChildrenDemo;
import org.apache.myfaces.trinidaddemo.feature.changePersistence.explicitAttributeChange.ExplicitAttributeChangeDemo;
import org.apache.myfaces.trinidaddemo.feature.changePersistence.implicitAttributeChange.ImplicitAttributeChangeDemo;
import org.apache.myfaces.trinidaddemo.feature.table.addRow.AddRowDemo;
import org.apache.myfaces.trinidaddemo.feature.table.exportToCSV.ExportToCSVDemo;
import org.apache.myfaces.trinidaddemo.feature.table.totalRow.TotalRowDemo;
import org.apache.myfaces.trinidaddemo.feature.others.accessibilityProfile.AccessibilityProfileDemo;
import org.apache.myfaces.trinidaddemo.feature.others.dialogFramework.DialogFrameworkDemo;
import org.apache.myfaces.trinidaddemo.feature.others.fileDownload.FileDownloadDemo;
import org.apache.myfaces.trinidaddemo.feature.others.partialPageRendering.PartialPageRenderingDemo;
import org.apache.myfaces.trinidaddemo.feature.others.progressSteps.ProgressStepsDemo;
import org.apache.myfaces.trinidaddemo.feature.others.showDetailDisclosure.ShowDetailDisclosureDemo;

/**
 *
 */
public class FeatureDemoInitializer {

    private static FeatureDemoInitializer instance = new FeatureDemoInitializer();

    private static final String CATEGORY_CONVERTERS_AND_VALIDATORS_TITLE   = "Converters and Validators";
    private static final String CHANGE_PERSISTENCE_TITLE  = "Change persistence";
    private static final String TABLE_TITLE  = "Table";
    private static final String OTHERS_TITLE   = "Others";


    /**
     * Constructor.
     */
    private FeatureDemoInitializer(){}

    /**
     * Gets the singleton instance of the feature demos initializer.
     *
     * @return a ComponentDemoInitializer.
     */
    public static FeatureDemoInitializer getInstance() {
        return instance;
    }

	public void init() {
		FeatureDemoRegistry registry = FeatureDemoRegistry.getInstance();
        registerFeatureDemos(registry);
	}

    /**
     * @param registry
     */
    public void registerFeatureDemos(FeatureDemoRegistry registry) {
        //  registering Layout Converters And Validators components
		registry.registerFeatureDemo(FeatureDemoCategoryId.convertersAndValidators, CATEGORY_CONVERTERS_AND_VALIDATORS_TITLE, new ClientSideConvertersDemo());
		registry.registerFeatureDemo(FeatureDemoCategoryId.convertersAndValidators, CATEGORY_CONVERTERS_AND_VALIDATORS_TITLE, new ClientSideValidatorsDemo());
		registry.registerFeatureDemo(FeatureDemoCategoryId.convertersAndValidators, CATEGORY_CONVERTERS_AND_VALIDATORS_TITLE, new ClientSideDateRestrictionDemo());
		registry.registerFeatureDemo(FeatureDemoCategoryId.convertersAndValidators, CATEGORY_CONVERTERS_AND_VALIDATORS_TITLE, new ServerSideConvertersDemo());
		registry.registerFeatureDemo(FeatureDemoCategoryId.convertersAndValidators, CATEGORY_CONVERTERS_AND_VALIDATORS_TITLE, new ServerSideValidatorsDemo());
		registry.registerFeatureDemo(FeatureDemoCategoryId.convertersAndValidators, CATEGORY_CONVERTERS_AND_VALIDATORS_TITLE, new ServerSideDateRestrictionsDemo());
 		registry.registerFeatureDemo(FeatureDemoCategoryId.convertersAndValidators, CATEGORY_CONVERTERS_AND_VALIDATORS_TITLE, new MessageCustomizationDemo());             

        registry.registerFeatureDemo(FeatureDemoCategoryId.changePersistence, CHANGE_PERSISTENCE_TITLE, new AddRemoveFacetsDemo());
        registry.registerFeatureDemo(FeatureDemoCategoryId.changePersistence, CHANGE_PERSISTENCE_TITLE, new AddRemoveReorderChildrenDemo());
        registry.registerFeatureDemo(FeatureDemoCategoryId.changePersistence, CHANGE_PERSISTENCE_TITLE, new ExplicitAttributeChangeDemo());
        registry.registerFeatureDemo(FeatureDemoCategoryId.changePersistence, CHANGE_PERSISTENCE_TITLE, new ImplicitAttributeChangeDemo());
        
        registry.registerFeatureDemo(FeatureDemoCategoryId.table, TABLE_TITLE, new AddRowDemo());
        registry.registerFeatureDemo(FeatureDemoCategoryId.table, TABLE_TITLE, new ExportToCSVDemo());
        registry.registerFeatureDemo(FeatureDemoCategoryId.table, TABLE_TITLE, new TotalRowDemo());

        registry.registerFeatureDemo(FeatureDemoCategoryId.others, OTHERS_TITLE, new AccessibilityProfileDemo());
        registry.registerFeatureDemo(FeatureDemoCategoryId.others, OTHERS_TITLE, new DialogFrameworkDemo());
        registry.registerFeatureDemo(FeatureDemoCategoryId.others, OTHERS_TITLE, new FileDownloadDemo());
        registry.registerFeatureDemo(FeatureDemoCategoryId.others, OTHERS_TITLE, new PartialPageRenderingDemo());
        registry.registerFeatureDemo(FeatureDemoCategoryId.others, OTHERS_TITLE, new ProgressStepsDemo());
        registry.registerFeatureDemo(FeatureDemoCategoryId.others, OTHERS_TITLE, new ShowDetailDisclosureDemo());        
    }
}
