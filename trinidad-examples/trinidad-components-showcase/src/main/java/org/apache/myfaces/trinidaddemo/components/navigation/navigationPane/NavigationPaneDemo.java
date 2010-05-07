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
package org.apache.myfaces.trinidaddemo.components.navigation.navigationPane;

import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;

/**
 *
 */
public class NavigationPaneDemo extends AbstractComponentDemo {
    
    private static final long serialVersionUID = -1982060956383498310L;

    private enum VARIANTS implements IComponentDemoVariantId {
		Bar,
        Buttons,
        Choice,
        List,
        Tabs
	}

	/**
	 * Constructor.
	 */
	public NavigationPaneDemo() {
		super(ComponentDemoId.navigationPane, "Navigation Pane");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Bar, this,
                new String[]{
                       "/components/navigation/navigationPane/navigationPaneBar.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Buttons, this,
                new String[]{
                        "/components/navigation/navigationPane/navigationPaneButtons.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Choice, this,
                new String[]{
                        "/components/navigation/navigationPane/navigationPaneChoice.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.List, this,
                new String[]{
                        "/components/navigation/navigationPane/navigationPaneList.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Tabs, this,
                new String[]{
                        "/components/navigation/navigationPane/navigationPaneTabs.xhtml"
                }));

        setDefaultVariant(VARIANTS.Tabs);
	}

    public String getSummaryResourcePath() {
        return "/components/navigation/navigationPane/summary.xhtml";
    }
}
