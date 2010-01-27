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
package org.apache.myfaces.trinidaddemo.components.layout.panelHorizontalLayout;

import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;
import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;

/**
 *
 */
public class HorizontalLayoutDemo extends AbstractComponentDemo {

	private static final long serialVersionUID = -1982071956823498710L;

    private enum VARIANTS implements IComponentDemoVariantId {
		Left,
		Middle,
        Right,
        Top
	}

	/**
	 * Constructor.
	 */
	public HorizontalLayoutDemo() {
		super(ComponentDemoId.panelHorizontalLayout, "Horizontal Layout");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Left, this,
                "/components/layout/horizontalLayout/panelHorizontalLayoutLeft.xhtml", getSummaryResourcePath()));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Middle, this,
                "/components/layout/horizontalLayout/panelHorizontalLayoutMiddle.xhtml", getSummaryResourcePath()));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Right, this,
                "/components/layout/horizontalLayout/panelHorizontalLayoutRight.xhtml", getSummaryResourcePath()));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Top, this,
                "/components/layout/horizontalLayout/panelHorizontalLayoutTop.xhtml", getSummaryResourcePath()));
	}

	public String getJsfResourcePath() {
		return "/components/layout/horizontalLayout/panelHorizontalLayout.xhtml";
	}

    public String getSummaryResourcePath() {
        return "/components/layout/horizontalLayout/summary.xhtml";
    }
}
