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
package org.apache.myfaces.trinidaddemo.components.layout.panelGroupLayout;

import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;
import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;

/**
 *
 */
public class GroupLayoutDemo extends AbstractComponentDemo {

	private static final long serialVersionUID = 2660360384187454374L;

    private enum VARIANTS implements IComponentDemoVariantId {		
		Horizontal,
		Vertical,
		Dynamic
	}

	/**
	 * Constructor.
	 */
	public GroupLayoutDemo() {
		super(ComponentDemoId.panelGroupLayout, "Group Layout");

		addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Horizontal, this,
                new String[]{
                        "/components/layout/groupLayout/panelGroupLayoutHorizontal.xhtml"
                }));
		addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Vertical, this,
                new String[]{
                        "/components/layout/groupLayout/panelGroupLayoutVertical.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Dynamic,this,
                new String[]{
                        "/components/layout/groupLayout/panelGroupLayoutDynamic.xhtml"
                }, getSummaryResourcePath(),
                "/org/apache/myfaces/trinidaddemo/components/layout/panelGroupLayout/GroupLayoutDynamicBean.java"));

        setDefaultVariant(VARIANTS.Vertical);
	}

    public String getSummaryResourcePath() {
        return "/components/layout/groupLayout/summary.xhtml";
    }
}
