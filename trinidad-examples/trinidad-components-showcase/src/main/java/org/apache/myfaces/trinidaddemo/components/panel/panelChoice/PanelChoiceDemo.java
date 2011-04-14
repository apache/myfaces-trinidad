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
package org.apache.myfaces.trinidaddemo.components.panel.panelChoice;

import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;
import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;

/**
 *
 */
public class PanelChoiceDemo extends AbstractComponentDemo {

    private static final long serialVersionUID = -1982061956882438710L;

    private enum VARIANTS implements IComponentDemoVariantId {
		Buttom,
        Center,
        Start,
        Top
	}

	/**
	 * Constructor.
	 */
	public PanelChoiceDemo() {
		super(ComponentDemoId.panelChoice, "Panel Choice");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Buttom, this,
                new String[]{
                        "/components/panel/panelChoice/panelChoiceButtom.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Center, this,
                new String[]{
                        "/components/panel/panelChoice/panelChoiceCenter.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Start, this,
                new String[]{
                        "/components/panel/panelChoice/panelChoiceStart.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Top, this,
                new String[]{
                        "/components/panel/panelChoice/panelChoiceTop.xhtml"
                }));

        setDefaultVariant(VARIANTS.Center);
	}

    public String getSummaryResourcePath() {
        return "/components/panel/panelChoice/summary.xhtml";
    }

    public String getSkinDocumentationLink(){
        return null;
    }
}
