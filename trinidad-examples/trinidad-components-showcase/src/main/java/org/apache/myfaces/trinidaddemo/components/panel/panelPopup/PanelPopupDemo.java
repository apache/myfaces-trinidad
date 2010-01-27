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
package org.apache.myfaces.trinidaddemo.components.panel.panelPopup;

import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;
import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;

/**
 *
 */
public class PanelPopupDemo extends AbstractComponentDemo {

    private static final long serialVersionUID = -1982064956382498710L;

    private enum VARIANTS implements IComponentDemoVariantId {
		Hover,
        Centered,
        Relatve,
        Click
	}

	/**
	 * Constructor.
	 */
	public PanelPopupDemo() {
		super(ComponentDemoId.panelPopup, "Panel Popup");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Centered, this,
                "/components/panel/panelPopup/panelPopupCentered.xhtml", getSummaryResourcePath()));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Click, this,
                "/components/panel/panelPopup/panelPopupClick.xhtml", getSummaryResourcePath()));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Hover, this,
                "/components/panel/panelPopup/panelPopupHover.xhtml", getSummaryResourcePath()));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Relatve, this,
                "/components/panel/panelPopup/panelPopupRelative.xhtml", getSummaryResourcePath()));
	}

	public String getJsfResourcePath() {
		return "/components/panel/panelPopup/panelPopup.xhtml";
	}

    public String getSummaryResourcePath() {
        return "/components/panel/panelPopup/summary.xhtml";
    }
}
