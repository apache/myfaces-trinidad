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
package org.apache.myfaces.trinidaddemo.components.panel.panelHeader;

import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;
import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;

/**
 *
 */
public class PanelHeaderDemo extends AbstractComponentDemo {

    private static final long serialVersionUID = -1982061956882338710L;

    private enum VARIANTS implements IComponentDemoVariantId {
		ConfirmationMessage,
        ErrorMessage,
        None,
        WarningMessage,
        InfoMessage
	}

	/**
	 * Constructor.
	 */
	public PanelHeaderDemo() {
		super(ComponentDemoId.panelHeader, "Panel Header");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.ConfirmationMessage, this,
                new String[]{
                        "/components/panel/panelHeader/panelHeaderConfirmationMessage.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.ErrorMessage, this,
                new String[]{
                        "/components/panel/panelHeader/panelHeaderErrorMessage.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.InfoMessage, this,
                new String[]{
                        "/components/panel/panelHeader/panelHeaderInfoMessage.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.None, this,
                new String[]{
                        "/components/panel/panelHeader/panelHeaderNone.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.WarningMessage, this,
                new String[]{
                        "/components/panel/panelHeader/panelHeaderWarningMessage.xhtml"
                }));

        setDefaultVariant(VARIANTS.None);
	}

    public String getSummaryResourcePath() {
        return "/components/panel/panelHeader/summary.xhtml";
    }
}
