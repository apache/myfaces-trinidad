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
package org.apache.myfaces.trinidaddemo.components.panel.panelBox;

import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;
import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;

/**
 *
 */
public class PanelBoxDemo extends AbstractComponentDemo {

    private static final long serialVersionUID = -4995780647798809893L;

    private enum VARIANTS implements IComponentDemoVariantId {
        Dark,
        Light,
        Medium,
        Transparent
    }
    /**
     * Constructor.
     */
    public PanelBoxDemo() {
        super(ComponentDemoId.panelBox, "Panel Box");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Dark, this,
                new String[]{
                        "/components/panel/panelBox/panelBoxDark.xhtml"
                }));

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Light, this,
                new String[]{
                        "/components/panel/panelBox/panelBoxLight.xhtml"
                }));

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Medium, this,
                new String[]{
                        "/components/panel/panelBox/panelBoxMedium.xhtml"
                }));

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Transparent, this,
                new String[]{
                        "/components/panel/panelBox/panelBoxTransparent.xhtml"
                }));

        setDefaultVariant(VARIANTS.Light);
    }

    public String getSummaryResourcePath() {
        return "/components/panel/panelBox/summary.xhtml";
    }
}
