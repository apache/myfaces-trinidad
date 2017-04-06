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
package org.apache.myfaces.trinidaddemo.components.output.chart;

import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;
import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;

/**
 *
 */
public class ChartDemo extends AbstractComponentDemo {

    private static final long serialVersionUID = -1982371956886498710L;

    private enum VARIANTS implements IComponentDemoVariantId {
        Area,
        HorizontalBar,
        VerticalBar,
        Pie,
        LegendButtom,
        LegendTop
    }

    /**
     * Constructor.
     */
    public ChartDemo() {
        super(ComponentDemoId.chart, "Chart");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.VerticalBar, this,
                new String[]{
                        "/components/output/chart/chartVerticalBar.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Area, this,
                new String[]{
                        "/components/output/chart/chartArea.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.HorizontalBar, this,
                new String[]{
                        "/components/output/chart/chartHorizontalBar.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.LegendButtom, this,
                new String[]{
                        "/components/output/chart/chartLegendButtom.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.LegendTop, this,
                new String[]{
                        "/components/output/chart/chartLegendTop.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Pie, this,
                new String[]{
                        "/components/output/chart/chartPie.xhtml"
                }));

        setDefaultVariant(VARIANTS.VerticalBar);
    }

    public String getSummaryResourcePath() {
        return "/components/output/chart/summary.xhtml";
    }

    public String getBackingBeanResourcePath() {
        return "/org/apache/myfaces/trinidaddemo/components/output/chart/ChartBean.java";
    }

    public String getSkinDocumentationLink(){
        return null;
    }
}
