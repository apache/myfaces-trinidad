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
package org.apache.myfaces.trinidaddemo.components.table.treeTable;

import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;

/**
 *
 */
public class TreeTableDemo extends AbstractComponentDemo {

    private static final long serialVersionUID = -1982064956883698710L;

    private enum VARIANTS implements IComponentDemoVariantId {
		NoGridLines,
        SingleRowSelection,
        MultipleRowSelection,
        Simple,
        Detailed,
        PathStamp
	}

	/**
	 * Constructor.
	 */
	public TreeTableDemo() {
		super(ComponentDemoId.treeTable, "Tree Table");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Detailed, this,
                new String[]{
                        "/components/table/treeTable/treeTableDetailed.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Simple, this,
                new String[]{
                        "/components/table/treeTable/treeTableSimple.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.NoGridLines, "No grid lines", this,
                new String[]{
                        "/components/table/treeTable/treeTableNoGridLines.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.SingleRowSelection, "Single selection", this,
                new String[]{
                        "/components/table/treeTable/treeTableSingleRowSelection.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.MultipleRowSelection, "Multiple selection", this,
                new String[]{
                        "/components/table/treeTable/treeTableMultipleRowSelection.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.PathStamp, "Path Stamp", this,
                new String[]{
                        "/components/table/treeTable/treeTablePathStamp.xhtml"
                }));

        setDefaultVariant(VARIANTS.Detailed);
	}

    public String getSummaryResourcePath() {
        return "/components/table/treeTable/summary.xhtml";
    }

    public String getBackingBeanResourcePath() {
		return "/org/apache/myfaces/trinidaddemo/components/table/treeTable/TreeTableBean.java";
	}
}
