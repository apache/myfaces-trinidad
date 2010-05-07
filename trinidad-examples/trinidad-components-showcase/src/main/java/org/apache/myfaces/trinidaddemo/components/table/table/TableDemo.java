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
package org.apache.myfaces.trinidaddemo.components.table.table;

import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;

/**
 *
 */
public class TableDemo extends AbstractComponentDemo {

    private static final long serialVersionUID = -1982064950883398710L;

    private enum VARIANTS implements IComponentDemoVariantId {
        GridLines,
		NoGridLines,
        SingleRowSelection,
        MultipleRowSelection,
        ShowHide,
        Pagination
	}

	/**
	 * Constructor.
	 */
	public TableDemo() {
		super(ComponentDemoId.table, "Table");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.GridLines, "Grid lines", this,
                new String[]{
                        "/components/table/table/tableGridLines.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.NoGridLines, "No grid lines", this,
                new String[]{
                        "/components/table/table/tableNoGridLines.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.SingleRowSelection, "Single Row Selection", this,
                new String[]{
                        "/components/table/table/tableSingleRowSelection.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.MultipleRowSelection, "Multiple Row Selection", this,
                new String[]{
                        "/components/table/table/tableMultipleRowSelection.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.ShowHide, "Show / Hide", this,
                new String[]{
                        "/components/table/table/tableShowHide.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Pagination, this,
                new String[]{
                        "/components/table/table/tablePaginated.xhtml"
                }));

        setDefaultVariant(VARIANTS.GridLines);
	}

    public String getSummaryResourcePath() {
        return "/components/table/table/summary.xhtml";
    }

    public String getBackingBeanResourcePath() {
		return "/org/apache/myfaces/trinidaddemo/components/table/column/TableColumnBean.java";
	}

}
