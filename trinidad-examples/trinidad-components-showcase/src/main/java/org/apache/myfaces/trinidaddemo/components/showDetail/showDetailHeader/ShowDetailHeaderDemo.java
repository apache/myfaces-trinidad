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
package org.apache.myfaces.trinidaddemo.components.showDetail.showDetailHeader;

import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;

/**
 *
 */
public class ShowDetailHeaderDemo extends AbstractComponentDemo {

    private static final long serialVersionUID = -1989461456014395510L;

    private enum VARIANTS implements IComponentDemoVariantId {
        NoneMessage,
		ConfirmationMessage,
        InfoMessage,
        ErrorMessage,
        WarningMessage
	}

	/**
	 * Constructor.
	 */
	public ShowDetailHeaderDemo(){
		super(ComponentDemoId.showDetailHeader, "Show Detail Header");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.NoneMessage, "None Message", this,
                new String[]{
                        "/components/showDetail/showDetailHeader/showDetailHeaderNoneMessage.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.InfoMessage, "Info Message", this,
                new String[]{
                        "/components/showDetail/showDetailHeader/showDetailHeaderInfoMessage.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.ConfirmationMessage, "Confirmation Message", this,
                new String[]{
                        "/components/showDetail/showDetailHeader/showDetailHeaderConfirmationMessage.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.ErrorMessage, "Error message", this,
                new String[]{
                        "/components/showDetail/showDetailHeader/showDetailHeaderErrorMessage.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.WarningMessage, "Warning message", this,
                new String[]{
                        "/components/showDetail/showDetailHeader/showDetailHeaderWarningMessage.xhtml"
                }));

        setDefaultVariant(VARIANTS.NoneMessage);
	}

    public String getSummaryResourcePath() {
        return "/components/showDetail/showDetailHeader/summary.xhtml";
    }
}
