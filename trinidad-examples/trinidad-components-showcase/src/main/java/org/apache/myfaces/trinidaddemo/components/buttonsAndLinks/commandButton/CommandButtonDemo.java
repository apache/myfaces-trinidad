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
package org.apache.myfaces.trinidaddemo.components.buttonsAndLinks.commandButton;

import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;

/**
 *
 */
public class CommandButtonDemo extends AbstractComponentDemo {

    private static final long serialVersionUID = -1982061956883408710L;

    private enum VARIANTS implements IComponentDemoVariantId {
		fullSubmit,
		ajaxSubmit
	}

	/**
	 * Constructor.
	 */
	public CommandButtonDemo() {
		super(ComponentDemoId.commandButton , "Command Button");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.fullSubmit, "Full submit", this,
            new String[]{
                "/components/buttonsAndLinks/commandButton/commandButton.xhtml"
            }));

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.ajaxSubmit, "Ajax submit", this,
            new String[]{
                "/components/buttonsAndLinks/commandButton/commandButtonAjax.xhtml"
            }));

       setDefaultVariant(VARIANTS.fullSubmit);
	}

    public String getSummaryResourcePath() {
        return "/components/buttonsAndLinks/commandButton/summary.xhtml";
    }

    public String getBackingBeanResourcePath() {
		return "/org/apache/myfaces/trinidaddemo/components/buttonsAndLinks/commandButton/CommandButtonBean.java";
	}
}
