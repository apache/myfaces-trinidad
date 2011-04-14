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
package org.apache.myfaces.trinidaddemo.components.navigation.commandNavigationItem;

import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;

/**
 *
 */
public class CommandNavigationItemDemo extends AbstractComponentDemo {

    private static final long serialVersionUID = -1982061956383498310L;

    private enum VARIANTS implements IComponentDemoVariantId {
		Default
	}

	/**
	 * Constructor.
	 */
	public CommandNavigationItemDemo() {
		super(ComponentDemoId.commandNavigationItem, "Command Navigation Item");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Default, "Default", this,
            new String[]{
                     "/components/navigation/commandNavigationItem/commandNavigationItem.xhtml"
            }));

        setDefaultVariant(VARIANTS.Default);
	}

    public String getSummaryResourcePath() {
        return "/components/navigation/commandNavigationItem/summary.xhtml";
    }

    public String getSkinDocumentationLink(){
        return null;
    }
}
