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
package org.apache.myfaces.trinidaddemo.components.output.message;

import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;

/**
 *
 */
public class MessageDemo extends AbstractComponentDemo {

    private static final long serialVersionUID = -1982371956886499710L;

    private enum VARIANTS implements IComponentDemoVariantId {
		None,
        Warning,
        Info,
        Error,
        Help
	}

	/**
	 * Constructor.
	 */
	public MessageDemo() {
		super(ComponentDemoId.message, "Message");

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.None, this,
                new String[]{
                        "/components/output/message/messageNone.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Warning, this,
                new String[]{
                        "/components/output/message/messageWarning.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Info, this,
                new String[]{
                        "/components/output/message/messageInfo.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Error, this,
                new String[]{
                        "/components/output/message/messageError.xhtml"
                }));
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Help, this,
                new String[]{
                        "/components/output/message/messageHelp.xhtml"
                }));

        setDefaultVariant(VARIANTS.None);
	}

    public String getSummaryResourcePath() {
        return "/components/output/message/summary.xhtml";
    }

    public String getSkinDocumentationLink(){
        return null;
    }
}
