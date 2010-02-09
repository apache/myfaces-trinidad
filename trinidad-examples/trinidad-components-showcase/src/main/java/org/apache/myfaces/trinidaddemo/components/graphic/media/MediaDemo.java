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
package org.apache.myfaces.trinidaddemo.components.graphic.media;

import org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentDemo;
import org.apache.myfaces.trinidaddemo.support.impl.ComponentVariantDemoImpl;
import org.apache.myfaces.trinidaddemo.support.ComponentDemoId;
import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;

/**
 *
 */
public class MediaDemo extends AbstractComponentDemo {
    
    private static final long serialVersionUID = -1982063956893498710L;

    private enum VARIANTS implements IComponentDemoVariantId {
		AllControls,
        Link,
        NoControls,
        Quicktime,
        Real,
        Windows
	}

	/**
	 * Constructor.
	 */
	public MediaDemo() {
		super(ComponentDemoId.media, "Media",
            new String[]{
                "/components/graphic/media/media.xhtml"
            });
        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.AllControls, "All controls", this,
                new String[]{
                        "/components/graphic/media/mediaAllControls.xhtml"
                }, getSummaryResourcePath()));

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Link, this,
                new String[]{
                        "/components/graphic/media/mediaLink.xhtml"
                }, getSummaryResourcePath()));

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.NoControls, "No controls", this,
                new String[]{
                        "/components/graphic/media/mediaNoControls.xhtml"
                }, getSummaryResourcePath()));

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Quicktime, this,
                new String[]{
                        "/components/graphic/media/mediaQuicktime.xhtml"
                }, getSummaryResourcePath()));

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Real, this,
                new String[]{
                        "/components/graphic/media/mediaReal.xhtml"
                }, getSummaryResourcePath()));

        addComponentDemoVariant(new ComponentVariantDemoImpl(VARIANTS.Windows, this,
                new String[]{
                        "/components/graphic/media/mediaWindows.xhtml"
                }, getSummaryResourcePath()));
	}

    public String getSummaryResourcePath() {
        return "/components/graphic/media/summary.xhtml";
    }
}
