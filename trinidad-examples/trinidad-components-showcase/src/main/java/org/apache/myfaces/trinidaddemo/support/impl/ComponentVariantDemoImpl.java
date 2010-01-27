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
package org.apache.myfaces.trinidaddemo.support.impl;

import org.apache.myfaces.trinidaddemo.support.IComponentDemoVariantId;

/**
 *  Default implementation of the {@link org.apache.myfaces.trinidaddemo.support.impl.AbstractComponentVariantDemo}
 */
public class ComponentVariantDemoImpl extends AbstractComponentVariantDemo {

    private String jsfResourcePath;
    private String summaryResourcePath;
    private String backingBeanResourcePath;

    /**
     * Constructor.
     *
     * @param variantId
     * @param componentDemo
     * @param jsfResourcePath
     * @param summaryResourcePath
     */
    public ComponentVariantDemoImpl(IComponentDemoVariantId variantId,
            AbstractComponentDemo componentDemo,
            String jsfResourcePath, String summaryResourcePath) {
        this(variantId, variantId.toString(), componentDemo, jsfResourcePath, summaryResourcePath);
    }

    /**
     * Constructor.
     *
     * @param variantId
     * @param componentDemo
     * @param jsfResourcePath
     * @param summaryResourcePath
     * @param backingBeanResourcePath
     */
    public ComponentVariantDemoImpl(IComponentDemoVariantId variantId,
            AbstractComponentDemo componentDemo,
            String jsfResourcePath, String summaryResourcePath, String backingBeanResourcePath) {
        this(variantId, variantId.toString(), componentDemo, jsfResourcePath, summaryResourcePath, backingBeanResourcePath);
    }

    /**
     * Constructor.
     *
     * @param variantId
     * @param variantDisplayName
     * @param componentDemo
     * @param jsfResourcePath
     * @param summaryResourcePath
     */
    public ComponentVariantDemoImpl(IComponentDemoVariantId variantId, String variantDisplayName,
            AbstractComponentDemo componentDemo,
            String jsfResourcePath, String summaryResourcePath) {
        this(variantId, variantDisplayName, componentDemo, jsfResourcePath, summaryResourcePath, null);
    }   

    /**
     * Constructor,
     *
     * @param variantId
     * @param variantDisplayName
     * @param componentDemo
     * @param jsfResourcePath
     * @param summaryResourcePath
     * @param backingBeanResourcePath
     */
    public ComponentVariantDemoImpl(IComponentDemoVariantId variantId, String variantDisplayName,
            AbstractComponentDemo componentDemo,
            String jsfResourcePath, String summaryResourcePath, String backingBeanResourcePath) {

        super(variantId, variantDisplayName, componentDemo);

        this.jsfResourcePath = jsfResourcePath;
        this.summaryResourcePath = summaryResourcePath;
        this.backingBeanResourcePath = backingBeanResourcePath;
    }

    public String getJsfResourcePath() {
        return jsfResourcePath;
    }

    public String getSummaryResourcePath() {
        return summaryResourcePath;
    }

    @Override
    public String getBackingBeanResourcePath() {
        return backingBeanResourcePath;
    }
}
