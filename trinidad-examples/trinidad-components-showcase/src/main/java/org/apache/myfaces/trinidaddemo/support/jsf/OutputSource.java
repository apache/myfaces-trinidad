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
package org.apache.myfaces.trinidaddemo.support.jsf;

import org.apache.myfaces.trinidad.component.core.output.CoreOutputFormatted;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.util.ComponentUtils;

/**
 *
 */
public class OutputSource extends CoreOutputFormatted {

    public static final String COMPONENT_TYPE = "org.apache.myfaces.trinidad.OutputSource";
    public static final String RENDERER_TYPE = "org.apache.myfaces.trinidad.OutputSource";

    static public final FacesBean.Type OUTPUT_SOURCE_TYPE = new FacesBean.Type(CoreOutputFormatted.TYPE);
    
    static public PropertyKey PATH_PREFIX_KEY = OUTPUT_SOURCE_TYPE.registerKey("pathPrefix", String.class);

    public OutputSource() {
        super(RENDERER_TYPE);
    }

    public String getPathPrefix() {
        return ComponentUtils.resolveString(getProperty(PATH_PREFIX_KEY));
    }

    public void setPathPrefix(String pathPrefix) {
        setProperty(PATH_PREFIX_KEY, pathPrefix);
    }
}
