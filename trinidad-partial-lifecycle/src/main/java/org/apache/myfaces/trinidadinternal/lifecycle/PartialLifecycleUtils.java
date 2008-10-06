package org.apache.myfaces.trinidadinternal.lifecycle;

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.share.xml.XMLUtils;

import javax.faces.context.FacesContext;
import java.util.Map;

public class PartialLifecycleUtils
{

  public static String[] getPartialTargets(FacesContext facesContext)
  {
    // TODO Is partialTarget a valid choice for the parameter name? 
    Map<String, String> parameterMap = facesContext.getExternalContext().getRequestParameterMap();
    String partialTargetsStr = parameterMap.get(XhtmlConstants.PARTIAL_TARGETS_PARAM);
    if (partialTargetsStr != null && partialTargetsStr.length() > 0)
    {
      return XMLUtils.parseNameTokens(partialTargetsStr);
    }
    return null;
  }
}
