/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import org.apache.myfaces.trinidadinternal.share.url.URLEncoder;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.base.desktop.SubTabBarUtils;

/**
 * Renderer for ShowItem
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ShowItemRenderer.java#0 $) $Date: 10-nov-2005.18:54:13 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ShowItemRenderer extends LinkRenderer 
{
  @Override
  protected void renderIndexedChild(
    UIXRenderingContext context,
    UINode           node,
    int              currVisChildIndex,
    int              prevVisChildIndex,
    int              nextVisChildIndex,
    int              ithRenderedChild
    ) throws IOException
  {
    //  Do nothing here, my parent will take care of rendering my children
    //  if I have my 'selected' attribute set to true.
  }
  
  // Returns the partial change script that is usually rendered for onClick
  @Override
  protected String getPartialChangeScript(
    UIXRenderingContext context,
    UINode           node
    )
  {
    String partialTargets = getAncestorPartialTargets(context);
    
    if (partialTargets == null)
      return null;

    URLEncoder encoder = context.getURLEncoder();
    String partialTargetsKey = encoder.encodeParameter(PARTIAL_TARGETS_PARAM);
    String eventParamKey = encoder.encodeParameter(EVENT_PARAM);
    String sourceParamKey = encoder.encodeParameter(SOURCE_PARAM);
    String sourceParam = BaseLafUtils.getStringAttributeValue(
      context, node, ID_ATTR);
    
    String formName = getParentFormName(context);

    char validate = Boolean.TRUE.equals(SubTabBarUtils.isUnvalidated(context)) ? 
                      '0' : '1';
    
    String partialChangeScript = 
      "_submitPartialChange('" + formName + "'," + validate + ",{"+
      partialTargetsKey + ":'" + partialTargets + "'," +
      eventParamKey + ":'" + SHOW_EVENT + "'," +
      sourceParamKey + ":'" + sourceParam + "'});return false";
    
    String partialKey = context.getURLEncoder().encodeParameter(PARTIAL_PARAM);
    FormValueRenderer.addNeededValue(context, formName, eventParamKey,
                                     partialTargetsKey, sourceParamKey, partialKey);

    return partialChangeScript;
  }
}
