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
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkUtils;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;

/**
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class LinkRenderer extends org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.LinkRenderer
{
  @Override
  protected Object getStyleClass(
    UIXRenderingContext context,
    UINode           node)
  {
    Object styleClass = node.getAttributeValue(context, STYLE_CLASS_ATTR);
    if (styleClass != null)
      return styleClass;

    if (!LinkUtils.isDefaultStyleClassDisabled(context))
    {
      if (isDisabled(context, node))
        styleClass = LINK_DISABLED_STYLE_CLASS;
      else
      {
        boolean isSelected =  Boolean.TRUE.equals(
           node.getAttributeValue(context, SELECTED_ATTR));
        if (isSelected)
          styleClass = LINK_SELECTED_STYLE_CLASS;
        else
          styleClass = LINK_STYLE_CLASS;
      }
    }

    return styleClass;
  }
}
