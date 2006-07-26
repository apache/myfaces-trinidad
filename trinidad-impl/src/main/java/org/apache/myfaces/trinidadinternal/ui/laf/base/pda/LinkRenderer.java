/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfinternal.ui.laf.base.pda;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.LinkUtils;
import org.apache.myfaces.adfinternal.ui.UINode;
import org.apache.myfaces.adfinternal.ui.RenderingContext;

/**
 */
public class LinkRenderer extends org.apache.myfaces.adfinternal.ui.laf.base.xhtml.LinkRenderer
{
  protected Object getStyleClass(
    RenderingContext context,
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
