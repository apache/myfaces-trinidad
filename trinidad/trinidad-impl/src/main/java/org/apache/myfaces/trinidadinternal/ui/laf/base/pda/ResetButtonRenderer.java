/*
 * Copyright  2000-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidadinternal.ui.laf.base.pda;

import java.io.IOException;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.XhtmlLafConstants;


/**
 * Renderer for reset button nodes.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/ResetButtonRenderer.java#0 $) $Date: 10-nov-2005.18:55:04 $
 * @author The Oracle ADF Faces Team
 */
public class ResetButtonRenderer 
       extends org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.ResetButtonRenderer
{
  public void render(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    boolean disabled = Boolean.TRUE.equals(
                      node.getAttributeValue(context, DISABLED_ATTR));
    if (!disabled || supportsDisabledFormElements(context)  )
      super.render(context, node);
  }

  protected void prerender(
    UIXRenderingContext context,
    UINode           node
    )throws IOException
  {

    super.prerender(context, node);
    Object styleClass = XhtmlLafConstants.BUTTON_TEXT_STYLE_CLASS;

    if (supportsStyleAttributes(context))
    {
      renderStyleClassAttribute(context, styleClass);
    }
    
  }  

}
