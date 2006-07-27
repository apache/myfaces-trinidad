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
package org.apache.myfaces.trinidadinternal.ui.laf.base.desktop;

import java.io.IOException;

import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * Renderer for form button submit nodes.
 * 
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/oracle/desktop/SubmitButtonRenderer.java#0 $) $Date: 10-nov-2005.18:51:55 $
 * @author The Oracle ADF Faces Team
 */
abstract public class SubmitButtonRenderer extends ResetButtonRenderer
{
  /**
   * handle case where we don;t have javascript. This is a copy of
   * org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.SubmitButtonRenderer.renderAttributes()
   */
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);
    
    // if we don't support scripting, we have to generate a special
    // compound name for the submit button
    if (!supportsScripting(context))
    {
      String compoundName = BaseDesktopUtils.createCompoundName(
                              context,
                              getParentFormName(context),

      org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.SubmitButtonRenderer.getNameValues(
                                  context,
                                  node));
                              
      context.getResponseWriter().writeAttribute("name", compoundName, null);
    }
  }

  /**
   * Returns the Javascript function call to return for this button.
   */
  protected String getFunctionCall(
    UIXRenderingContext context,
    UINode           node,
    String           formName
    )
  {
    return org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.SubmitButtonRenderer.createJSFunctionCall(
                                          context,
                                          node,
                                          formName); 
  }  
  
  /**
   * Override of ResetButtonRenderer.getAltRenderer() which returns the
   * alternate Renderer for submit buttons.
   * @return xhtml SubmitButtonRenderer
   */
  protected Renderer getAltRenderer()
  {
    return _ALTERNATE_RENDERER;
  }
  
  // Alternate renderer in screen reader mode
  private static final Renderer _ALTERNATE_RENDERER =
    new org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml.SubmitButtonRenderer();  
}
