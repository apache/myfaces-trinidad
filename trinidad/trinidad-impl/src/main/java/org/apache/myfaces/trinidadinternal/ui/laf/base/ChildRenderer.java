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
package org.apache.myfaces.trinidadinternal.ui.laf.base;

import org.apache.myfaces.trinidadinternal.ui.BaseRenderer;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * Renderer that renders nothing.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/ChildRenderer.java#0 $) $Date: 10-nov-2005.18:52:58 $
 * @author The Oracle ADF Faces Team
 */
public class ChildRenderer extends BaseRenderer
{
  static public Renderer getInstance()
  {
    return _INSTANCE;
  }
  
  /**
   * Override to not render any surrounding element tag
   */
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return null;
  }

  private ChildRenderer()
  {
  }
  
  private static final ChildRenderer _INSTANCE = new ChildRenderer();
}
