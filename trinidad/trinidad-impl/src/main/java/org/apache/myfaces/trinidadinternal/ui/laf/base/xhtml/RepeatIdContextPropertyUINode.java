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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.io.RepeatIdResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

import org.apache.myfaces.trinidadinternal.ui.composite.ContextPropertyUINode;

/**
 * Node that extends ContextPropertyUINode and renders a suffix on its
 * 'id' and 'for' attributes.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/RepeatIdContextPropertyUINode.java#0 $) $Date: 10-nov-2005.18:54:10 $
 * @author The Oracle ADF Faces Team
 */

public final class RepeatIdContextPropertyUINode extends ContextPropertyUINode
{

  public RepeatIdContextPropertyUINode(
   String propertyNamespace,
   Object propertyName,
   Object propertyValue)
  {
    super(propertyNamespace, propertyName, propertyValue);
  }


  /**
   * 
   * render using the RepeatIdResponseWriter.
   * @param context
   * @param node
   * @throws IOException
   */
  public void render(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    ResponseWriter oldRW = RepeatIdResponseWriter.install(context.getFacesContext());
    try
    {
      super.render(context, node);
    }
    finally
    {
      RepeatIdResponseWriter.remove(context.getFacesContext(), oldRW);
    }
  }

}
