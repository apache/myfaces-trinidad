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
package org.apache.myfaces.adfinternal.ui.laf.base.xhtml;

import java.io.IOException;

import org.apache.myfaces.adfinternal.ui.BaseRenderer;
import org.apache.myfaces.adfinternal.ui.NodeRole;
import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.RoledRenderer;
import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.ui.UINode;

/**
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ImportScriptRenderer.java#0 $) $Date: 10-nov-2005.18:53:59 $
 * @author The Oracle ADF Faces Team
 */
public class ImportScriptRenderer extends BaseRenderer
  implements UIConstants, RoledRenderer

{
  public NodeRole getNodeRole(
    RenderingContext context,
    UINode           node)
  {
    return USER_INVISIBLE_ROLE;
  }

  protected void renderContent(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    Object names = node.getAttributeValue(context, NAMES_ATTR);
    if (names instanceof Object[])
    {
      Object[] array = (Object[]) names;
      for (int i = 0; i < array.length; i++)
        _importScript(context, array[i]);
    }
    else
    {
      _importScript(context, names);
    }
  }

  private void _importScript(
    RenderingContext context,
    Object           name) throws IOException
  {
    XhtmlLafUtils.addLib(context, name);
  }
}
