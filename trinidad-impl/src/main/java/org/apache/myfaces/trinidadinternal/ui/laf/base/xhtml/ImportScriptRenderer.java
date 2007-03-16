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

import org.apache.myfaces.trinidadinternal.ui.BaseRenderer;
import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.RoledRenderer;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ImportScriptRenderer.java#0 $) $Date: 10-nov-2005.18:53:59 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class ImportScriptRenderer extends BaseRenderer
  implements UIConstants, RoledRenderer

{
  public NodeRole getNodeRole(
    UIXRenderingContext context,
    UINode           node)
  {
    return USER_INVISIBLE_ROLE;
  }

  @Override
  protected void renderContent(
    UIXRenderingContext context,
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
    UIXRenderingContext context,
    Object           name) throws IOException
  {
    XhtmlLafUtils.addLib(context, name);
  }
}
