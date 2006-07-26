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

import org.apache.myfaces.trinidad.logging.ADFLogger;

import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.Renderer;
import org.apache.myfaces.trinidadinternal.ui.RendererManager;
import org.apache.myfaces.trinidadinternal.ui.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.RoledRenderer;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;

abstract public class AliasRenderer implements Renderer, RoledRenderer
{
  public void render(
    RenderingContext context,
    UINode           node) throws IOException
  {
    Renderer renderer = _getRenderer(context, node);
    if (renderer != null)
      renderer.render(context, node);
  }

  public NodeRole getNodeRole(
    RenderingContext context,
    UINode           node)
  {
    Renderer renderer = _getRenderer(context, node);
    if (renderer instanceof RoledRenderer)
      return ((RoledRenderer) renderer).getNodeRole(context, node);

    return UIConstants.UNKNOWN_ROLE;
  }

  private Renderer _getRenderer(
    RenderingContext context,
    UINode           node)
  {
    RendererManager manager = context.getRendererManager();
    String localName = getLocalName(context, node);
    Renderer renderer = manager.getRenderer(node.getNamespaceURI(), localName);
    if (renderer == null)
      _LOG.severe("Could not find renderer for alias " + localName);

    return renderer;
  }


  abstract protected String getLocalName(
    RenderingContext context,
    UINode           node);

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(AliasRenderer.class);
}
