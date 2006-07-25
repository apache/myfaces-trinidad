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
package org.apache.myfaces.adfinternal.ui.html;

import java.io.IOException;

import java.util.Iterator;

import org.apache.myfaces.adfinternal.ui.AttributeKey;
import org.apache.myfaces.adfinternal.ui.ElementRenderer;
import org.apache.myfaces.adfinternal.ui.Renderer;
import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.ui.UINode;

/**
 * Renderer implementation that outputs HTML elements.  Copies
 * all the attributes set on the node directly to the output method.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/html/HTMLElementRenderer.java#0 $) $Date: 10-nov-2005.18:56:25 $
 * @author The Oracle ADF Faces Team
 */
public class HTMLElementRenderer extends ElementRenderer
{
  /**
   * Returns a shared instance fo the renderer.
   */
  static public Renderer getRenderer()
  {
    return _sInstance;
  }

  /**
   * Called to retrieve the element name to render.
   * @param context the rendering context
   * @param node the current node
   */
  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    return node.getLocalName();
  }

  /**
   * Renders all attributes of the current node.
   */
  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    Iterator attrs = node.getAttributeNames(context);
    if (attrs != null)
    {
      while (attrs.hasNext())
      {
        AttributeKey currKey = (AttributeKey) attrs.next();
        if ((currKey != UIConstants.RENDERED_ATTR) &&
            (currKey != UIConstants.ANNOTATION_ATTR))
          renderAttribute(context,
                          currKey.getAttributeName(),
                          node.getAttributeValue(context, currKey));
      }
    }
  }

  // A shared instance.
  static private final Renderer _sInstance = new HTMLElementRenderer();
}
