/*
 * Copyright  2002-2006 The Apache Software Foundation.
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

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;

import org.apache.myfaces.adfinternal.skin.icon.Icon;


/**
 * Renderer for the iconKey component.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/IconKeyRenderer.java#0 $) $Date: 10-nov-2005.18:53:57 $
 * @author The Oracle ADF Faces Team
 */
public class IconKeyRenderer extends XhtmlLafRenderer
{
  protected void renderContent(RenderingContext context, UINode node)
    throws IOException
  {
    Object name = node.getAttributeValue(context, NAME_ATTR);
    if (ICON_REQUIRED.equals(name))
    {

      // Get the required Icon from the context
      Icon icon = context.getIcon(XhtmlLafConstants.REQUIRED_ICON_ALIAS_NAME);
      String title = getTranslatedString(context, "REQUIRED_TIP");
      XhtmlLafUtils.renderIcon(context, icon, title, null);
      
      ResponseWriter writer = context.getResponseWriter();

      writer.writeText(XhtmlLafConstants.NBSP_STRING, null);
      writer.writeText(getTranslatedValue(context, _REQUIRED_KEY), null);
    }
  }

  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    return "span";
  }

  protected Object getStyleClass(
    RenderingContext context,
    UINode           node)
  {
    return XhtmlLafConstants.TIP_TEXT_STYLE_CLASS;
  }
  
  // translation keys
  private static final String _REQUIRED_KEY = 
    "af_objectLegend.REQUIRED_KEY";  
}
