/*
 * Copyright  2003-2006 The Apache Software Foundation.
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

import java.util.Map;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.share.xml.NamespaceURI;

import org.apache.myfaces.adfinternal.ui.Renderer;
import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.ui.UINode;

import org.apache.myfaces.adfinternal.skin.icon.Icon;
import org.apache.myfaces.adf.util.ArrayMap;

/**
 * Renderer for the icon component.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/IconRenderer.java#0 $) $Date: 10-nov-2005.18:53:57 $
 * @author The Oracle ADF Faces Team
 */
public class IconRenderer implements Renderer
{
  /**
   * Render the icon
   */
  public void render(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    NamespaceURI qname = (NamespaceURI)node.getAttributeValue(context, 
                                             UIConstants.NAME_ATTR);

    if (qname != null)
    {
      String name = qname.getName();

      if (name !=  null)
      {
        String namespace = qname.getNamespace();
        if (namespace == null)
          namespace = UIConstants.MARLIN_NAMESPACE;
        

        Icon icon = context.getIcon(/*namespace, */name);
        
        if (icon == null)
        {
          // if we didn't get an icon from the context, then create a new
          // name, assuming the user set the short name. e.g.,
          // the user sets name="required", and we map it to the aliased
          // name, "AFRequiredIcon", which is the key where
          // the icon is stored on the skin.       
          StringBuffer fullName = new StringBuffer(_ICON_NAME_PREFIX.length() + 
                                                   name.length() + 
                                                   _ICON_NAME_SUFFIX.length());
          fullName.append(_ICON_NAME_PREFIX);
          char firstChar = Character.toUpperCase(name.charAt(0));
          fullName.append(firstChar);
          fullName.append(name.substring(1));
          fullName.append(_ICON_NAME_SUFFIX);
          
          icon = context.getIcon(/*namespace, */fullName.toString());
        }

        // If we've got an Icon, render it
        if (icon != null)
        {
          // The span is written out here because the writer needs to see the UIComponent.
          ResponseWriter writer = context.getResponseWriter();
          writer.startElement("span", node.getUIComponent());
          AdfRenderingContext arc = AdfRenderingContext.getCurrentInstance();
          FacesContext fContext = context.getFacesContext();
          icon.renderIcon(fContext, arc, _getNodeAttributeMap(context, node));
          writer.endElement("span");
        }
      }
    }
  }

  /**
   * retrieve attributes from the node that might be useful for the icon.
   * @param context 
   * @param node 
   * @return Map containing IconConstants as keys, and the values from 
   *             the node.
   */
  private Map _getNodeAttributeMap(
    RenderingContext context,
    UINode           node)
  {
    Map attrs = null;

    Object id         = node.getAttributeValue(context, 
                                               UIConstants.ID_ATTR);
    Object shortDesc  = node.getAttributeValue(context, 
                                               UIConstants.SHORT_DESC_ATTR);
    Object styleClass = node.getAttributeValue(context, 
                                               UIConstants.STYLE_CLASS_ATTR);
    Object embedded   = node.getAttributeValue(context, 
                                               UIConstants.EMBEDDED_ATTR);
    
    attrs = new ArrayMap(4);
    attrs.put(Icon.ID_KEY, id);
    attrs.put(Icon.SHORT_DESC_KEY, shortDesc);
    attrs.put(Icon.STYLE_CLASS_KEY, styleClass);
    attrs.put(Icon.EMBEDDED_KEY, embedded);

    return attrs;
  }

  private static String _ICON_NAME_PREFIX = "AF";
  private static String _ICON_NAME_SUFFIX = "Icon";
  
}
