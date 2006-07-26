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
package org.apache.myfaces.trinidadinternal.renderkit.htmlBasic;

import java.io.IOException;
import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.bean.FacesBean;

import org.apache.myfaces.trinidadinternal.renderkit.RenderUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.CommandLinkRenderer;

/**
 * Renderer for h:commandLink.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/htmlBasic/CommandLinkRenderer.java#0 $) $Date: 10-nov-2005.19:00:39 $
 * @author The Oracle ADF Faces Team
 */
public class HtmlCommandLinkRenderer extends Renderer
{
  public void decode(FacesContext context,
                     UIComponent component)
  {
    createRenderer(component).decode(context, component);
  }

  public boolean getRendersChildren()
  {
    return true;
  }

  public void encodeChildren(FacesContext context,
                             UIComponent component) throws IOException
  {
    // Do nothing - we'll do it all in encodeEnd()
  }

  public void encodeEnd(FacesContext context,
                     UIComponent component) throws IOException
  {
    // The af:commandLink is not a rendersChildren component,
    // but h:commandLink is.  Hence, the difference in behavior
    Renderer renderer = createRenderer(component);
    renderer.encodeBegin(context, component);

    Iterator children = component.getChildren().iterator();
    while (children.hasNext())
      RenderUtils.encodeRecursive(context, (UIComponent) children.next());

    renderer.encodeEnd(context, component);
  }

  protected Renderer createRenderer(final UIComponent component)
  {
    final FacesBean bean = new ComponentFacesBean(component);
    return new CommandLinkRenderer()
    {
      public FacesBean getFacesBean(UIComponent comp)
      {
        return bean;
      }

      protected String getText(FacesBean bean)
      {
        return toString(component.getAttributes().get("value"));
      }

      protected String getShortDesc(FacesBean bean)
      {
        return toString(component.getAttributes().get("title"));
      }

      protected char getAccessKey(FacesBean bean)
      {
        return toChar(component.getAttributes().get("accesskey"));
      }

      protected String getInlineStyle(FacesBean bean)
      {
        return toString(component.getAttributes().get("style"));
      }
    };
  }
}
