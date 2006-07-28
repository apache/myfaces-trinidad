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
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.bean.FacesBean;

import org.apache.myfaces.trinidadinternal.renderkit.RenderUtils;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.FormRenderer;

/**
 * Renderer for h:commandLink.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/htmlBasic/FormRenderer.java#0 $) $Date: 10-nov-2005.19:00:39 $
 * @author The Oracle ADF Faces Team
 */
public class HtmlFormRenderer extends Renderer
{
  public void decode(FacesContext context,
                     UIComponent component)
  {
    createRenderer(component).decode(context, component);
  }

  public boolean getRendersChildren()
  {
    return false;
  }


  public void encodeBegin(FacesContext context,
                             UIComponent component) throws IOException
  {
    createRenderer(component).encodeBegin(context, component);
  }

  public void encodeEnd(FacesContext context,
                     UIComponent component) throws IOException
  {
    createRenderer(component).encodeEnd(context, component);
  }

  protected Renderer createRenderer(final UIComponent component)
  {
    final FacesBean bean = new ComponentFacesBean(component);
    return new FormRenderer()
    {
      public FacesBean getFacesBean(UIComponent comp)
      {
        return bean;
      }

      protected String getInlineStyle(FacesBean bean)
      {
        return toString(component.getAttributes().get("style"));
      }

      protected String getTargetFrame(FacesBean bean)
      {
        return toString(component.getAttributes().get("target"));
      }

      protected String getShortDesc(FacesBean bean)
      {
        return toString(component.getAttributes().get("title"));
      }

      protected boolean getUsesUpload(FacesBean bean)
      {
        Map attrs = component.getAttributes();
        return "multipart/form-data".equals(attrs.get("enctype"));
      }
    };
  }
}
