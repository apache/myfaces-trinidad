/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit;

import java.io.IOException;
import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.component.UIForm;

import javax.faces.context.FacesContext;

import org.apache.myfaces.adf.component.UIXForm;

/**
 * Generic utilities for rendering.
 *
 * @author The Oracle ADF Faces Team
 */
public class RenderUtils
{
  private RenderUtils()
  {
  }


  /**
   * Encodes a component and all of its children.
   */
  static public void encodeRecursive(FacesContext context,
                                     UIComponent component)
    throws IOException
  {
    if (component.isRendered())
    {
      component.encodeBegin(context);
      if (component.getRendersChildren())
      {
        component.encodeChildren(context);
      }
      else
      {
        if (component.getChildCount() > 0)
        {
          Iterator children = component.getChildren().iterator();
          while (children.hasNext())
            encodeRecursive(context, (UIComponent) children.next());
        }
      }

      component.encodeEnd(context);
    }
  }

  /**
   *  Retrieves id of the form the component is contained in.
   *
   * @param context the faces context object
   * @param component UIComponent whose container form id is to be retuirned
   *
   * @return String id of the form if one exists in component's hierarchy,
   *                otherwise null
   */
  public static String getFormId(
    FacesContext context,
    UIComponent  component)
  {
    UIComponent form = null;
    while (component != null)
    {
      if ((component instanceof UIForm) ||
          (component instanceof UIXForm))
      {
        form = component;
        break;
      }

      component = component.getParent();
    }

    if (form == null)
      return null;

    return form.getClientId(context);
  }
}
