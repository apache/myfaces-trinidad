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
package org.apache.myfaces.adfinternal.renderkit.uix;

import java.io.IOException;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.adf.component.UIXShowDetail;

import org.apache.myfaces.adfinternal.uinode.UINodeRendererBase;

/**
 * Renderer for ShowOneTab
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/uix/ShowOneTabRenderer.java#0 $) $Date: 10-nov-2005.19:00:37 $
 * @author The Oracle ADF Faces Team
 */
public class ShowOneTabRenderer extends UINodeRendererBase
{
  /**
   * Make sure we've got at least one selected item;  if not,
   * the "decode" phase will have problems.
   */
  public void encodeBegin(FacesContext context, UIComponent component)
    throws IOException
  {
    List children = component.getChildren();
    int childCount = children.size();
    boolean oneIsDisclosed = false;
    for (int i=0; i<childCount; i++)
    {
      UIXShowDetail child =  (UIXShowDetail) children.get(i);
      if (child.isDisclosed())
      {
        oneIsDisclosed = true;
        break;
      }
    }

    // OK, nothing's selected: pick the first non-disabled item
    if (!oneIsDisclosed)
    {
      for (int i=0; i<childCount; i++)
      {
        UIXShowDetail child =  (UIXShowDetail) children.get(i);
        Object disabled = child.getAttributes().get("disabled");
        if (Boolean.TRUE.equals(disabled))
          continue;

        child.setDisclosed(true);
        break;
      }
    }

    super.encodeBegin(context, component);
  }
}
