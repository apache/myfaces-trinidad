/*
 * Copyright  2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelGroup;

import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;

public class PanelGroupRenderer extends XhtmlRenderer
{
  public PanelGroupRenderer()
  {
    super(CorePanelGroup.TYPE);
  }
  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _layoutKey = type.findKey("layout");
  }


  public boolean getRendersChildren()
  {
    return true;
  }

  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    Object layout        = getLayout(bean);
    boolean isVertical   = CorePanelGroup.LAYOUT_VERTICAL.equals(layout);;
    boolean isHorizontal = CorePanelGroup.LAYOUT_HORIZONTAL.equals(layout);

    if (isHorizontal)
    {
      rw.startElement("table", component);
      OutputUtils.renderLayoutTableAttributes(context, arc, "0", null);
    }
    else if (isVertical)
    {
      rw.startElement("div", component);
    }
    else
    {
      rw.startElement("span", component);
    }

    renderId(context, component);
    renderAllAttributes(context, arc, bean);

    if (isHorizontal)
    {
      rw.startElement("tr", null);
    }

    _encodeChildren(context, component, isVertical, isHorizontal);

    if (isHorizontal)
    {
      rw.endElement("tr");
      rw.endElement("table");
    }
    else if (isVertical)
    {
      rw.endElement("div");
    }
    else
    {
      rw.endElement("span");
    }
  }

  /**
   * Render all the children of the PanelGroup
   */
  private void _encodeChildren(
    FacesContext context,
    UIComponent  component,
    boolean      isVertical,
    boolean      isHorizontal
    ) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    List children   = component.getChildren();
    int  childCount = component.getChildCount();

    UIComponent separator = getFacet(component,
                                     CorePanelGroup.SEPARATOR_FACET);

    boolean needSeparator = false;
    for (int i = 0; i < childCount; i++)
    {
      UIComponent child = (UIComponent) children.get(i);
      if (!child.isRendered())
        continue;

      if (needSeparator)
      {
        if (isVertical)
          rw.startElement("div", null);
        if (separator != null)
          _encodeChild(context, separator, isHorizontal);
        if (isVertical)
          rw.endElement("div");
      }

      _encodeChild(context, child, isHorizontal);
      if ((separator != null) || isVertical)
        needSeparator = true;
    }
  }

  /**
   * Render a single child (or the separator facet)
   */
  private void _encodeChild(
    FacesContext context,
    UIComponent  child,
    boolean      isHorizontal) throws IOException
  {
    if (isHorizontal)
    {
      ResponseWriter rw = context.getResponseWriter();
      rw.startElement("td", null);
      encodeChild(context, child);
      rw.endElement("td");
    }
    else
    {
      encodeChild(context, child);
    }
  }

  protected Object getLayout(FacesBean bean)
  {
    return bean.getProperty(_layoutKey);
  }


  private PropertyKey _layoutKey;
}
