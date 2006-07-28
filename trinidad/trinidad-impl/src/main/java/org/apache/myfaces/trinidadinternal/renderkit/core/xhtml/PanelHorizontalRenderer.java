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
import org.apache.myfaces.trinidad.component.core.layout.CorePanelHorizontal;

import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;

public class PanelHorizontalRenderer extends XhtmlRenderer
{
  public PanelHorizontalRenderer()
  {
    super(CorePanelHorizontal.TYPE);
  }
  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _valignKey = type.findKey("valign");
    _halignKey = type.findKey("halign");
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

    Object valign = getValign(bean);
    Object halign = getHalign(bean);


    rw.startElement("table", component);
    OutputUtils.renderLayoutTableAttributes(context, arc, "0", null);
    if (CorePanelHorizontal.HALIGN_CENTER.equals(halign))
      rw.writeAttribute("align", "center", "halign");

    renderId(context, component);
    renderAllAttributes(context, arc, bean);


    rw.startElement("tr", null);

    _encodeChildren(context, arc, component, valign, halign);


    rw.endElement("tr");
    rw.endElement("table");

  }
  

  protected Object getValign(FacesBean bean)
  {
    return bean.getProperty(_valignKey);
  }

  protected Object getHalign(FacesBean bean)
  {
    return bean.getProperty(_halignKey);
  }  

  /**
   * Render all the children of the PanelGroup
   */
  private void _encodeChildren(
    FacesContext context,
    RenderingContext arc,
    UIComponent  component,
    Object       vAlign,
    Object       hAlign
    ) throws IOException
  {

    List children   = component.getChildren();
    int  childCount = component.getChildCount();


    UIComponent separator = getFacet(component,
                                     CorePanelHorizontal.SEPARATOR_FACET);

    boolean needSeparator = false;
    boolean isFirstChild = true;
    
    boolean isEndAlignment;
    if (CorePanelHorizontal.HALIGN_END.equals(hAlign))
      isEndAlignment = true;
    else if (CorePanelHorizontal.HALIGN_LEFT.equals(hAlign))
      isEndAlignment = arc.isRightToLeft();
    else if (CorePanelHorizontal.HALIGN_RIGHT.equals(hAlign))
      isEndAlignment = !arc.isRightToLeft();
    else
      isEndAlignment = false;
    
    for (int i = 0; i < childCount; i++)
    {
      UIComponent child = (UIComponent) children.get(i);
      if (!child.isRendered())
        continue;

      if (isFirstChild)
      {
        isFirstChild = false;
        if (isEndAlignment)
        {
          ResponseWriter rw = context.getResponseWriter();
          rw.startElement("td", null);
          rw.writeAttribute("width", "100%", null);
          rw.endElement("td");
        }
      }

      if (needSeparator)
      {
        if (separator != null)
        {
          _encodeChild(context, separator, vAlign);
        }
      }

      _encodeChild(context, child, vAlign);
      if (separator != null )
        needSeparator = true;
    }
  }

  /**
   * Render a single child (or the separator facet)
   */
  private void _encodeChild(
    FacesContext context,
    UIComponent  child,
    Object       vAlign) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("td", null);
    rw.writeAttribute("valign", vAlign, null);

    encodeChild(context, child);

    rw.endElement("td");

  }

  private PropertyKey _valignKey;
  private PropertyKey _halignKey;
}
