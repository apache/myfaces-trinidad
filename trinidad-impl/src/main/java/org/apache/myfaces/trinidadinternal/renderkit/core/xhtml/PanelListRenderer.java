/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelList;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidad.util.IntegerUtils;

public class PanelListRenderer extends XhtmlRenderer
{
  public PanelListRenderer()
  {
    super(CorePanelList.TYPE);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _maxColumnsKey = type.findKey("maxColumns");
    _rowsKey = type.findKey("rows");
    _listStyleKey = type.findKey("listStyle");

  }

  /*
   * We want to render the styleClass/inlineStyle attributes as well
   * as our component styleClass.
   */
  @Override
  protected void renderStyleAttributes(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean) throws IOException
  {
    renderStyleAttributes(context, arc, bean,
                          SkinSelectors.AF_PANEL_LIST_STYLE_CLASS);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {

    int split = _getSplit(bean);
    int maxCols = _getMaxColumns(arc, bean);
    int childCount = component.getChildCount();
    // no kids/ no rows / no columns, then no list
    if (childCount <= 0 || split < 1 || maxCols < 1)
      return;

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("div", component);
    renderId(context, component);
    renderAllAttributes(context, arc, bean);

    ((CoreRenderingContext) arc).setDefaultLinkStyleDisabled(true);

    if (split >= childCount)
      _renderNoColumns(context, arc, component, bean, 0, childCount);
    else
      _renderColumns(context, arc, component, bean, split, childCount);
    ((CoreRenderingContext) arc).setDefaultLinkStyleDisabled(false);
    writer.endElement("div");

  }

  protected Number getRows(FacesBean bean)
  {
    return (Number)bean.getProperty(_rowsKey);
  }

  protected Number getMaxColumns(FacesBean bean)
  {
    return (Number)bean.getProperty(_maxColumnsKey);
  }

  protected String getListStyle(FacesBean bean)
  {
    return (String)bean.getProperty(_listStyleKey);
  }
 
  private void _renderListStyle(
  FacesContext context,
  FacesBean    bean) throws IOException
  {
    String listStyle = getListStyle(bean);
    if (listStyle != null)
    {
      context.getResponseWriter().writeAttribute("style",
                                                  listStyle,
                                                  "listStyle");
    }
  }

  // render without columns. Called when rows > children or when we have
  // multiple columns, and we are rendering a single column.
  @SuppressWarnings("unchecked")
  private int _renderNoColumns(
    FacesContext context,
    RenderingContext arc,
    UIComponent  component,
    FacesBean    bean,
    int          start,
    int          numToRender)
  throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    int maxChildIndex = component.getChildCount();
    int numRendered = 0;
    int childIndex = start;
    List<UIComponent> children   = component.getChildren();

    writer.startElement("ul", null);
    _renderListStyle(context, bean);

    while ( numRendered < numToRender  && childIndex < maxChildIndex)
    {
      UIComponent child = children.get(childIndex);

      // if visible child
      if ( child != null && child.isRendered())
      {
        writer.startElement("li", null);

        encodeChild(context, child);

        writer.endElement("li");
        numRendered++;
      }

      childIndex++;
    }

    writer.endElement("ul");
    return childIndex;
  }


  // render with columns
  private void _renderColumns(
    FacesContext context,
    RenderingContext arc,
    UIComponent  component,
    FacesBean    bean,
    int          split,
    int          childCount)
  throws IOException
  {

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("table", null);
    OutputUtils.renderLayoutTableAttributes(context, arc, "0", "100%");
    writer.startElement("tr", null);

    int start = 0;
    int numToRender = 0;

    int maxCols = _getMaxColumns(arc, bean);

    int numCols = (int) Math.ceil( (double) childCount/ (double)split );

    if ( numCols > maxCols )
       numCols = maxCols;

    if ( numCols > 1 )
      numToRender =
        (int) Math.ceil( ((double)childCount)/ (double) numCols );
    else
      numToRender = childCount;

    String width = IntegerUtils.getString( 100 / numCols );

    width = width + "%";


    for ( int i = 0; i < numCols; i++ )
    {
      writer.startElement("td", null);
      writer.writeAttribute("width",width, null);
      writer.writeAttribute("valign","top", null);

      start =
        _renderNoColumns( context, arc, component, bean, start, numToRender );

      writer.endElement("td");

      if ( start >= childCount )
        break;
    }

    writer.endElement("tr");
    writer.endElement("table");

  }

  private int _getMaxColumns(
    RenderingContext arc,
    FacesBean bean)
  {
    // get the number of columns
    Number maxColumnsNumber = getMaxColumns(bean);
    return (maxColumnsNumber != null) ?
              maxColumnsNumber.intValue() :
              _getDefaultColumns(arc);
  }

  private int _getSplit(FacesBean bean)
  {
    Number splitNumber = getRows(bean);

    return (splitNumber != null) ?
              splitNumber.intValue() :
              SPLIT_DEFAULT;
  }

  private int _getDefaultColumns(RenderingContext arc)
  {

    Integer defaultColumns =
      (Integer)arc.getSkin().getProperty(
        SkinProperties.AF_PANEL_LIST_DEFAULT_COLUMNS);
    return (defaultColumns != null) ?
            defaultColumns.intValue() :
            COLUMNS_DEFAULT;

  }
  //TODO put this default in the XML file. It is the 'rows' default value
  private  static final int SPLIT_DEFAULT = Integer.MAX_VALUE;

  private  static final int COLUMNS_DEFAULT = 3;

  private PropertyKey _maxColumnsKey;
  private PropertyKey _rowsKey;
  private PropertyKey _listStyleKey;
}
