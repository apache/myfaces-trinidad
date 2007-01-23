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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelBorderLayout;
import org.apache.myfaces.trinidad.context.RenderingContext;

/**
 * @version $Name:  $ $
 * @author Adam Winer
 */
public class PanelBorderLayoutRenderer extends XhtmlRenderer
{
  public PanelBorderLayoutRenderer()
  {
    super(CorePanelBorderLayout.TYPE);
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
    boolean hasSideFacets = _hasSideFacets(arc, component);
    if (hasSideFacets)
    {
      _encodeAllWithSideFacets(context, arc, component, bean);
    } 
    else
    {
      _encodeAllInDiv(context, arc, component, bean);
    }
  }

  private boolean _hasSideFacets(RenderingContext arc, UIComponent component)
  {
    // For PDAs, disavow allow knowledge of side facets (there's no space
    // to render them).  Ideally, this would be height/width driven...
    if (isPDA(arc))
      return false;

    return ((getFacet(component, CorePanelBorderLayout.LEFT_FACET) != null) ||
            (getFacet(component, CorePanelBorderLayout.START_FACET) != null) ||
            (getFacet(component, CorePanelBorderLayout.RIGHT_FACET) != null) ||
            (getFacet(component, CorePanelBorderLayout.END_FACET) != null) ||
            (getFacet(component, CorePanelBorderLayout.INNER_LEFT_FACET) != null) ||
            (getFacet(component, CorePanelBorderLayout.INNER_START_FACET) != null) ||
            (getFacet(component, CorePanelBorderLayout.INNER_RIGHT_FACET) != null) ||
            (getFacet(component, CorePanelBorderLayout.INNER_END_FACET) != null));
  }


  protected void _encodeAllWithSideFacets(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("table", component);
    OutputUtils.renderLayoutTableAttributes(context, arc, "0", "100%");
    
    renderId(context, component);
    renderAllAttributes(context, arc, bean);


    Integer rowSpan = _getRowSpan(component);
    Integer colSpan = _getColSpan(component, arc, rowSpan);

    //
    // If we have a header node, render it
    //
    UIComponent topFacet = getFacet(component,
                                       CorePanelBorderLayout.TOP_FACET);
    if (topFacet != null)
    {
      rw.startElement("tr", null);
      _renderMarginSpacer(context, arc, null);

      rw.startElement("td", null);
      rw.writeAttribute("colspan", colSpan, null);
      encodeChild(context, topFacet);
      rw.endElement("td");

      _renderMarginSpacer(context, arc, null);
      rw.endElement("tr");
    }

    //
    // Begin rendering the content row
    //
    rw.startElement("tr", null);

    _renderMarginSpacer(context, arc, rowSpan);


    //
    // Render the left hand side
    //
    String leftName = _getSideFacet(component, arc, true);
    UIComponent leftFacet = getFacet(component, leftName);
    if (leftFacet != null)
      renderSideFacet(context, leftFacet, rowSpan, null);
  
    //
    // Render the inner left side node, if any
    //
    String innerleftName = _getInnerSideFacet(component, arc, true);
    UIComponent innerLeftFacet = getFacet(component, innerleftName);

    if (innerLeftFacet != null)
    {
      renderSideFacet(context, innerLeftFacet, rowSpan, null);
    }
  
    //
    // Render the child on the inside top of the layout
    //
    boolean isRightSideRendered =
      _renderMiddleFacet(context, arc, component,
                         CorePanelBorderLayout.INNER_TOP_FACET, rowSpan,
                         false, true);

        
    // render the content
    if (component.getChildCount() > 0)
    {
      /* if the right hand side has been rendered then we need to start a new
         row. otherwise, we can still render on to the previous row. */
      if (isRightSideRendered)
      {
        rw.startElement("tr", null);
      }
        
      rw.startElement("td", null);
      rw.writeAttribute("width", "100%", null);
      rw.writeAttribute("valign", "top", null);
        
      encodeAllChildren(context, component);

      rw.endElement("td");

      /* if the right hand side nodes have not been rendered, then render them
         here */
      if (!isRightSideRendered)
      {
        _renderRightFacets(context, arc, component, rowSpan);
        isRightSideRendered = true;
      }

      rw.endElement("tr");
    }

    /* render the inner bottom child. start a new TR only if the 
       right side nodes have been rendered, and render the 
       right side nodes if they have not been rendered.  */
    isRightSideRendered |=
      _renderMiddleFacet(context, arc, component,
                         CorePanelBorderLayout.INNER_BOTTOM_FACET, rowSpan,
                         isRightSideRendered,
                         !isRightSideRendered);

    /* if a right hand side still has not been rendered then we need
       to render it here */
    if (!isRightSideRendered) 
    {
      _renderRightFacets(context, arc, component, rowSpan);
      rw.endElement("tr");
    }

    //
    // render the  bottom node
    //
    UIComponent bottomFacet = getFacet(component,
                                       CorePanelBorderLayout.BOTTOM_FACET);
    if (bottomFacet != null)
    {       
      rw.startElement("tr", null);
      _renderMarginSpacer(context, arc, null);
        
      rw.startElement("td", null);
      rw.writeAttribute("colspan", colSpan, null);
      encodeChild(context, bottomFacet);
      rw.endElement("td");
        
      _renderMarginSpacer(context, arc, null);
      rw.endElement("tr");
    }

    rw.endElement("table");
  }


  protected void _encodeAllInDiv(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("div", component);
    renderId(context, component);
    renderAllAttributes(context, arc, bean);

    UIComponent top = getFacet(component, CorePanelBorderLayout.TOP_FACET);
    if (top != null)
      encodeChild(context, top);

    rw.startElement("div", null);
    rw.endElement("div");
    
    UIComponent innerTop = getFacet(component, CorePanelBorderLayout.INNER_TOP_FACET);
    if (innerTop != null)
      encodeChild(context, innerTop);

    rw.startElement("div", null);
    rw.endElement("div");
    
    encodeAllChildren(context, component);

    rw.startElement("div", null);
    rw.endElement("div");

    UIComponent innerBottom = getFacet(component, CorePanelBorderLayout.INNER_BOTTOM_FACET);
    if (innerBottom != null)
      encodeChild(context, innerBottom);
    
    rw.startElement("div", null);
    rw.endElement("div");

    UIComponent bottom = getFacet(component, CorePanelBorderLayout.BOTTOM_FACET);
    if (bottom != null)
      encodeChild(context, bottom);

    rw.endElement("div");
  }

  /**
   * Renders one of the side nodes.
   */
  protected void renderSideFacet(
    FacesContext context,
    UIComponent  sideFacet,
    Integer      rowSpan,
    String       width
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("td", null);
    writer.writeAttribute("valign", "top", null);
    writer.writeAttribute("rowspan", rowSpan, null);
    writer.writeAttribute("width", width, null);
    encodeChild(context, sideFacet);
    writer.endElement("td");
  }


  private boolean _renderMiddleFacet(FacesContext context,
                                     RenderingContext arc,
                                     UIComponent component,
                                     String middleFacetName,
                                     Integer rowSpan,
                                     boolean startTableRow,
                                     boolean renderRightFacets)
    throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    UIComponent middleFacet = getFacet(component, middleFacetName);
    if (middleFacet != null)
    {
      if (startTableRow) 
        writer.startElement("tr", null);
    
      _renderInnerFacet(context, middleFacet);

      if (renderRightFacets)
        _renderRightFacets(context, arc, component, rowSpan);

      writer.endElement("tr");
      return true;
    }
    return false;
  }

  /**
   * Renders the right hand side facets.
   * @return true if at least one facet was rendered.
   */
  private boolean _renderRightFacets(
    FacesContext     context,
    RenderingContext arc,
    UIComponent      component,
    Integer          rowSpan
    ) throws IOException
  {
    boolean facetRendered = false;
    
    //
    // Render the inner right node.
    //
    String innerSideName = _getInnerSideFacet(component, arc, false);
    UIComponent innerSideFacet = getFacet(component, innerSideName);
    if (innerSideFacet != null)
    {
      renderSideFacet(context,
                      innerSideFacet,
                      rowSpan,
                      null);

      facetRendered = true;
    }
    
    //
    // Render the side node
    //
    String sideName = _getSideFacet(component, arc, false);
    UIComponent sideFacet = getFacet(component, sideName);

    if (sideFacet != null)
    {
      renderSideFacet(context, sideFacet, rowSpan, null);
      _renderMarginSpacer(context, arc, rowSpan);
      
      facetRendered = true;
    }
    
    return facetRendered;
  }
  

  /**
   * Renders one of the inner nodes.
   */
  private void _renderInnerFacet(
    FacesContext context,
    UIComponent  facet
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("td", null);
    writer.writeAttribute("width", "100%", null);
    encodeChild(context, facet);
    writer.endElement("td");
  }


  private void _renderMarginSpacer(
    FacesContext     context,
    RenderingContext arc,
    Integer          rowSpan
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("td", null);
    writer.writeAttribute("rowspan", rowSpan, null);
    Object spacerWidth = arc.getSkin().getProperty(
       SkinProperties.AF_PANEL_BORDER_LAYOUT_SPACER_WIDTH);
    if (spacerWidth != null)
      renderSpacer(context, arc, spacerWidth.toString(), "0");
    writer.endElement("td");
  }
    

  /**
   * Returns the row span to use for the left and right children
   */
  private Integer _getRowSpan(
    UIComponent component
    )
  {
    int rowSpan = 0;

    if (getFacet(component, CorePanelBorderLayout.INNER_TOP_FACET) != null)
      rowSpan++;

    // increment the rowspan if we have any content children.
    if (component.getChildCount() > 0)
      rowSpan++;

    if (getFacet(component, CorePanelBorderLayout.INNER_BOTTOM_FACET) != null)
      rowSpan++;

    return (rowSpan > 0) ? rowSpan : null;
  }


  /**
   * Returns the column span to use for the top and bottom children.
   */
  private Integer _getColSpan(
    UIComponent       component,
    RenderingContext  arc,
    Integer           rowSpan
    )
  {
    int colSpan = 0;
    
    // increment the colspan if we have a left side node
    if (getFacet(component, _getSideFacet(component, arc, true)) != null)
    {
      colSpan++;      
    }

    // increment the colspan if we have an inner left side node
    if (getFacet(component, _getInnerSideFacet(component, arc, true)) != null)
    {
      colSpan++;      
    }

    // increment the colspan if we have any center nodes.
    if (rowSpan != null)
    {
      colSpan++;
    }

    // increment the colspan if we have an inner right side node
    if (getFacet(component, _getInnerSideFacet(component, arc, false)) != null)
    {
      colSpan++;
    }
    
    // increment the colspan if we have a right side node
    if (getFacet(component, _getSideFacet(component, arc, false)) != null)
    {
      colSpan++;
    }

    // return null if the colspan is zero
    return (colSpan > 0) ? colSpan : null;
  }


  /**
   * Returns the name of the node to use for the left or right side of the
   * layout, following bi-di rules.
   */
  private String _getSideFacet(
    UIComponent         component,
    RenderingContext    arc,
    boolean             getLeftName
    )
  {
    boolean isRTL = arc.isRightToLeft();

    //
    // A precisely specified name wins over a dynamic name
    //
    // Since the browser flips for us in a bidi environment,
    // we actually flip the left and right child if the
    // direction is exact
    //

    String exactName = (isRTL)
                         ? (getLeftName)
                             ? CorePanelBorderLayout.RIGHT_FACET
                             : CorePanelBorderLayout.LEFT_FACET
                         : (getLeftName)
                             ? CorePanelBorderLayout.LEFT_FACET
                             : CorePanelBorderLayout.RIGHT_FACET;

    if (getFacet(component, exactName) == null)
    {
      //
      // if not exact, fall back on the start or end child
      //

      exactName = (getLeftName) ?
        CorePanelBorderLayout.START_FACET : CorePanelBorderLayout.END_FACET;
    }

    return exactName;
  }


  /**
   * Returns the name of the node to use for the left or right side of the
   * layout, following bi-di rules.
   */
  private String _getInnerSideFacet(
    UIComponent         component,
    RenderingContext    arc,
    boolean             getLeftName
    )
  {
    boolean isRTL = arc.isRightToLeft();

    //
    // A precisely specified name wins over a dynamic name
    //
    // Since the browser flips for us in a bidi environment,
    // we actually flip the left and right child if the
    // direction is exact
    //

    String exactName = (isRTL)
                         ? (getLeftName)
                             ? CorePanelBorderLayout.INNER_RIGHT_FACET
                             : CorePanelBorderLayout.INNER_LEFT_FACET
                         : (getLeftName)
                             ? CorePanelBorderLayout.INNER_LEFT_FACET
                             : CorePanelBorderLayout.INNER_RIGHT_FACET;

    if (getFacet(component, exactName) == null)
    {
      //
      // if not exact, fall back on the start or end child
      //

      exactName = (getLeftName) ?
        CorePanelBorderLayout.INNER_START_FACET : CorePanelBorderLayout.INNER_END_FACET;
    }

    return exactName;
  }
}
