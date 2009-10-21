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
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.bean.FacesBean.Type;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelBorderLayout;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.XhtmlConstants;
import org.apache.myfaces.trinidad.util.ComponentUtils;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.jsLibs.Scriptlet;

/**
 * @version $Name:  $ $
 */
public class PanelBorderLayoutRenderer extends XhtmlRenderer
{
  private PropertyKey _layoutKey;
  private PropertyKey _topHeightKey;
  private PropertyKey _innerTopHeightKey;
  private PropertyKey _bottomHeightKey;
  private PropertyKey _innerBottomHeightKey;
  private PropertyKey _leftWidthKey;
  private PropertyKey _innerLeftWidthKey;
  private PropertyKey _rightWidthKey;
  private PropertyKey _innerRightWidthKey;
  private PropertyKey _startWidthKey;
  private PropertyKey _innerStartWidthKey;
  private PropertyKey _endWidthKey;
  private PropertyKey _innerEndWidthKey;
  
  public PanelBorderLayoutRenderer()
  {
    super(CorePanelBorderLayout.TYPE);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }
  
  /**
   * @see org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer#getDefaultStyleClass(
   * org.apache.myfaces.trinidad.bean.FacesBean)
   */
  @Override
  protected String getDefaultStyleClass(FacesBean bean)
  {
    return "positioned".equals(getLayout(bean)) ? SkinSelectors.AF_PANEL_BORDER_POSITIONED_ROOT_STYLE_CLASS : null;
  }
  
  /**
   * @see org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer#shouldRenderId(
   * javax.faces.context.FacesContext, javax.faces.component.UIComponent)
   */
  @Override
  protected boolean shouldRenderId(FacesContext context, UIComponent component)
  {
    return true;
  }
  
  /**
   * @see org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlRenderer#findTypeConstants(
   * org.apache.myfaces.trinidad.bean.FacesBean.Type)
   */
  @Override
  protected void findTypeConstants(Type type)
  {
    super.findTypeConstants(type);
    _layoutKey = type.findKey("layout");
    _topHeightKey = type.findKey("topHeight");
    _innerTopHeightKey = type.findKey("innerTopHeight");
    _bottomHeightKey = type.findKey("bottomHeight");
    _innerBottomHeightKey = type.findKey("innerBottomHeight");
    _leftWidthKey = type.findKey("leftWidth");
    _innerLeftWidthKey = type.findKey("innerLeftWidth");
    _rightWidthKey = type.findKey("rightWidth");
    _innerRightWidthKey = type.findKey("innerRightWidth");
    _startWidthKey = type.findKey("startWidth");
    _innerStartWidthKey = type.findKey("innerStartWidth");
    _endWidthKey = type.findKey("endWidth");
    _innerEndWidthKey = type.findKey("innerEndWidth");
  }
  
  protected String getLayout(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_layoutKey), (String)_layoutKey.getDefault());
  }
  
  protected String getTopHeight(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_topHeightKey), (String)_topHeightKey.getDefault());
  }
  
  protected String getinnerTopHeight(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_innerTopHeightKey), (String)_innerTopHeightKey.getDefault());
  }
  
  protected String getBottomHeight(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_bottomHeightKey), (String)_bottomHeightKey.getDefault());
  }
  
  protected String getinnerBottomHeight(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_innerBottomHeightKey), (String)_innerBottomHeightKey.getDefault());
  }
  
  protected String getLeftWidth(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_leftWidthKey), (String)_leftWidthKey.getDefault());
  }
  
  protected String getinnerLeftWidth(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_innerLeftWidthKey), (String)_innerLeftWidthKey.getDefault());
  }
  
  protected String getRightWidth(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_rightWidthKey), (String)_rightWidthKey.getDefault());
  }
  
  protected String getinnerRightWidth(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_innerRightWidthKey), (String)_innerRightWidthKey.getDefault());
  }
  
  protected String getEndWidth(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_endWidthKey), (String)_endWidthKey.getDefault());
  }
  
  protected String getEndInnerWidth(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_innerEndWidthKey), (String)_innerEndWidthKey.getDefault());
  }
  
  protected String getStartWidth(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_startWidthKey), (String)_startWidthKey.getDefault());
  }
  
  protected String getStartInnerWidth(FacesBean bean)
  {
    return ComponentUtils.resolveString(bean.getProperty(_innerStartWidthKey), (String)_innerStartWidthKey.getDefault());
  }

  @Override
  protected void encodeAll(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    String layout = getLayout(bean);
    
    if ("positioned".equals(layout))
    {
      _encodeAllPositioned(context, rc, component, bean);
    }
    else
    {
      boolean hasSideFacets = _hasSideFacets(rc, component);
      if (hasSideFacets)
      {
        _encodeAllWithSideFacets(context, rc, component, bean);
      }
      else
      {
        _encodeAllInDiv(context, rc, component, bean);
      }
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
  
  /**
   * Encode the panel for a positioned layout
   */
  private void _encodeAllPositioned(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    
    // IE6 does not support stretching by setting both top & bottom or left & right,
    // so we have to use some ugly code.
    TrinidadAgent agent = (TrinidadAgent)rc.getAgent();
    boolean isIE6 = Agent.AGENT_IE.equals(agent.getAgentName()) &&
      agent.getAgentMajorVersion() == 6;
    if (isIE6)
    {
      // send down the JS that we need to stretch the components
      XhtmlUtils.addLib(context, rc, IE6_SCRIPT.getScriptletKey());
    }
    
    writer.startElement("div", component);
    renderId(context, component);
    if (isIE6)
    {
      writer.writeAttribute("onresize", "TrPanelBorderLayoutResizeIE6(this)", null);
    }
    renderAllAttributes(context, rc, bean);
        
    UIComponent topFacet = getFacet(component, CorePanelBorderLayout.TOP_FACET);
    UIComponent bottomFacet = getFacet(component, CorePanelBorderLayout.BOTTOM_FACET);
    UIComponent leftFacet = getFacet(component, CorePanelBorderLayout.LEFT_FACET);
    UIComponent rightFacet = getFacet(component, CorePanelBorderLayout.RIGHT_FACET);
    UIComponent innerTopFacet = getFacet(component, CorePanelBorderLayout.INNER_TOP_FACET);
    UIComponent innerBottomFacet = getFacet(component, CorePanelBorderLayout.INNER_BOTTOM_FACET);
    UIComponent innerLeftFacet = getFacet(component, CorePanelBorderLayout.INNER_LEFT_FACET);
    UIComponent innerRightFacet = getFacet(component, CorePanelBorderLayout.INNER_RIGHT_FACET);
    
    // Don't make room for non-rendered facets
    if (topFacet != null && !topFacet.isRendered()) topFacet = null;
    if (bottomFacet != null && !bottomFacet.isRendered()) bottomFacet = null;
    if (innerTopFacet != null && !innerTopFacet.isRendered()) innerTopFacet = null;
    if (innerBottomFacet != null && !innerBottomFacet.isRendered()) innerBottomFacet = null;
    
    boolean useLR = leftFacet != null || rightFacet != null
      || innerLeftFacet != null || innerRightFacet != null;
    boolean ltr = !rc.isRightToLeft();
    
    if (!useLR)
    {
      leftFacet = getFacet(component, ltr ?
        CorePanelBorderLayout.START_FACET : CorePanelBorderLayout.END_FACET);
      innerLeftFacet = getFacet(component, ltr ?
        CorePanelBorderLayout.INNER_START_FACET : CorePanelBorderLayout.INNER_END_FACET);
      rightFacet = getFacet(component, ltr ?
        CorePanelBorderLayout.END_FACET : CorePanelBorderLayout.START_FACET);
      innerRightFacet = getFacet(component, ltr ?
        CorePanelBorderLayout.INNER_END_FACET : CorePanelBorderLayout.INNER_START_FACET);
    }
    // Don't make room for non-rendered facets
    if (innerLeftFacet != null && !innerLeftFacet.isRendered()) innerLeftFacet = null;
    if (innerRightFacet != null && !innerRightFacet.isRendered()) innerRightFacet = null;
    if (innerLeftFacet != null && !innerLeftFacet.isRendered()) innerLeftFacet = null;
    if (innerRightFacet != null && !innerRightFacet.isRendered()) innerRightFacet = null;
    
    String topHeight = (topFacet == null) ? "0px" : getTopHeight(bean);
    String innerTopHeight = (innerTopFacet == null) ? "0px" : getinnerTopHeight(bean);
    String bottomHeight = (bottomFacet == null) ? "0px" : getBottomHeight(bean);
    String innerBottomHeight = (innerBottomFacet == null) ? "0px" : getinnerBottomHeight(bean);
    String leftWidth = (leftFacet == null) ? "0px" :
      (useLR ? getLeftWidth(bean) : (ltr ? getStartWidth(bean) : getEndWidth(bean)));
    String rightWidth = (rightFacet == null) ? "0px" :
      (useLR ? getRightWidth(bean) : (ltr ? getEndWidth(bean) : getStartWidth(bean)));
    String innerLeftWidth = (innerLeftFacet == null) ? "0px" :
      (useLR ? getinnerLeftWidth(bean) :
        (ltr ? getStartInnerWidth(bean) : getEndInnerWidth(bean)));
    String innerRightWidth = (innerRightFacet == null) ? "0px" :
      (useLR ? getinnerRightWidth(bean) :
        (ltr ? getEndInnerWidth(bean) : getStartInnerWidth(bean)));
    
    String clientId = component.getClientId(context);
    
    _encodePositionedFacetGroup(context, rc, bean, writer, clientId, component,
      topFacet, topHeight,
      rightFacet, rightWidth,
      bottomFacet, bottomHeight,
      leftFacet, leftWidth,
      false, useLR, isIE6, ltr);
    
    writer.startElement("div", null);
    writer.writeAttribute("id", _createSubId(clientId, "center"), null);
    writer.writeAttribute("style", _buildPositionedCenterCss(
      topHeight, bottomHeight, leftWidth, rightWidth, isIE6), null);
    renderStyleClass(context, rc, SkinSelectors.AF_PANEL_BORDER_POSITIONED_CENTER_STYLE_CLASS);
    
    _encodePositionedFacetGroup(context, rc, bean, writer, clientId, component,
      innerTopFacet, innerTopHeight,
      innerRightFacet, innerRightWidth,
      innerBottomFacet, innerBottomHeight,
      innerLeftFacet, innerLeftWidth,
      true, useLR, isIE6, ltr);

    writer.startElement("div", null);
    writer.writeAttribute("id", _createSubId(clientId, "innerCenter"), null);
    writer.writeAttribute("style", _buildPositionedCenterCss(
      innerTopHeight, innerBottomHeight, innerLeftWidth, innerRightWidth, isIE6), null);
    renderStyleClass(context, rc, SkinSelectors.AF_PANEL_BORDER_POSITIONED_INNER_CENTER_STYLE_CLASS);

    encodeAllChildren(context, component);
    
    writer.endElement("div");
    writer.endElement("div");
    writer.endElement("div");
    
    if (isIE6)
    {
      // force the resize code to run at earliest moment
      writer.startElement(XhtmlConstants.SCRIPT_ELEMENT, null);
      renderScriptDeferAttribute(context, rc);
      renderScriptTypeAttribute(context, rc);
      writer.writeText(
        new StringBuilder(clientId.length() + 58)
          .append("TrPanelBorderLayoutResizeIE6(document.getElementById('")
          .append(clientId)
          .append("'));").toString(), null);
      writer.endElement(XhtmlConstants.SCRIPT_ELEMENT);
    }
  }
  
  private String _buildPositionedCenterCss(
    String topHeight,
    String bottomHeight,
    String leftWidth,
    String rightWidth,
    boolean isIE6)
  {
    return isIE6 ?
      new StringBuilder(10 + topHeight.length() + leftWidth.length())
        .append("top:") // 4
        .append(topHeight)
        .append(";left:") // 6
        .append(leftWidth)
        .toString()
      :
      new StringBuilder(25 + topHeight.length() + leftWidth.length() +
        rightWidth.length() + bottomHeight.length())
        .append("top:") // 4
        .append(topHeight)
        .append(";bottom:") // 8
        .append(bottomHeight)
        .append(";left:") // 6
        .append(leftWidth)
        .append(";right:") // 7
        .append(rightWidth)
        .toString();
  }
  
  private void _encodePositionedFacetGroup(
    FacesContext     context,
    RenderingContext rc,
    FacesBean        bean,
    ResponseWriter   writer,
    String           clientId,
    UIComponent      component,
    UIComponent      topFacet,
    String           topHeight,
    UIComponent      rightFacet,
    String           rightWidth,
    UIComponent      bottomFacet,
    String           bottomHeight,
    UIComponent      leftFacet,
    String           leftWidth,
    boolean          isInner,
    boolean          useLR,
    boolean          isIE6,
    boolean          ltr
    ) throws IOException
  {
    if (topFacet != null)
    {
      _encodePositionedHeightFacet(context, rc, topFacet, bean, writer, clientId, topHeight,
        isInner ? SkinSelectors.AF_PANEL_BORDER_POSITIONED_INNER_TOP_STYLE_CLASS :
          SkinSelectors.AF_PANEL_BORDER_POSITIONED_TOP_STYLE_CLASS,
        isInner, true, isIE6);
    }
    if (leftFacet != null)
    {
      _encodePositionedWidthFacet(context, rc, leftFacet, bean, writer, clientId, leftWidth,
        topHeight, bottomHeight,
        (isInner ?
          (ltr ? SkinSelectors.AF_PANEL_BORDER_POSITIONED_INNER_START_STYLE_CLASS :
            SkinSelectors.AF_PANEL_BORDER_POSITIONED_INNER_END_STYLE_CLASS)
          : (ltr ? SkinSelectors.AF_PANEL_BORDER_POSITIONED_START_STYLE_CLASS :
            SkinSelectors.AF_PANEL_BORDER_POSITIONED_END_STYLE_CLASS)),
        isInner, true, isIE6);
    }
    if (rightFacet != null)
    {
      _encodePositionedWidthFacet(context, rc, rightFacet, bean, writer, clientId, rightWidth,
        topHeight, bottomHeight,
        (isInner ?
          (ltr ? SkinSelectors.AF_PANEL_BORDER_POSITIONED_INNER_END_STYLE_CLASS :
            SkinSelectors.AF_PANEL_BORDER_POSITIONED_INNER_START_STYLE_CLASS)
          : (ltr ? SkinSelectors.AF_PANEL_BORDER_POSITIONED_END_STYLE_CLASS :
            SkinSelectors.AF_PANEL_BORDER_POSITIONED_START_STYLE_CLASS)),
        isInner, false, isIE6);
    }
    if (bottomFacet != null)
    {
      _encodePositionedHeightFacet(context, rc, bottomFacet, bean, writer, clientId, bottomHeight,
        isInner ? SkinSelectors.AF_PANEL_BORDER_POSITIONED_INNER_BOTTOM_STYLE_CLASS :
          SkinSelectors.AF_PANEL_BORDER_POSITIONED_BOTTOM_STYLE_CLASS,
        isInner, false, isIE6);
    }
  }
  
  private String _createSubId(
    String clientId,
    String name)
  {
    return new StringBuilder(clientId.length() + name.length() + 2)
      .append(clientId)
      .append("::")
      .append(name)
      .toString();
  }
  
  private void _encodePositionedHeightFacet(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    ResponseWriter   writer,
    String           clientId,
    String           cssHeight,
    String           cssClass,
    boolean          isInner,
    boolean          isTop,
    boolean          isIE6
    ) throws IOException
  {
    writer.startElement("div", null);
    writer.writeAttribute("id", _createSubId(clientId,
      isInner ? (isTop ? "innerTop" : "innerBottom") : (isTop ? "top" : "bottom")), null);
    renderStyleClass(context, rc, cssClass);
    String style = new StringBuilder(7 + cssHeight.length())
      .append("height:") // 7 chars
      .append(cssHeight)
      .toString();
    renderInlineStyleAttribute(context, rc, style);
    encodeChild(context, component);
    writer.endElement("div");
  }
  
  private void _encodePositionedWidthFacet(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean,
    ResponseWriter   writer,
    String           clientId,
    String           cssWidth,
    String           cssTopHeight,
    String           cssBottomHeight,
    String           cssClass,
    boolean          isInner,
    boolean          isLeft,
    boolean          isIE6
    ) throws IOException
  {
    writer.startElement("div", null);
    writer.writeAttribute("id", _createSubId(clientId,
      isInner ? (isLeft ? "innerLeft" : "innerRight") : (isLeft ? "left" : "right")), null);
    renderStyleClass(context, rc, cssClass);
    String style;
    
    if (isIE6)
    {
      style = new StringBuilder(11 + cssTopHeight.length() + cssWidth.length())
        .append("width:") // 6 chars
        .append(cssWidth)
        .append(";top:") // 5 chars
        .append(cssTopHeight)
        .toString();
    }
    else
    {
      style = new StringBuilder(19 + cssTopHeight.length() + cssBottomHeight.length() +
          cssWidth.length())
          .append("width:") // 6 chars
          .append(cssWidth)
          .append(";top:") // 5 chars
          .append(cssTopHeight)
          .append(";bottom:") // 8 chars
          .append(cssBottomHeight)
          .toString();
    }
    renderInlineStyleAttribute(context, rc, style);
    encodeChild(context, component);
    writer.endElement("div");
  }
  
  // Since IE6 cannot stretch components using docking, we are forced to use
  // JavaScript to do the dirty work
  private static final Scriptlet IE6_SCRIPT = new Scriptlet()
  {
    private static final String IE_JS_CODE =
      "function TrPanelBorderLayoutResizeIE6(elem)\n" +
      "{\n" +
      "  var id = elem.id;\n" +
      "  var innerElems = elem._trInnerElems;\n" +
      "  if (innerElems == null)\n" +
      "  {\n" +
      "    innerElems = elem._trInnerElems = new Array(\n" +
      "      document.getElementById(id + '::top'),\n" +
      "      document.getElementById(id + '::bottom'),\n" +
      "      document.getElementById(id + '::left'),\n" +
      "      document.getElementById(id + '::right'),\n" +
      "      document.getElementById(id + '::center'),\n" +
      "      document.getElementById(id + '::innerTop'),\n" +
      "      document.getElementById(id + '::innerBottom'),\n" +
      "      document.getElementById(id + '::innerLeft'),\n" +
      "      document.getElementById(id + '::innerRight'),\n" +
      "      document.getElementById(id + '::innerCenter'));\n" +
      "  }\n" +
      "  var topHeight = innerElems[0] ? innerElems[0].offsetHeight : 0;\n" +
      "  var bottomHeight = innerElems[1] ? innerElems[1].offsetHeight : 0;\n" +
      "  var leftWidth = innerElems[2] ? innerElems[2].offsetWidth : 0;\n" +
      "  var rightWidth = innerElems[3] ? innerElems[3].offsetWidth : 0;\n" +
      "  var center = innerElems[4];\n" +
      "  var innerCenter = innerElems[9];\n" +
      "  var width = elem.clientWidth;\n" +
      "  var height = elem.clientHeight;\n" +
      "  var centerHeight = (height - topHeight - bottomHeight);\n" +
      "  var centerWidth = (width - leftWidth - rightWidth);\n" +
      "  center.style.height = centerHeight + 'px';\n" +
      "  center.style.width = centerWidth + 'px';\n" +
      "  if (innerElems[2]) innerElems[2].style.height = centerHeight + 'px';\n" +
      "  if (innerElems[3]) innerElems[3].style.height = centerHeight + 'px';\n" +
      "  var innerTopHeight = innerElems[5] ? innerElems[5].offsetHeight : 0;\n" +
      "  var innerBottomHeight = innerElems[6] ? innerElems[6].offsetHeight : 0;\n" +
      "  var innerLeftWidth = innerElems[7] ? innerElems[7].offsetWidth : 0;\n" +
      "  var innerRightWidth = innerElems[8] ? innerElems[8].offsetWidth : 0;\n" +
      "  var innerCenterHeight = (centerHeight - innerTopHeight - innerBottomHeight);\n" +
      "  var innerCenterWidth = (centerWidth - innerLeftWidth - innerRightWidth);\n" +
      "  innerCenter.style.height = innerCenterHeight + 'px';\n" +
      "  innerCenter.style.width = innerCenterWidth + 'px';\n" +
      "  if (innerElems[7]) innerElems[7].style.height = innerCenterHeight + 'px';\n" +
      "  if (innerElems[8]) innerElems[8].style.height = innerCenterHeight + 'px';\n" +
      "}";
    
    @Override
    public Object getScriptletKey()
    {
      return "TrPanelBorderLayoutResizeIE6()";
    };
    
    @Override
    protected void outputScriptletContent(
      FacesContext context,
      RenderingContext rc
      ) throws IOException
    {
      context.getResponseWriter().writeText(IE_JS_CODE, null);
    };
  };
  
  static
  {
    XhtmlUtils.registerScriptlet(IE6_SCRIPT.getScriptletKey(), IE6_SCRIPT);
  };
}
