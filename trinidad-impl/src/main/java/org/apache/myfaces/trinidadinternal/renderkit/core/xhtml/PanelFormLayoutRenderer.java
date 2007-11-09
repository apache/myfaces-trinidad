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

import java.awt.Dimension;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXEditableValue;
import org.apache.myfaces.trinidad.component.UIXGroup;
import org.apache.myfaces.trinidad.component.UIXPanel;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelFormLayout;
import org.apache.myfaces.trinidad.context.RenderingContext;

public class PanelFormLayoutRenderer extends XhtmlRenderer
{
  public PanelFormLayoutRenderer()
  {
    super(CorePanelFormLayout.TYPE);
  }
  
  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);

    _labelWidthKey = type.findKey("labelWidth");
    _fieldWidthKey = type.findKey("fieldWidth");
    _rowsKey = type.findKey("rows");
    _maxColumnsKey = type.findKey("maxColumns");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  /**
   * This is how we can render both the user defined styleClass and our
   * component style class.
   */
  @Override
  protected void renderStyleAttributes(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean) throws IOException
  {
    renderStyleAttributes(context, arc, bean, 
      SkinSelectors.AF_PANEL_FORM_STYLE_CLASS);
  }

  private Object _getLabelWidth(FacesBean bean)
  {
    return bean.getProperty(_labelWidthKey);
  }
  
  private Object _getFieldWidth(FacesBean bean)
  {
    return bean.getProperty(_fieldWidthKey);
  }
  
  private Number _getRows(FacesBean bean)
  {    
    return (Number)bean.getProperty(_rowsKey);
  }
  
  private Number _getMaxColumns(FacesBean bean)
  {    
    return (Number)bean.getProperty(_maxColumnsKey);
  }
 
  /**
   * Get the default number of columns
   */
  private int _getColumnsDefault(
    )
  {
    return _COLUMNS_DEFAULT;
  }

  private boolean _isFullRow(UIComponent component)
  {
    String rendererType = component.getRendererType();

    if (component instanceof UIXEditableValue)
    {
      return !_UNSUPPORTED_RENDERER_TYPES.contains(rendererType);
    }

    if (UIXPanel.COMPONENT_FAMILY.equals(component.getFamily()))
    {
      if ("org.apache.myfaces.trinidad.LabelAndMessage".equals(rendererType) ||
          "org.apache.myfaces.trinidad.rich.LabelAndMessage".equals(rendererType))
        return true;
      return false;
    }
    
    return false;
  }

  @Override
  protected String getDefaultStyleClass(FacesBean bean)
  {
    return SkinSelectors.AF_LABEL_TEXT_STYLE_CLASS;
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("div", component); // the root element
    renderId(context, component);
    renderAllAttributes(context, arc, bean);

    int maxColumns = 0;
    Number maxColumnsNumber = _getMaxColumns(bean);
    if (maxColumnsNumber != null)
    {
      maxColumns = maxColumnsNumber.intValue();
    }
    else
    {
      maxColumns = _getColumnsDefault();
    }

    int rows = 0;
    if (isPDA(arc))
    {
      maxColumns = 1;
    }
    Number rowsNumber = _getRows(bean);
    if (rowsNumber == null)
    {
      rows = Integer.MAX_VALUE;
    }
    else
    {
      rows = rowsNumber.intValue();
      if (rows < 1)
      {
        rows = Integer.MAX_VALUE;
      }
    }

    // Fetch a list of footer components:
    List<UIComponent> footerComponents = null;
    UIComponent footerFacetComponent = component.getFacet("footer");
    if (footerFacetComponent != null)
    {
      if (footerFacetComponent instanceof UIXGroup)
      {
        // a grouping of components
        if (footerFacetComponent.isRendered())
        {
          footerComponents = footerFacetComponent.getChildren();
        }
      }
      else
      {
        // a single component
        footerComponents = new ArrayList<UIComponent>();
        footerComponents.add(footerFacetComponent);
      }
    }

    _encodeChildren(context, arc, component, bean, footerComponents, maxColumns, rows);

    rw.endElement("div"); // the root element
  }
  
  @SuppressWarnings("unchecked")
  private void _encodeChildren(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean,
    List<UIComponent>   footerComponents,
    int                 maxColumns,
    int                 rows) throws IOException
  {
    // We cannot render a nested panelForm with any more than a single column
    // so we must monitor whether we are nested or not:
    Map<String, Object> requestMap = 
      context.getExternalContext().getRequestMap();
    
    Integer nestLevelObject = (Integer)requestMap.get(PANEL_FORM_NEST_LEVEL_KEY);
    int nestLevel = 0;
    if (nestLevelObject != null)
    {
      nestLevel = nestLevelObject.intValue() + 1;
    }
    requestMap.put(PANEL_FORM_NEST_LEVEL_KEY, nestLevel);

    // Iterate through the childPeers extracting and counting the number of
    // visible children, also count the visible children inside of visible
    // DhtmlGroupPeers:
    List<UIComponent> childComponents = component.getChildren();
    FormItemInfo visibleFormItemInfo = _extractVisibleItems(childComponents);
    List<FormItem> visibleFormItems = visibleFormItemInfo.getFormItems();
    int totalFormItemCount = visibleFormItemInfo.getTotalFormItemCount();
  
    // Iterate through the footerPeers extracting the visible children:
    int totalFooterItemCount = 0;
    List<FormItem> visibleFooterItems = null;
    if (footerComponents != null)
    {
      FormItemInfo visibleFooterItemInfo = _extractVisibleItems(footerComponents);
      visibleFooterItems = visibleFooterItemInfo.getFormItems();
      totalFooterItemCount = visibleFooterItemInfo.getTotalFormItemCount();
    }
  
    // Now that we have the list and counts of visible form items (and group
    // arrangements), we must figure out how many actual columns and actual rows
    // we really need:
    int actualColumns = maxColumns;
    int actualRows = rows;
    boolean startAlignedLabels = (nestLevel == 0);
    if ( !startAlignedLabels || (totalFormItemCount == 0) )
    {
      // Must use a single column and unlimited rows:
      actualColumns = 1;
      actualRows = Integer.MAX_VALUE;
    }
    else if (actualColumns == 1)
    {
      // Developer wanted to use a single column and unlimited rows:
      actualRows = Integer.MAX_VALUE;
    }
    else
    {
      // We must compute how many rows will fit in the given max number of columns
      // and also see if there are actually fewer columns needed:
      Dimension actualResults = PanelFormLayoutRenderer._computeActualRowsAndColumns(
        actualRows,
        actualColumns,
        totalFormItemCount,
        visibleFormItems);
      actualRows = (int)actualResults.getHeight();
      actualColumns = (int)actualResults.getWidth();
    }
    if (actualColumns < 1)
    {
      return;
    }

    // These widths can either be pixels, percentages, or undefined.
    // We must ensure that if using percentages or undefined that we correct them
    // to total up properly.
    String labelWidth = (String)_getLabelWidth(bean);
    String fieldWidth = (String)_getFieldWidth(bean);

    // Create the DOM for the form:
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("table", null); // the outer table
    OutputUtils.renderLayoutTableAttributes(context, arc, "0", null);

    String mainLabelWidth = null;
    String mainFieldWidth = null;
    String footerLabelWidth = null;
    String footerFieldWidth = null;
    if (startAlignedLabels)
    {
      FormWidths effectiveWidths =
        _computeEffectiveWidths(labelWidth, fieldWidth, actualColumns);
      mainLabelWidth = effectiveWidths.getMainLabelWidth();
      mainFieldWidth = effectiveWidths.getMainFieldWidth();
      footerLabelWidth = effectiveWidths.getFooterLabelWidth();
      footerFieldWidth = effectiveWidths.getFooterFieldWidth();
      rw.writeAttribute(
        "style", "width: " + effectiveWidths.getOverallWidth(), null);
    }
    else
    {
      rw.writeAttribute("style", "width: 100%", null);
    }

    rw.startElement("tbody", null); // the outer tbody

    // Create the form columns:
    _encodeFormColumns(
      context,
      arc,
      rw,
      startAlignedLabels,
      mainLabelWidth,
      mainFieldWidth,
      actualRows,
      actualColumns,
      1, // colSpan
      visibleFormItems);

    // Create the column-spanning footer row(s):
    if (totalFooterItemCount > 0)
    {
      _encodeFormColumns(
        context,
        arc,
        rw,
        startAlignedLabels,
        footerLabelWidth,
        footerFieldWidth,
        totalFooterItemCount, // row count
        1, // column count
        actualColumns, // this is actually colSpan
        visibleFooterItems);
    }

    // Indicate that we are leaving this level of nesting:
    if (nestLevel == 0)
    {
      // delete the value altogether:
      requestMap.remove(PANEL_FORM_NEST_LEVEL_KEY);
    }
    else
    {
      // decrement the value:
      requestMap.put(PANEL_FORM_NEST_LEVEL_KEY, nestLevel - 1);
    }

    rw.endElement("tbody"); // the outer tbody
    rw.endElement("table"); // the outer table
  }

  /**
   * Iterates through the childPeers extracting and counting the number of
   * visible children, also counts the visible children inside of visible
   * UIXGroups.
   * @param 
   */
  @SuppressWarnings("unchecked")
  private FormItemInfo _extractVisibleItems(List<UIComponent> children)
  {
    FormItemInfo formItemInfo = new FormItemInfo();
    int totalFormItemCount = 0;
    for (UIComponent child : children)
    {
      if (child.isRendered())
      {
        if (child instanceof UIXGroup)
        {
          // only count children of the group
          List<UIComponent> groupChildren = child.getChildren();
          int visibleChildrenCount = 0;
          for (UIComponent groupChild : groupChildren)
          {
            if (groupChild.isRendered())
            {
              // count the group child
              visibleChildrenCount++;
            }
          }
          if (visibleChildrenCount > 0)
          {
            totalFormItemCount += visibleChildrenCount;
            formItemInfo.add(child, visibleChildrenCount, true);
          }
        }
        else
        {
          // only count the child
          totalFormItemCount++;
          formItemInfo.add(child, 1, false);
        }
      }
    }
    formItemInfo.setTotalFormItemCount(totalFormItemCount);
    return formItemInfo;
  }

  private static Dimension _computeActualRowsAndColumns(
    int guessRows,
    int guessColumns,
    int totalFormItemCount,
    List<FormItem> visibleItems)
  {
    int actualRows = guessRows;
    int actualColumns = guessColumns;

    // Take a first guess at what the actualRows needs to be though we might
    // need to use a larger value.  For example, if rows = 4, maxColumns = 3,
    // totalFormItemCount = 11, and items 2 through 6 are in a group, then 3
    // columns and 4 rows are not enough!

    // first guess:
    // (note the page developer can specify a rows value higher than is actually
    // needed so we must take that into account)
    actualRows =
      (int)Math.max(actualRows, Math.ceil(
                         ((double) totalFormItemCount) / actualColumns));

    // test the first guess and keep trying more rows until it fits:
    int visibleItemsLength = visibleItems.size();
    boolean itemsWillFit = false;
    while (!itemsWillFit)
    {
      int currentItemIndex = 0;
      for (int col=0; (col<actualColumns && !itemsWillFit); col++)
      {
        int currentRow = 0;
        while (currentRow < actualRows)
        {
          FormItem item = visibleItems.get(currentItemIndex);
          int itemSize = item.getSize();
          if ( (currentRow == 0) || (currentRow + itemSize <= actualRows) )
          {
            // If the item or group of items is the first one in this column...
            //  - or -
            // If the item or group of items fit...

            // This particular item or group of items fits:
            currentItemIndex++;
            currentRow += itemSize;

            if (currentItemIndex >= visibleItemsLength)
            {
              // the items fit so we now know how many rows and columns we need:
              actualColumns = 1+col;
              itemsWillFit = true;
              break;
            }
          }
          else
          {
            break;
          }
        }
      }

      if (!itemsWillFit)
      {
        // didn't fit, so let's try more rows:
        actualRows++;
      }
    }

    return new Dimension(actualColumns, actualRows);
  }

  private FormWidths _computeEffectiveWidths(
    String labelWidth,
    String fieldWidth,
    int actualColumns)
  {
    String effectiveLabelWidth = null;
    String effectiveFieldWidth = null;
    String effectiveFooterLabelWidth = null;
    String effectiveFooterFieldWidth = null;
    String outerTableWidth = "100%";

    double labelRatio = 1;
    double fieldRatio = 1;

    if (labelWidth != null)
    {
      int percentCharIndex = labelWidth.indexOf("%");
      if (percentCharIndex == -1) // pixels
      {
        // Use the same number of pixels for the column labels as for the footer
        // labels:
        effectiveLabelWidth = labelWidth + "px";
        effectiveFooterLabelWidth = effectiveLabelWidth;
      }
      else // percentage
      {
        // Note the percentage ratio (it will be normalized later):
        labelRatio = Double.valueOf(labelWidth.substring(0, percentCharIndex));
      }
    }

    if (fieldWidth != null)
    {
      int percentCharIndex = fieldWidth.indexOf("%");
      if (percentCharIndex == -1) // pixels
      {
        effectiveFieldWidth = fieldWidth + "px";
      }
      else // percentage
      {
        // Note the percentage ratio (it will be normalized later):
        fieldRatio = Double.valueOf(fieldWidth.substring(0, percentCharIndex));
      }
    }

    if ( (labelWidth != null) && (effectiveLabelWidth == null) &&
      (fieldWidth != null) && (effectiveFieldWidth == null) )
    {
      // We are dealing with percentages for both the label and field.
      // Normalize the percentages (including the footer label width):
      double ratioTotal = (labelRatio + fieldRatio) / 100;
      double effectiveLabelWidthDouble = labelRatio / ratioTotal;
      effectiveLabelWidth =
        Math.floor(effectiveLabelWidthDouble) + "%";
      effectiveFieldWidth =
        Math.floor(fieldRatio / ratioTotal) + "%";
      int footerLabel = (int)Math.floor(effectiveLabelWidthDouble / actualColumns);
      int footerField = 100 - footerLabel;
      effectiveFooterLabelWidth = footerLabel + "%";
      effectiveFooterFieldWidth = footerField + "%";
    }
    else if ( (labelWidth != null) && (effectiveLabelWidth == null) )
    {
      // We are dealing with a percentage for the label and either pixels for the
      // field or an undefined field.
      // The field width is already computed.
      // Use the labelWidth percentage as specified:
      effectiveLabelWidth = labelWidth;

      // Compute the footer label % width:
      effectiveFooterLabelWidth =
        Math.floor(labelRatio / actualColumns) + "%";
    }
    else if ( (fieldWidth != null) && (effectiveFieldWidth == null) )
    {
      // We are dealing with a percentage for the field and either pixels for the
      // label or an undefined label.
      // The label width is already computed (so is the footer label width).
      // Use the fieldWidth percentage as specified:
      effectiveFieldWidth = fieldWidth;
    }
    else if ( (labelWidth != null) && (fieldWidth != null) )
    {
      // Pixels are used for both labels and fields:
      // This is important to note because the outer table must not be set to
      // a certain number of pixels.
      int labelPixels = Integer.valueOf(labelWidth);
      int fieldPixels = Integer.valueOf(fieldWidth);
      int outerTablePixels = (labelPixels + fieldPixels) * actualColumns;
      outerTableWidth = outerTablePixels + "px";
      effectiveFooterFieldWidth = (outerTablePixels - labelPixels) + "px";
    }
    else
    {
      // No percentages or pixels are used for either labels or fields:
      // This is important to note because the outer table must not be set to
      // stretch to 100% when this is the case.
      outerTableWidth = "auto";
    }

    return new FormWidths(
      effectiveLabelWidth,
      effectiveFieldWidth,
      effectiveFooterLabelWidth,
      effectiveFooterFieldWidth,
      outerTableWidth);
  }

  @SuppressWarnings("unchecked")
  private void _encodeFormColumns(
    FacesContext        context,
    RenderingContext    arc,
    ResponseWriter      rw,
    boolean             startAlignedLabels,
    String              effectiveLabelWidth,
    String              effectiveFieldWidth,
    int                 actualRows,
    int                 actualColumns,
    int                 colSpan,
    List<FormItem>      visibleItems) throws IOException
  {
    if (visibleItems.isEmpty())
      return;

    rw.startElement("tr", null); // the outer row
    int currentItemIndex = 0;
    int visibleItemsLength = visibleItems.size();
    String outerColumnWidth = Math.floor(((double) 100) / actualColumns) + "%";
    for (int col=0; col<actualColumns; col++)
    {
      rw.startElement("td", null); // the outer column
      renderStyleClass(context, arc,
        SkinSelectors.AF_PANEL_FORM_COLUMN_STYLE_CLASS);
      rw.writeAttribute("colspan", colSpan, null);
      if (col < actualColumns - 1) // let the last column take the leftover space
      {
        rw.writeAttribute("width", outerColumnWidth, null);
      }

      rw.startElement("table", null); // the inner table
      OutputUtils.renderLayoutTableAttributes(context, arc, "0", "100%");
      rw.startElement("tbody", null); // the inner tbody
      if (startAlignedLabels)
      {
        rw.startElement("tr", null); // the sizing row
        rw.startElement("td", null); // the sizing label cell
        if (effectiveLabelWidth != null)
        {
          rw.writeAttribute(
            "style", "width: " + effectiveLabelWidth, null);
        }
        rw.endElement("td"); // the sizing label cell
        rw.startElement("td", null); // the sizing field cell
        if (effectiveFieldWidth != null)
        {
          rw.writeAttribute(
            "style", "width: " + effectiveFieldWidth, null);
        }
        rw.endElement("td"); // the sizing field cell
        rw.endElement("tr"); // the sizing row
      }
      int currentRow = 0;
      boolean groupSeparatorNeeded = false;
      while (currentRow < actualRows)
      {
        FormItem item = visibleItems.get(currentItemIndex);
        UIComponent itemChild = item.getChild();
        int itemSize = item.getSize();
        boolean isGroup = item.isGroup();
        int sizeAfterThis = currentRow + itemSize;
        if ( (currentRow == 0) || (sizeAfterThis <= actualRows) )
        {
          if (isGroup)
          {
            if (currentRow > 0)
            {
              // insert group separator
              PanelFormLayoutRenderer._encodeGroupDivider(
                context, arc, rw, startAlignedLabels);
            }
            groupSeparatorNeeded = true;
            List<UIComponent> groupChildren = itemChild.getChildren();
            for (UIComponent groupChild : groupChildren)
            {
              if (groupChild.isRendered())
              {
                // add the group child
                _encodeFormItem(context, arc, rw, startAlignedLabels, groupChild);
              }
            }
          }
          else
          {
            if (groupSeparatorNeeded)
            {
              groupSeparatorNeeded = false;
              // insert group separator
              PanelFormLayoutRenderer._encodeGroupDivider(
                context, arc, rw, startAlignedLabels);
            }
            _encodeFormItem(context, arc, rw, startAlignedLabels, itemChild);
          }

          // This particular item or group of items fits:
          currentItemIndex++;
          currentRow += itemSize;
          
          if (currentItemIndex >= visibleItemsLength)
          {
            // there are no more items
            break;
          }
        }
        else
        {
          break;
        }
      }
      rw.endElement("tbody"); // the inner tbody
      rw.endElement("table"); // the inner table
      rw.endElement("td"); // the outer column
    }
    rw.endElement("tr"); // the outer row
  }

  private static void _encodeGroupDivider(
    FacesContext        context,
    RenderingContext arc,
    ResponseWriter      rw,
    boolean             startAlignedLabels) throws IOException
  {
    rw.startElement("tr", null);
    rw.startElement("td", null);
    if (startAlignedLabels)
    {
      rw.writeAttribute("colspan", "2", null);
    }
    // =-= mcc I considered using an HR but IE6 always adds its own border around
    //     any background graphics you attempt to put inside.  Firefox 1.5 behaves
    //     as expected.  Using a DIV until we know a way to fix IE6.
    rw.startElement("div", null);
    renderStyleClass(context, arc,
      SkinSelectors.AF_PANEL_FORM_SEPARATOR_STYLE_CLASS);
    rw.endElement("div");
    rw.endElement("td");
    rw.endElement("tr");
  }

  private void _encodeFormItem(
    FacesContext        context,
    RenderingContext arc,
    ResponseWriter      rw,
    boolean             startAlignedLabels,
    UIComponent         item) throws IOException
  {
    boolean isFullRow = _isFullRow(item);
    if (isFullRow) // "plays well" with panelForm
    {
      // If a peer wants to play well with panelForm, it must use the proper
      // PanelForm wrapper APIs to ensure proper DOM structure.
      _encodeBeforeLabelTd(context, arc, rw, startAlignedLabels);
      Map<String, String> originalResourceKeyMap = arc.getSkinResourceKeyMap();
      try
      {
        if (startAlignedLabels)
        {
          arc.setSkinResourceKeyMap(
            PanelFormLayoutRenderer._RESOURCE_KEY_SIDE_BY_SIDE_MAP);
        }
        else
        {
          arc.setSkinResourceKeyMap(
            PanelFormLayoutRenderer._RESOURCE_KEY_STACKED_MAP);
        }
        encodeChild(context, item);
      }
      finally
      {
        arc.setSkinResourceKeyMap(originalResourceKeyMap);
      }
      _encodeAfterFieldTd(rw, startAlignedLabels);
    }
    else // does not "play well" with panelForm
    {
      if (startAlignedLabels) // (labels side-by-side with fields)
      {
        rw.startElement("tr", null);

        rw.startElement("td", null); // label cell (empty)
        rw.endElement("td"); // label cell (empty)

        rw.startElement("td", null); // field cell (non-empty)
        renderStyleClass(context, arc,
          SkinSelectors.AF_PANEL_FORM_CONTENT_CELL_STYLE_CLASS);
        encodeChild(context, item);
        rw.endElement("td"); // field cell (non-empty)

        rw.endElement("tr");
      }
      else // top-aligned (labels stacked above fields)
      {
        rw.startElement("tr", null);

        rw.startElement("td", null); // field cell (non-empty)
        renderStyleClass(context, arc,
          SkinSelectors.AF_PANEL_FORM_CONTENT_CELL_STYLE_CLASS);
        encodeChild(context, item);
        rw.endElement("td"); // field cell (non-empty)

        rw.endElement("tr");
      }
    }
  }
  
  private static void _encodeBeforeLabelTd(
    FacesContext        context,
    RenderingContext arc,
    ResponseWriter      rw,
    boolean             startAlignedLabels) throws IOException
  {
    rw.startElement("tr", null); // form item row
    // startAlignedLabels means (labels side-by-side with fields)
    if (!startAlignedLabels) // top-aligned (labels stacked above fields)
    {
      rw.startElement("td", null); // stack cell
      rw.startElement("table", null); // inner table
      OutputUtils.renderLayoutTableAttributes(context, arc, "0", "100%");
      rw.startElement("tbody", null); // inner tbody

      rw.startElement("tr", null); // label row
    }
  }

  /**
   * The form children that support rendering inside of panelForms must call
   * this method between encoding their labels and fields.
   * @param context FacesContext
   * @param arc AdfRenderingContext
   * @param rw ResponseWriter
   * 
   * @return <code>true</code> if a new element was opened, <code>false</code>
   *         otherwise.
   *         
   * @throws IOException
   */
  @SuppressWarnings("unchecked")
  protected static boolean encodeBetweenLabelAndFieldCells(
    FacesContext        context,
    RenderingContext arc,
    ResponseWriter      rw) throws IOException
  {
    Map<String, Object> requestMap = 
      context.getExternalContext().getRequestMap();
    
    Integer nestLevelObject = (Integer)requestMap.get(PANEL_FORM_NEST_LEVEL_KEY);
    if ( (nestLevelObject != null) &&
      (nestLevelObject.intValue() > 0) ) // top-aligned (labels stacked above fields)
    {
      rw.endElement("tr"); // label row

      rw.startElement("tr", null); // field row
      return true;
    }
    
    return false;
  }

  private static void _encodeAfterFieldTd(
    ResponseWriter      rw,
    boolean             startAlignedLabels) throws IOException
  {
    if (!startAlignedLabels) // top-aligned (labels stacked above fields)
    {
      rw.endElement("tr"); // field row

      rw.endElement("tbody"); // inner tbody
      rw.endElement("table"); // inner table
      rw.endElement("td"); // stack cell
    }
    rw.endElement("tr"); // form item row
  }

  static private class FormWidths
  {
    FormWidths(
      String mainLabelWidth,
      String mainFieldWidth,
      String footerLabelWidth,
      String footerFieldWidth,
      String overallWidth)
    {
      _mainLabelWidth = mainLabelWidth;
      _mainFieldWidth = mainFieldWidth;
      _footerLabelWidth = footerLabelWidth;
      _footerFieldWidth = footerFieldWidth;
      _overallWidth = overallWidth;
    }

    String getMainLabelWidth()
    {
      return _mainLabelWidth;
    }

    String getMainFieldWidth()
    {
      return _mainFieldWidth;
    }

    String getFooterLabelWidth()
    {
      return _footerLabelWidth;
    }

    String getFooterFieldWidth()
    {
      return _footerFieldWidth;
    }

    String getOverallWidth()
    {
      return _overallWidth;
    }

    String _mainLabelWidth;
    String _mainFieldWidth;
    String _footerLabelWidth;
    String _footerFieldWidth;
    String _overallWidth;
  }

  static private class FormItem
  {
    FormItem(UIComponent child, int size, boolean group)
    {
      _child = child;
      _size = size;
      _group = group;
    }

    UIComponent getChild()
    {
      return _child;
    }

    int getSize()
    {
      return _size;
    }

    boolean isGroup()
    {
      return _group;
    }

    private UIComponent _child;
    private int _size;
    private boolean _group;
  }
  
  static private class FormItemInfo
  {
    FormItemInfo()
    {
      _formItems = new ArrayList<FormItem>();
    }

    void add(UIComponent child, int size, boolean group)
    {
      _formItems.add(new FormItem(child, size, group));
    }

    List<FormItem> getFormItems()
    {
      return _formItems;
    }

    int getTotalFormItemCount()
    {
      return _totalFormItemCount;
    }

    void setTotalFormItemCount(int totalFormItemCount)
    {
      _totalFormItemCount = totalFormItemCount;
    }

    private List<FormItem> _formItems;
    private int _totalFormItemCount;
  }

  private PropertyKey _labelWidthKey;
  private PropertyKey _fieldWidthKey;
  private PropertyKey _rowsKey;
  private PropertyKey _maxColumnsKey;

  // Overallocate because we basically want everything to miss
  private static final Set<String> _UNSUPPORTED_RENDERER_TYPES;
  static
  {
    _UNSUPPORTED_RENDERER_TYPES = new HashSet<String>(64);
    _UNSUPPORTED_RENDERER_TYPES.add("org.apache.myfaces.trinidad.Hidden");
    _UNSUPPORTED_RENDERER_TYPES.add("org.apache.myfaces.trinidad.Shuttle");
    _UNSUPPORTED_RENDERER_TYPES.add("org.apache.myfaces.trinidad.rich.Hidden");
    _UNSUPPORTED_RENDERER_TYPES.add("org.apache.myfaces.trinidad.rich.Shuttle");
  }

  private static final String PANEL_FORM_NEST_LEVEL_KEY =
    "org.apache.myfaces.trinidadinternal.PanelFormNestLevel";

  private static final int _COLUMNS_DEFAULT = 3;

  // we need a  resource key map since we are using LabelAndMessageRenderer.
  private static final Map<String, String> _RESOURCE_KEY_SIDE_BY_SIDE_MAP;
  private static final Map<String, String> _RESOURCE_KEY_STACKED_MAP;

  static
  {
    // style keys.
    // for panelForm, we want a specific af|panelFormLayout style for the label cell,
    // instead of the generic prompt cell style.

    // Start-aligned labels for side-by-side orientation:
    _RESOURCE_KEY_SIDE_BY_SIDE_MAP = new HashMap<String, String>();
    
    _RESOURCE_KEY_SIDE_BY_SIDE_MAP.put(
      SkinSelectors.AF_LABEL_TEXT_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_LABEL_CELL_STYLE_CLASS);
    _RESOURCE_KEY_SIDE_BY_SIDE_MAP.put(
      SkinSelectors.AF_CONTENT_CELL_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_CONTENT_CELL_STYLE_CLASS);
    _RESOURCE_KEY_SIDE_BY_SIDE_MAP.put(
      SkinSelectors.AF_COMPONENT_MESSAGE_CELL_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_MESSAGE_CELL_STYLE_CLASS);

    // Stacked labels for one-over-the-other orientation:
    _RESOURCE_KEY_STACKED_MAP = new HashMap<String, String>();
    
    _RESOURCE_KEY_STACKED_MAP.put(
      SkinSelectors.AF_LABEL_TEXT_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_LABEL_STACKED_CELL_STYLE_CLASS);
    _RESOURCE_KEY_STACKED_MAP.put(
      SkinSelectors.AF_CONTENT_CELL_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_CONTENT_CELL_STYLE_CLASS);
    _RESOURCE_KEY_STACKED_MAP.put(
      SkinSelectors.AF_COMPONENT_MESSAGE_CELL_STYLE_CLASS,
      SkinSelectors.AF_PANEL_FORM_MESSAGE_CELL_STYLE_CLASS);
  }
}
