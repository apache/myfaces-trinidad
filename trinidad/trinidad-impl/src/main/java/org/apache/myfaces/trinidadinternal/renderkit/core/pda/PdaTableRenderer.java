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
package org.apache.myfaces.adfinternal.renderkit.core.pda;

import java.io.IOException;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.component.CollectionComponent;
import org.apache.myfaces.adf.component.UIXColumn;
import org.apache.myfaces.adf.component.UIXTable;
import org.apache.myfaces.adf.component.core.data.CoreColumn;
import org.apache.myfaces.adf.component.core.data.CoreTable;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;

import org.apache.myfaces.adfinternal.renderkit.core.xhtml.TableRenderer;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.XhtmlConstants;

import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.BandingData;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.ColumnData;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.RenderStage;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.RowData;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.TableRenderingContext;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.TableUtils;

import org.apache.myfaces.adfinternal.util.IntegerUtils;

public class PdaTableRenderer extends TableRenderer
{
  /**
   */
  public PdaTableRenderer()
  {
    super(CoreTable.TYPE);
  }
  
  protected final void renderControlBar(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext,
    UIComponent           component)
    throws IOException
  {
    boolean tableNotEmpty = !tContext.getRowData().isEmptyTable();
    boolean hasNav = tContext.hasNavigation() && tableNotEmpty;

    if (hasNav)
    {
      ResponseWriter writer = context.getResponseWriter();

      // start control bar row
      writer.startElement("tr", null);
      writer.startElement("td", null);

      // start control bar
      writer.startElement("div", null);
      renderStyleClass(context, arc,
                       XhtmlConstants.AF_TABLE_CONTROL_BAR_TOP_STYLE);

      if ( hasNav)
      {
        writer.startElement("div", null);
        /*
          if (arc.isRightToLeft())
            writer.writeAttribute("align", "left", null);
          else
            writer.writeAttribute("align", "right", null);
        */
        // =-=AEW Is "valign" even a real attr for divs???
        writer.writeAttribute("valign", "middle", null);
        delegateRenderer(context, arc, component,
                         getFacesBean(component), getSharedNavBarRenderer());
        
        writer.endElement("div");
      }

      writer.endElement("div");
      
      // end control bar row
      writer.endElement("td");
      writer.endElement("tr");
      
    }
  }

  protected void renderSubControlBar(
    FacesContext context,
    AdfRenderingContext arc,
    TableRenderingContext tContext,
    UIComponent component,
    boolean isUpper) throws IOException
  {
    // No-op.
  }

  private void _renderEmptyCell(
    FacesContext        context,
    AdfRenderingContext arc,
    TableRenderingContext tContext,
    boolean             isSelect,
    Object              emptyText) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("td", null);
    String cellClass = _getCellFormat(tContext, isSelect);
    renderStyleClass(context, arc, cellClass);

    if (emptyText == null)
      emptyText = XhtmlConstants.NBSP_STRING;
    writer.writeText(emptyText, null);
    
    // end the cell
    writer.endElement("td");
  }

  // render the actual table content, with headers
  protected void renderTableContent(
    FacesContext context,
    final AdfRenderingContext arc,
    final TableRenderingContext tContext,
    final UIComponent component) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    FacesBean bean = getFacesBean(component);
    //
    // no nested tables, so end the previous table and start a
    // new one
    //
    writer.endElement("table");

    
    //
    // start the content table with the same attributes as the title table
    //
    writer.startElement("table", component);
    renderTableAttributes(context, arc, component, bean,
                          "2", "1");

    //
    // write out the grid color as the border color for the grid lines
    // and border of the table
    //
    renderStyleClass(context, arc, XhtmlConstants.AF_TABLE_CONTENT_STYLE);

    //
    // 1. Gather all the data we need to render
    //
    
    final RowData rowData = tContext.getRowData();
    boolean isEmptyTable      = rowData.isEmptyTable();
    final UIComponent detail = tContext.getDetail();

    //
    // 2. Render the top / column header
    //
    
    // render the column header
    if (tContext.hasColumnHeaders())
      _renderColumnHeader(context, arc, tContext, component);

    //
    // 3. Render each row
    //
    
    ColumnData colData = tContext.getColumnData();
    final RenderStage renderStage = tContext.getRenderStage();
    
    renderStage.setStage(RenderStage.DATA_STAGE);
    
    
    // use the special response writer in our data section that
    // defaults data cells with no data to <br>
    //tContext.setDataResponseWriterUsed(true);
    int physicalCol = 0;
    
    if (isEmptyTable)
    {
      writer.startElement("tr", null);
      if (tContext.hasSelection())
      {
        colData.setColumnIndex(physicalCol++, ColumnData.SPECIAL_COLUMN_INDEX);
        _renderEmptyCell(context, arc, tContext, true, null);
      }
      
      // render detail control (hide/show for the row)
      if (detail != null)
      {
        colData.setColumnIndex(physicalCol++, ColumnData.SPECIAL_COLUMN_INDEX);
        _renderEmptyCell(context, arc, tContext, true, null);
      }
      
      int objectNameColumnIndex = colData.getObjectNameColumnIndex();
      if (objectNameColumnIndex < physicalCol)
        objectNameColumnIndex = physicalCol;
      for (int columns = colData.getColumnCount(); physicalCol < columns;)
      {
        colData.setColumnIndex(physicalCol, ColumnData.SPECIAL_COLUMN_INDEX);
        
        final Object emptyText;
        if (objectNameColumnIndex == physicalCol)
        {
          emptyText = getEmptyText(bean);
        }
        else
        {
          emptyText =  null;
        }
        
        _renderEmptyCell(context, arc, tContext, false, //isSelect
                         emptyText);
        physicalCol++;
      }
      writer.endElement("tr");
    }
    else //not an empty table
    {
      TableUtils.RowLoop loop = new TableUtils.RowLoop()
        {
          protected void processRowImpl(FacesContext fc, CollectionComponent tableBase)
            throws IOException
          {
            ResponseWriter rw = fc.getResponseWriter();
            // compute all the rowSpans for the current row:
            rowData.setCurrentRowSpan(-1); //reset
            renderStage.setStage(RenderStage.START_ROW_STAGE);
            renderSingleRow(fc, arc, tContext, component);
            renderStage.setStage(RenderStage.DATA_STAGE);
            // render each of the individual rows in the rowSpan:
            for(int i=0, sz=rowData.getCurrentRowSpan(); i<sz; i++)
            {
              // start the row
              rw.startElement("tr", null);
              renderSingleRow(fc, arc, tContext, component);
              rowData.incCurrentSubRow();
              // end the row
              rw.endElement("tr");
            }
            
            // if necessary, render a detail row
            if ((detail != null) &&
                ((UIXTable) tableBase).getDisclosedRowKeys().isContained())
            {
              renderStage.setStage(RenderStage.DETAIL_ROW_STAGE);
              
              rw.startElement("tr", null);
              rw.startElement("td", null);
              rw.writeAttribute("colspan",
                                IntegerUtils.getString(tContext.getActualColumnCount()),
                                null);
              
              // out.writeAttribute(CLASS_ATTRIBUTE, TABLE_DETAIL_STYLE);
              
              encodeChild(fc, detail);
              
              rw.endElement("td");
              rw.endElement("tr");
              
              // restore the data stage
              renderStage.setStage(RenderStage.DATA_STAGE);
            }
            
          }
        };
      loop.run(context, tContext.getCollectionComponent());
    }
    // render the column footer
    _renderColumnFooter(context,arc,tContext,component);
    
    // we're done with the defaulting data response writer
    //context.setDataResponseWriterUsed(false);
  }
  
     private void _renderColumnFooter(
       FacesContext          context,
       AdfRenderingContext   arc,
       TableRenderingContext tContext,
       UIComponent           component) throws IOException

     {
       tContext.getRenderStage().setStage(RenderStage.COLUMN_FOOTER_STAGE);
       final ColumnData colData = tContext.getColumnData();
       UIComponent footer = getFacet(component, CoreTable.FOOTER_FACET);
       if (footer != null)
       {
         ResponseWriter writer = context.getResponseWriter();
         writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
       /*  boolean useScroll = (getHeight(getFacesBean(component)) != null) && isIE(arc);
         if (useScroll)
         {
           writer.writeAttribute("style", "position:relative;"+
                                          "bottom:expression("+
                                           "this.offsetParent.scrollHeight-this.offsetParent.scrollTop-"+
                                           "this.offsetParent.clientHeight+1);" +
                                          "left:-1px", null);
         }
*/
         writer.startElement(XhtmlConstants.TABLE_HEADER_ELEMENT, null);
         // total rows may need an ID. see bug 3211593:
         /* Need new scheme for generateUniqueId()?
         String rowID = XhtmlLafUtils.generateUniqueID(tContext);
         writer.writeAttribute(XhtmlLafConstants.ID_ATTRIBUTE, rowID, null);
         tContext.getRowData().setCurrentRowHeaderID(rowID);
         */
         final int firstFooterPhysicalIndex = colData.getPhysicalIndexOfFirstFooter();
         final int colSpan = (firstFooterPhysicalIndex > 0)?  firstFooterPhysicalIndex: tContext.getActualColumnCount();
         writer.writeAttribute(XhtmlConstants.COLSPAN_ATTRIBUTE, IntegerUtils.getString(colSpan), null);
         renderStyleClass(context, arc, XhtmlConstants.AF_TABLE_COLUMN_FOOTER_STYLE);
         encodeChild(context, footer);
         writer.endElement(XhtmlConstants.TABLE_HEADER_ELEMENT);
         if (firstFooterPhysicalIndex > 0)
         {
           colData.setColumnIndex(tContext.getSpecialColumnCount(),
                                  0/*logicalColumnIndex*/);

           List children = component.getChildren();
           int count = children.size();

           for (int i = 0; i < count; i++)
           {
             UIComponent child = (UIComponent) children.get(i);
             if (child.isRendered())
               encodeChild(context, child);
           }
         }
         writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
       }
     }


  protected final void renderSingleRow(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext,
    UIComponent           component) throws IOException
  {
    int stage = tContext.getRenderStage().getStage();
    
    switch(stage)
    {
      case RenderStage.COLUMN_HEADER_STAGE:
        return;
      case RenderStage.INITIAL_STAGE:
      case RenderStage.DATA_STAGE:
      case RenderStage.END_STAGE:
      case RenderStage.START_ROW_STAGE:
        break;
      default:
        throw new AssertionError("Bad renderStage:"+stage);
    }
    
    ColumnData colData = tContext.getColumnData();
    int[] hidden = tContext.getHiddenColumns();
    int columns = tContext.getColumnCount();
    
    // render the special columns, such as selection and details:
    int physicalColumn = renderSpecialColumns(context,
                                              arc,
                                              tContext,
                                              component,
                                              0);
    
    for (int currCol = 0; currCol < columns; currCol++)
    {
      if (hidden[currCol] == TableRenderingContext.NORMAL_COLUMN)
      {
        UIXColumn column =
          (UIXColumn) component.getChildren().get(currCol);
        boolean isRowHeader = Boolean.TRUE.equals(
            column.getAttributes().get(CoreColumn.ROW_HEADER_KEY.getName()));
        if (!isRowHeader)
        {
          colData.setColumnIndex(physicalColumn,currCol);
          encodeChild(context, column);
          // ColumnBeans automatically increment the physical and logical
          // column indices (these may be increase by more than one, if
          // there are columnGroups). So we must not increment the column
          // indices here
          physicalColumn = colData.getPhysicalColumnIndex();
        }
      }
    }
    
  }
  


  // render the complete column header
  private void _renderColumnHeader(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext,
    UIComponent           component) throws IOException
  {
    tContext.getRenderStage().setStage(RenderStage.COLUMN_HEADER_STAGE);
    
    ResponseWriter writer = context.getResponseWriter();
    ColumnData colData = tContext.getColumnData();
    colData.setRowIndex(0);
    writer.startElement("tr", null);

    int physicalCol = renderSpecialColumns(context, arc, tContext, component, 0);
    int[] hidden = tContext.getHiddenColumns();
    int colCount = component.getChildCount();

    for (int j = 0; j < colCount; j++)
    {
      if (hidden[j] != TableRenderingContext.NORMAL_COLUMN)
        continue;
      CoreColumn column = (CoreColumn) component.getChildren().get(j);
      if (!column.isRowHeader())
      {
        colData.setColumnIndex(physicalCol, j);
        encodeChild(context, column);
        // ColumnBeans automatically increment the physical and logical
        // column indices (these may be increase by more than one, if
        // there are columnGroups). So we must not increment the column
        // indices here
        physicalCol = colData.getPhysicalColumnIndex();
      }
    }
    
    colData.setRowIndex(-1);
    
    writer.endElement("tr");
  }
  
  // get the style class for the current cell
  private String _getCellFormat(
    TableRenderingContext tContext,
    boolean               isSelect
    ) throws IOException
  {
    ColumnData colData = tContext.getColumnData();
    RowData rowData = tContext.getRowData();
    int row = rowData.getRangeIndex();
    int physicalColumn = colData.getPhysicalColumnIndex();
    int logicalColumn = colData.getLogicalColumnIndex();
    
    BandingData bandingData = tContext.getBanding();
    boolean band = bandingData.getBand(tContext, row,
                                       physicalColumn,
                                       logicalColumn);
    //
    // determine the cell class
    //
    String cellClass;
    if (band)
    {
      if (isSelect)
        cellClass = XhtmlConstants.TABLE_BAND_SELECT_CELL_STYLE;
      else
      {
        cellClass = ColumnData.selectFormat(tContext,
                             XhtmlConstants.AF_COLUMN_CELL_TEXT_BAND_STYLE,
                             XhtmlConstants.AF_COLUMN_CELL_NUMBER_BAND_STYLE,
                             XhtmlConstants.AF_COLUMN_CELL_ICON_BAND_STYLE);
      }
    }
    else
    {
      if (isSelect)
        cellClass = XhtmlConstants.TABLE_SELECT_CELL_STYLE;
      else
      {
        cellClass = ColumnData.selectFormat(tContext,
                             XhtmlConstants.AF_COLUMN_CELL_TEXT_STYLE,
                             XhtmlConstants.AF_COLUMN_CELL_NUMBER_STYLE,
                             XhtmlConstants.AF_COLUMN_CELL_ICON_FORMAT_STYLE);
      }
    }
    
    return cellClass;
  }
}
