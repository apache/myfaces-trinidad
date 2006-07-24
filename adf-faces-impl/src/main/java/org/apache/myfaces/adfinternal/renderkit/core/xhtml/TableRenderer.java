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
package org.apache.myfaces.adfinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.FacesEvent;

import org.apache.myfaces.adf.logging.ADFLogger;
import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.PropertyKey;
import org.apache.myfaces.adf.component.CollectionComponent;
import org.apache.myfaces.adf.component.TableUtils;
import org.apache.myfaces.adf.component.UIXCollection;
import org.apache.myfaces.adf.component.UIXColumn;
import org.apache.myfaces.adf.component.UIXTable;
import org.apache.myfaces.adf.component.core.data.CoreColumn;
import org.apache.myfaces.adf.component.core.data.CoreTable;
import org.apache.myfaces.adf.context.AdfFacesContext;
import org.apache.myfaces.adf.event.RowDisclosureEvent;
import org.apache.myfaces.adf.event.RangeChangeEvent;
import org.apache.myfaces.adf.event.SortEvent;
import org.apache.myfaces.adf.model.RowKeySet;
import org.apache.myfaces.adf.model.SortCriterion;

import org.apache.myfaces.adfinternal.agent.AdfFacesAgent;
import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.renderkit.core.CoreRenderer;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.CellUtils;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.ColumnData;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.DetailColumnRenderer;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.RenderStage;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.SelectionColumnRenderer;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.SpecialColumnRenderer;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.TableRenderingContext;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.TableSelectManyRenderer;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.TableSelectOneRenderer;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.table.TreeUtils;
import org.apache.myfaces.adfinternal.util.IntegerUtils;

abstract public class TableRenderer extends XhtmlRenderer
{
  public TableRenderer(FacesBean.Type type)
  {
    super(type);
    _resourceKeyMap = createResourceKeyMap();
  }

  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _navBarRenderer = new NavBar(type);
    _widthKey  = type.findKey("width");
    _emptyTextKey  = type.findKey("emptyText");
  }


  public boolean getRendersChildren()
  {
    return true;
  }


  public void decode(FacesContext context, UIComponent component)
  {
    decodeSelection(context, component);

    Map parameters =  context.getExternalContext().getRequestParameterMap();
    Object source = parameters.get(XhtmlConstants.SOURCE_PARAM);
    String id = component.getClientId(context);
    if (!id.equals(source))
      return;

    UIXTable table = (UIXTable) component;
    Object eventParam = parameters.get(XhtmlConstants.EVENT_PARAM);
    if (XhtmlConstants.GOTO_EVENT.equals(eventParam))
    {
      _decodeGoto(table, parameters);
    }
    else if (XhtmlConstants.HIDE_EVENT.equals(eventParam) ||
             XhtmlConstants.SHOW_EVENT.equals(eventParam))
    {
      _decodeHideShow(table, parameters, eventParam);
    }
    else if (XhtmlConstants.SORT_EVENT.equals(eventParam))
    {
      _decodeSort(table, parameters);
    }

    AdfFacesContext.getCurrentInstance().addPartialTarget(table);
  }

  protected final void decodeSelection(FacesContext context, UIComponent treeTable)
  {
    String selection = (String)
      treeTable.getAttributes().get(CoreTable.ROW_SELECTION_KEY.getName());
    if ("single".equals(selection))
      _selectOne.decode(context, treeTable);
    else if ("multiple".equals(selection))
      _selectMany.decode(context, treeTable);
  }

  public static RangeChangeEvent createRangeChangeEvent(CollectionComponent table,
                                                        int newStart)
  {
    int newEnd = TableUtils.getLast(table, newStart);

    return _createRangeChangeEvent(table, newStart, newEnd);
  }

  private static RangeChangeEvent _createRangeChangeEvent(
    CollectionComponent table,
    int newStart,
    int newEnd)
  {
    int oldStart = table.getFirst();
    int oldEnd = TableUtils.getLast(table) + 1;
    return
      new RangeChangeEvent((UIComponent) table, oldStart, oldEnd, newStart, newEnd);
  }
   private void _decodeSort(
    UIXTable table,
    Map parameters)
  {
    String property = (String) parameters.get(XhtmlConstants.VALUE_PARAM);
    Object state = parameters.get(XhtmlConstants.STATE_PARAM);
    boolean sortOrder = !XhtmlConstants.SORTABLE_ASCENDING.equals(state);
    SortCriterion criterion = new SortCriterion(property, sortOrder);

    SortEvent event =
      new SortEvent(table, Collections.singletonList(criterion));
    event.queue();
  }

  private void _decodeGoto(
    UIXTable table,
    Map parameters)
  {
    String value = (String) parameters.get(XhtmlConstants.VALUE_PARAM);
    if (value != null)
    {
      final FacesEvent event;
      if (XhtmlConstants.VALUE_SHOW_ALL.equals(value))
      {
        int newEnd = table.getRowCount();
        if (newEnd >= 0)
          event = _createRangeChangeEvent(table, 0, newEnd);
        else
          return;
      }
      else
      {
        int newStart = Integer.parseInt(value) - 1;
        event = createRangeChangeEvent(table, newStart);
      }

      event.queue();
      /* =-=AEW Don't set current value immediately - since that
           would mean that validate/updateModelValues run on the
           wrong rows!!!  Queue an event.
      System.out.println("DECODE: GOTO " + value);
      component.setAttribute("currentValue",
          new Integer(Integer.parseInt(value)));*/

      // I don't believe we want to skip to "renderResponse()" here,
      // since we want values to be applied!
      // context.renderResponse();
    }
  }


  private void _decodeHideShow(
    UIXTable table,
    Map parameters,
    Object eventParam)
  {
    boolean doExpand = XhtmlConstants.SHOW_EVENT.equals(eventParam);
    Object value = parameters.get(XhtmlConstants.VALUE_PARAM);
    if (value != null)
    {
      RowKeySet old = table.getDisclosedRowKeys();
      RowKeySet newset = old.clone();
      if ("all".equals(value))
      {
        if (doExpand)
          newset.addAll();
        else
          newset.removeAll();
        FacesEvent event = new RowDisclosureEvent(old, newset, table);
        event.queue();
      }
      else
      {
        int rowIndex = Integer.parseInt((String) value);
        int oldIndex = table.getRowIndex();
        table.setRowIndex(rowIndex);
        newset.setContained(doExpand);
        FacesEvent event = new RowDisclosureEvent(old, newset, table);
        event.queue();
        table.setRowIndex(oldIndex);
      }
    }
  }

  protected void encodeAll(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    // save current skin resource map, if any, on the local property
    Map oldSkinResourceMap = arc.getSkinResourceKeyMap();

    // store TableRenderer's skin resource map, so that called to
    // context.getTranslatedValue will get the correct key.
    arc.setSkinResourceKeyMap(_resourceKeyMap);

    TableRenderingContext tContext = createRenderingContext(context,
                                        arc,
                                        component);

    try
    {
      tContext.install();

      ResponseWriter rw = context.getResponseWriter();

      rw.startElement("div", component);
      renderId(context, component);
      renderAllAttributes(context, arc, bean);

      // If we need to render a "special" empty table, then bail.
      if (renderTableWithoutColumns(context, arc, tContext, component))
          return;

      // start the outer table:
      rw.startElement(XhtmlConstants.TABLE_ELEMENT, null);
      renderTableAttributes(context, arc, component, bean, "0", "0");

      RenderStage renderStage = tContext.getRenderStage();
      assert (renderStage.getStage()==RenderStage.INITIAL_STAGE);

      // give the table's columns a chance to initialize:
      renderSingleRow(context, arc, tContext, component);

      // 1. render the header bars (title, controlbar and subcontrolbar)
      renderNavigationHeaderBars(context, arc, tContext, component, bean);

      // 2. render the table content
      renderTableContent(context, arc, tContext, component);

      // end the outertable:
      rw.endElement(XhtmlConstants.TABLE_ELEMENT);


      // gives some beans the chance to cleanup:
      renderStage.setStage(RenderStage.END_STAGE);
      renderSingleRow(context, arc, tContext, component);

      String tid = tContext.getTableId();
      if (arc.getFormData() != null)
      {
        rw.startElement(XhtmlConstants.SCRIPT_ELEMENT, null);
        renderScriptDeferAttribute(context, arc);
        // Bug #3426092:
        // render the type="text/javascript" attribute in accessibility mode
        renderScriptTypeAttribute(context, arc);

        String formName = arc.getFormData().getName();

        rw.writeText(tContext.getJSVarName()+"="+
                     TreeUtils.createNewJSCollectionComponentState(formName, tid)+";", null);
        rw.endElement(XhtmlConstants.SCRIPT_ELEMENT);
      }

      int first = tContext.getCollectionComponent().getFirst();

      if (supportsScripting(arc))
      {
        XhtmlUtils.addLib(context, arc, "TableProxy()");

        // Bug #2378405: Add a javascript variable giving the row number of
        // the first row in the displayed rowset.

        // Although it seems like we should check for existence here (to
        // prevent duplication), we actually should not. If we have multiple
        // tables on a page, they will all need independent value fields.

        // We'd really like to use the flattened name here, but a colon doesn't
        // work as part of a javascript variable name, so we have to build up a
        // pseudo flattened name of the form _<tableName>_value.
        // (=-=AEW Change code to write the value directly into the window,
        // so a colon *would* work;  however, if a field had an id of "value"
        // in the table, we'd get a conflict;  so don't change?)

        // Also, since 1 is by far the most common value, don't bother
        // writing it out: the Javascript will assume the value is 1 if
        // it isn't.
        int value = first + 1;
        if (value != 1)
        {
          rw.startElement(XhtmlConstants.SCRIPT_NAME, null);
          renderScriptDeferAttribute(context, arc);
          // Bug #3426092:
          // render the type="text/javascript" attribute in accessibility mode
          renderScriptTypeAttribute(context, arc);
          rw.writeText("window[\"_", null);
          rw.writeText(tContext.getTableId(), null);
          rw.writeText(_VALUE_FIELD_NAME, null);
          rw.writeText("\"]=", null);
          rw.writeText(IntegerUtils.getString(value), null);
          rw.endElement(XhtmlConstants.SCRIPT_NAME);
        }
      }

      OutputUtils.renderHiddenField(context,
                                    tContext.getTableId() + ":rangeStart",
                                    IntegerUtils.getString(first));

      /* =-=AEW ENABLE IF WE REBUILD THE SAVE MODEL
      if (BodyRenderer.__isSaveModelActive(context) &&
          (tContext.getSelection() != null) &&
          !tContext.getRowData().isEmptyTable())
      {
        _writeNavExclude(tContext);
      }
      */

      rw.endElement("div");
    }
    finally
    {
      // restore current skin resource map. Most likely there won't be one.
      arc.setSkinResourceKeyMap(oldSkinResourceMap);

      if (tContext != null)
        tContext.release();
    }

  }


  /**
   * renders attributes on the outermost table element.
   * this includes width, cellpadding, cellspacing, border.
   */
  protected void renderTableAttributes(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent  component,
    FacesBean    bean,
    Object       cellPadding,
    Object       border
    ) throws IOException
  {
    Object width = getWidth(bean);

    OutputUtils.renderLayoutTableAttributes(context,
                                            arc,
                                            cellPadding,
                                            "0",    // cell spacing
                                            border,
                                            width); // table width
  }

  /**
   * Creates the correct subclass of the TableRenderingContext to
   * use for this Renderer.
   */
  protected TableRenderingContext createRenderingContext(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component
    )
  {
    return new TableRenderingContext(context, arc, component);
  }

  protected abstract void renderSingleRow(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext,
    UIComponent           component) throws IOException;


  /**
   * Render an empty table, if necessary.
   * @return true if the table was empty, and an alternative empty
   * version was shown, false otherwise.
   * @TODO COMPRESS JOINED STYLES
   */
  protected boolean renderTableWithoutColumns(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext,
    UIComponent           component) throws IOException
  {
    ColumnData colData = tContext.getColumnData();
    if (colData.getColumnCount() <= 0)
    {
      // see bug 2633464
      if (_LOG.isWarning())
        _LOG.warning("Table with id: "
                     + tContext.getTableId()
                     + " has no visible columns!");

      ResponseWriter writer = context.getResponseWriter();

      // render something so that the visual editor previewer will show a
      // simple <table/> tag:
      writer.startElement(XhtmlConstants.TABLE_ELEMENT, component);
      writer.startElement(XhtmlConstants.TABLE_ROW_ELEMENT, null);
      writer.startElement(XhtmlConstants.TABLE_DATA_ELEMENT, null);
      writer.writeAttribute(XhtmlConstants.WIDTH_ATTRIBUTE, "30", null);
      renderStyleClasses(context, arc,
                         new String[]{
                           XhtmlConstants.AF_COLUMN_CELL_TEXT_STYLE,
                           CellUtils.getBorderClass(
                               true,
                               true,
                               true,
                               true)});

      renderSpacer(context, arc, "30", "30");
      writer.endElement(XhtmlConstants.TABLE_DATA_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_ROW_ELEMENT);
      writer.endElement(XhtmlConstants.TABLE_ELEMENT);
      return true;
    }

    return false;
  }

  /**
   * used to render special column headers, like select and details.
   * @return the next physicalColumnIndex
   */
  protected int renderSpecialColumns(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext,
    UIComponent           treeTable,
    int                   physicalColumnIndex)
    throws IOException
  {
    // renders a whole bunch of <TH>...</TH> elements, or <TD>..</TD> elements
    // depending on the RenderStage
    final ColumnData colData = tContext.getColumnData();
    int[] hidden = tContext.getHiddenColumns();
    List children = treeTable.getChildren();
    int colCount  = children.size();
    for (int i = 0;  i < colCount;  i++)
    {
      if (hidden[i] != TableRenderingContext.NORMAL_COLUMN)
        continue;
      String selection = (String)
        treeTable.getAttributes().get(CoreTable.ROW_SELECTION_KEY.getName());

      UIXColumn column = (UIXColumn) children.get(i);
      boolean isRowHeader = Boolean.TRUE.equals(
            column.getAttributes().get(CoreColumn.ROW_HEADER_KEY.getName()));
      if (isRowHeader)
      {
        colData.setColumnIndex(physicalColumnIndex, i);
        encodeChild(context, column);
        // ColumnBeans automatically increment the physical and logical
        // column indices (these may be increase by more than one, if
        // there are columnGroups). So we must not increment the column
        // indices here
        physicalColumnIndex = colData.getPhysicalColumnIndex();
      }
      else
        break;
    }

    // special case... render the selection column
    if (tContext.hasSelection())
    {
      colData.setColumnIndex(physicalColumnIndex, ColumnData.SPECIAL_COLUMN_INDEX);

      _renderSelectionColumn(context, arc, tContext);
      physicalColumnIndex++;
    }
    // special case... render the detail column
    UIComponent detail = tContext.getDetail();
    if (detail != null)
    {
      colData.setColumnIndex(physicalColumnIndex, ColumnData.SPECIAL_COLUMN_INDEX);
      _renderDetailColumn(context, arc);

      physicalColumnIndex++;
    }

    return physicalColumnIndex;
  }

  private void _renderDetailColumn(
    FacesContext          context,
    AdfRenderingContext   arc) throws IOException
  {
    UIComponent column = _detailRenderer.getSpecialColumn();
    delegateRenderer(context, arc, column,
                     getFacesBean(column), _detailRenderer);
  }

  private void _renderSelectionColumn(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext) throws IOException
  {
    Map originalResourceKeyMap = arc.getSkinResourceKeyMap();
    setSelectionResourceKeyMap(arc, tContext);
    try
    {
      UIComponent column = _selectRenderer.getSpecialColumn();
      delegateRenderer(context, arc, column,
                       getFacesBean(column), _selectRenderer);
    }
    finally
    {
      arc.setSkinResourceKeyMap(originalResourceKeyMap);
    }
  }

  /**
   * Render the navigation header bars, i.e. all the bars that appear above the
   * actual data table. eg. title, controlbar and subcontrolbar
   */
  protected void renderNavigationHeaderBars(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext,
    UIComponent           component,
    FacesBean           bean) throws IOException
  {

    // 2. render the upper control bar - must render tableActions even
    // if table is empty
    _renderControlBar(context, arc, tContext, component, true); //isUpper

    //   render the sub control bar. we need to to this even if the table is empty
    // because we need to render the filter area. bug 3757395
    renderSubControlBar(context, arc, tContext, component, true);
  }

  /**
   * @todo Decide if we really want to support "repeating" regions
   */
  private void _renderControlBar(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext,
    UIComponent           component,
    boolean isUpper) throws IOException
  {
    // indicate that this is a repeating region, so that beans like the
    // choiceBean can stay synchronized
    arc.getProperties().put(XhtmlConstants.REPEAT_PROPERTY,
                         Boolean.TRUE);

    RenderStage rs = tContext.getRenderStage();
    rs.setStage(isUpper
                ? RenderStage.UPPER_CONTROL_BAR_STAGE
                : RenderStage.LOWER_CONTROL_BAR_STAGE);


    // Due to layout problems that occur when performing a partial
    // page replacement of a TableBean (2275703), we need to also perform
    // an explicit partial replacement of the upper navigation bar.  This
    // means that we need to generate a unique ID for the upper
    // navigation bar.  Since we may not actually have a TableRenderingContext
    // when fetching the navigation bar's ID (we may have a
    // PartialRenderingContext - which is another problem altOgether),
    // we just generate the ID here and store it away on the RenderingContext.
    if (isUpper)
    {
      // We only generate the navigation bar ID if the agent is IE
      // and partial rendering is enabled.
      Object id = tContext.getTableId();
      AdfFacesAgent agent = arc.getAgent();

      if ((agent.getAgentApplication() == AdfFacesAgent.APPLICATION_IEXPLORER) &&
          PartialPageUtils.isPPRActive(context))
      {
        String navBarID = id.toString() + "-nb";
        setRenderingProperty(arc, _UPPER_NAV_BAR_ID_PROPERTY, navBarID);
      }
    }


    renderControlBar(context, arc, tContext, component);

    // no longer a repeating region
    arc.getProperties().remove(XhtmlConstants.REPEAT_PROPERTY);

    if (isUpper)
      setRenderingProperty(arc, _UPPER_NAV_BAR_ID_PROPERTY, null);
  }


  /**
   * Renders the control bar
   */
  protected abstract void renderControlBar(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext,
    UIComponent           component)
    throws IOException;



  /**
   * Render sthe area with the filter and links, if necessary
   */
  protected abstract void renderSubControlBar(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext,
    UIComponent           component,
    boolean isUpper) throws IOException;





  /**
   * Renders the actual table content, with headers
   */
  protected abstract void renderTableContent(
    FacesContext          context,
    AdfRenderingContext   arc,
    TableRenderingContext tContext,
    UIComponent           component) throws IOException;

  protected String getEmptyText(FacesBean bean)
  {
    return toString(bean.getProperty(_emptyTextKey));
  }

  protected Object getWidth(FacesBean bean)
  {
    return bean.getProperty(_widthKey);
  }

  /**
   * Returns the shared UINode used to render detail hide/show
   */
  protected final CoreRenderer getSharedHideShowNode()
  {
    return null;
  }


  /**
   * Returns the shared Renderer used to render navbars
   */
  protected CoreRenderer getSharedNavBarRenderer()
  {
    return _navBarRenderer;
  }

  /**
   */
  public static String getRowHeaderFormatClass()
  {
    return XhtmlConstants.AF_COLUMN_ROW_HEADER_TEXT_STYLE;
  }

  /**
   * @param isColumnHeader true if the style for a column header is needed.
   * @todo Eliminate this method altogether;  row headers are static,
   *   and column headers should just call
   *     ColumnGroupRenderer.getHeaderStyleClass()
   */
  public static String getHeaderFormatClass(TableRenderingContext tContext,
                                            boolean isColumnHeader)
  {
    if (isColumnHeader)
      throw new IllegalStateException("Don't call this for column headers");

    return XhtmlConstants.AF_COLUMN_ROW_HEADER_TEXT_STYLE;
  }

  /**
   * Sets the skinResourceKeyMap on the RenderingContext with a map
   * which maps AF_COLUMN_CELL* styles to AF_TABLE_SELECT_MANY or
   * AF_TABLE_SELECT_ONE styles. We look at the selectionNode to figure
   * out if it is tableSelectOne or tableSelectMany
   * @todo Can this be private?
   * @todo reuse these Maps!
   */
  public static void setSelectionResourceKeyMap(
    AdfRenderingContext   arc,
    TableRenderingContext tContext)
  {
    if (tContext.hasSelection())
    {
      Map selectionColumnStylesMap = new HashMap();
      // if selection is multiple-selection:
      if (tContext.hasSelectAll())
      {
        selectionColumnStylesMap.put(XhtmlConstants.AF_COLUMN_CELL_ICON_FORMAT_STYLE,
                                XhtmlConstants.AF_TABLE_SELECT_MANY_CELL_ICON_FORMAT_STYLE);
        selectionColumnStylesMap.put(XhtmlConstants.AF_COLUMN_CELL_ICON_BAND_STYLE,
                                XhtmlConstants.AF_TABLE_SELECT_MANY_CELL_ICON_BAND_STYLE);
      }
      else
      {
        selectionColumnStylesMap.put(XhtmlConstants.AF_COLUMN_CELL_ICON_FORMAT_STYLE,
                                XhtmlConstants.AF_TABLE_SELECT_ONE_CELL_ICON_FORMAT_STYLE);
        selectionColumnStylesMap.put(XhtmlConstants.AF_COLUMN_CELL_ICON_BAND_STYLE,
                                XhtmlConstants.AF_TABLE_SELECT_ONE_CELL_ICON_BAND_STYLE);
      }
      arc.setSkinResourceKeyMap(selectionColumnStylesMap);
    }

  }

  protected boolean shouldRenderId(FacesContext context, UIComponent component)
  {
    return true;
  }

  protected Map createResourceKeyMap()
  {
    // map the skin resource keys that are used in components used
    // by the table renderer to table keys.
    // This way the table can be customized separately from other
    // components that it uses within it. For example, we can customize
    // af_table.DISCLOSED translation key
    // separately from af_showDetail.DISCLOSED.
    Map map = new HashMap(6);
    map.put("af_showDetail.DISCLOSED",
            "af_table.DISCLOSED");
    map.put("af_showDetail.UNDISCLOSED",
            "af_table.UNDISCLOSED");
    map.put("af_showDetail.DISCLOSED_TIP",
            "af_table.DISCLOSED_TIP");
    map.put("af_showDetail.UNDISCLOSED_TIP",
            "af_table.UNDISCLOSED_TIP");

    map.put(XhtmlConstants.AF_SHOW_DETAIL_DISCLOSED_ICON_NAME,
            XhtmlConstants.AF_TABLE_SD_DISCLOSED_ICON_NAME);
    map.put(XhtmlConstants.AF_SHOW_DETAIL_UNDISCLOSED_ICON_NAME,
            XhtmlConstants.AF_TABLE_SD_UNDISCLOSED_ICON_NAME);
    map.put(XhtmlConstants.AF_SELECT_RANGE_CHOICE_BAR_PREV_ICON_NAME,
            XhtmlConstants.AF_TABLE_NB_PREV_ICON_NAME);
    map.put(XhtmlConstants.AF_SELECT_RANGE_CHOICE_BAR_NEXT_ICON_NAME,
          XhtmlConstants.AF_TABLE_NB_NEXT_ICON_NAME);
    map.put(XhtmlConstants.AF_SELECT_RANGE_CHOICE_BAR_PREV_DISABLED_ICON_NAME,
          XhtmlConstants.AF_TABLE_NB_PREV_DISABLED_ICON_NAME);
    map.put(XhtmlConstants.AF_SELECT_RANGE_CHOICE_BAR_NEXT_DISABLED_ICON_NAME,
        XhtmlConstants.AF_TABLE_NB_NEXT_DISABLED_ICON_NAME);



    return Collections.unmodifiableMap(map);
  }

  static private class NavBar extends SelectRangeChoiceBarRenderer
  {
    public NavBar(FacesBean.Type type)
    {
      super(type);
    }

    protected void renderAllAttributes(
      FacesContext context, AdfRenderingContext arc, FacesBean bean)
    {
    }

    protected boolean getShowAll(FacesBean bean)
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      UIComponent component = tContext.getTable();
      if (component instanceof UIXTable)
      {
        UIXTable table = (UIXTable) component;
        return table.isShowAll();
      }

      return false;
    }

    // For now, disable showAll except on UIXTable
    protected boolean showAllSupported()
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      UIComponent component = tContext.getTable();
      return (component instanceof UIXTable);
      }


    protected String getSource()
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      return tContext.getTableId();
    }

    /**
     * @todo Deal with repeating!
     */
    protected String getClientId(FacesContext context, UIComponent component)
    {
      TableRenderingContext tContext =
        TableRenderingContext.getCurrentInstance();
      return tContext.getTableId() + "-nb";
    }

    protected String getVar(FacesBean bean)
    {
      return null;
    }

    // No support for range labels
    protected UIComponent getRangeLabel(UIComponent component)
    {
      return null;
    }

    protected int getRowCount(UIComponent component)
    {
      return ((CollectionComponent) component).getRowCount();
    }

    protected int getRowIndex(UIComponent component)
    {
      return ((CollectionComponent) component).getRowIndex();
    }

    protected void setRowIndex(UIComponent component, int index)
    {
      ((CollectionComponent) component).setRowIndex(index);
    }

    protected boolean isRowAvailable(UIComponent component)
    {
      return ((CollectionComponent) component).isRowAvailable();
    }

    protected boolean isRowAvailable(UIComponent component, int rowIndex)
    {
      return ((UIXCollection) component).isRowAvailable(rowIndex);
    }

    protected Object getRowData(UIComponent component)
    {
      return ((CollectionComponent) component).getRowData();
    }

    protected int getRows(UIComponent component, FacesBean bean)
    {
      return ((CollectionComponent) component).getRows();
    }

    protected int getFirst(UIComponent component, FacesBean bean)
    {
      return ((CollectionComponent) component).getFirst();
    }

  }

  private PropertyKey _widthKey;
  private PropertyKey _emptyTextKey;
  private final Map _resourceKeyMap;

  // Key for RenderingContext property used to store the generated ID
  // to use for the upper navigation bar.  (Part of fix for 2275703.)
  private static final Object _UPPER_NAV_BAR_ID_PROPERTY = new Object();

  private static final String _VALUE_FIELD_NAME      = "_value";

  private CoreRenderer _navBarRenderer;

  private final SpecialColumnRenderer _selectRenderer = new SelectionColumnRenderer();
  private final SpecialColumnRenderer _detailRenderer = new DetailColumnRenderer();
  private final CoreRenderer _selectOne = new TableSelectOneRenderer();
  private final CoreRenderer _selectMany = new TableSelectManyRenderer();

  private static final ADFLogger _LOG = ADFLogger.createADFLogger(TableRenderer.class);
}
