/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.core.xhtml.table;

import org.apache.myfaces.adf.component.CollectionComponent;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/table/RowData.java#0 $) $Date: 10-nov-2005.19:02:36 $
 * @author The Oracle ADF Faces Team
 */
public final class RowData
{

  public RowData(TableRenderingContext tContext)
  {
    CollectionComponent table = tContext.getCollectionComponent();
    _tableBase = table;
    _rowCount = TableUtils.getVisibleRowCount(table);
  }

  /**
   * gets the index of the current row, as an index in the currently visible
   * range.
   */
  public int getRangeIndex()
  {
    int first  = _tableBase.getFirst() + 1;
    if (first <= 0)
      first = 1;
    return _tableBase.getRowIndex() - first + 1;
  }

  /**
   * @return the number of visible table rows
   */
  public int getVisibleRowCount()
  {
    return _rowCount;
  }

  /**
   * Return true if this is a special "empty" table with no rendered data rows.
   */
  public boolean isEmptyTable()
  {
    return getVisibleRowCount() <= 0;
  }

  /**
   * sets the rowHeader ID for the current row.
   */
  public void setCurrentRowHeaderID(String id)
  {
    _currRowHeaderID = id;
  }

  /**
   * gets the rowHeader ID for the current row. This ID must be part of the
   * headers attribute for each table cell on the current row.  */
  public String getCurrentRowHeaderID()
  {
    return _currRowHeaderID;
  }

  /**
   * gets the max row span for the current row.
   */
  public int getCurrentRowSpan()
  {
      // indicate that we have read the rowspan:
      _currentRowSpanState = 2;
    return _currRowSpan;
  }

  /**
   * sets the max row span for the current row.
   * @param rowSpan use -1 to reset between rows.
   */
  public void setCurrentRowSpan(int rowSpan)
  {

	boolean assertEnabled = false;
	assert assertEnabled = true;

    if (rowSpan < 0)
    {
      // preform a reset
      _currRowSpan = 1;
      _currSpanRow = 0;


      if (assertEnabled)
      {
        // make sure prev operation was get:
        assert (_currentRowSpanState == 2);
        _currentRowSpanState = 0; // indicate that we have reset the rowspan
      }
    }
    else
    {
      if (rowSpan > _currRowSpan)
        _currRowSpan = rowSpan;

      if (assertEnabled)
      {
        // make sure that the previous operation was a set or reset:
        assert (_currentRowSpanState <= 1);

        // indicate that we have set the rowSpan:
        _currentRowSpanState = 1;
      }
    }
  }

  /**
   * gets the current sub row index for the current row. This is useful only
   * if the current row has a rowSpan that is greater than one
   */
  public int getCurrentSubRow()
  {
      // make sure that the sub-row number we are at is smaller than the
      // rowSpan of the current row:
      assert (_currSpanRow < _currRowSpan);
       return _currSpanRow;
  }

  /**
   * increments the current sub row index by one.
   */
  public void incCurrentSubRow()
  {
    _currSpanRow++;
  }

  private String _currRowHeaderID = null;
  private final int _rowCount;
  private int _currRowSpan = 1, _currSpanRow = 0;

  private final CollectionComponent _tableBase;

  // the following constants are used in Assert.DEBUG mode only:

  // currentRowSpanState
  // Get = G = 2
  // Set = S = 1
  // Reset=R = 0
  // FSM:
  //      GSR
  //     G101
  //     S110
  //     R110
  private byte _currentRowSpanState = 2; // initialize to get

}
