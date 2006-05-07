/*
 * Copyright 2004,2006 The Apache Software Foundation.
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

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.context.MockExternalContext;
import javax.faces.event.FacesEvent;

import junit.textui.TestRunner;

import org.apache.myfaces.adf.component.core.data.CoreTable;
import org.apache.myfaces.adf.event.SelectionEvent;
import org.apache.myfaces.adf.model.RowKeySet;
import org.apache.myfaces.adf.model.RowKeySetImpl;

import org.apache.myfaces.adfbuild.test.FacesTestCase;
import org.apache.myfaces.adfbuild.test.MockFContext;


/**
 * @author Arjuna Wijeyekoon
 */
public class TableSelectOneRendererTest extends FacesTestCase
{

  public TableSelectOneRendererTest(String testName)
  {
    super(testName);
  }

  public static void main(String[] args)
  {
    TestRunner.run(TableSelectOneRendererTest.class);
    try
    {
      //new TreeStateTest("test").testExpandAllDepth();
      //new TreeStateTest("test").testSerialization();
    }
    catch (Throwable e)
    {
      e.printStackTrace();
    }
  }

  /**
   * if there are no selection parameters on the request, then the current
   * selectedIndex should not change:
   */
  public void testDecodeNothing()
  {
    new MockFContext();

    CoreTable table = _createComponent();
    _doDecode(table, -1);
    _testSelection(table, _INIT_SELECTION);
    MockFContext.clearContext();
  }

  /**
   * if there is a new selectedIndex on the request, then it must be set on
   * the component:
   */
  public void testDecodeSelected()
  {
    final int selectedIndex = 4;
    new MockFContext();

    TestTable table = (TestTable) _createComponent();
    _doDecode(table, selectedIndex);

    SelectionEvent event = (SelectionEvent) table.event;

    assertNotNull(event);

    RowKeySet unselect = event.getRemovedSet();
    RowKeySet select = event.getAddedSet();
    int oldIndex = table.getRowIndex();

    table.setRowIndex(_INIT_SELECTION);
    assertTrue(unselect.isContained());
    assertFalse(select.isContained());

    table.setRowIndex(selectedIndex);
    assertFalse(unselect.isContained());
    assertTrue(select.isContained());

    table.setRowIndex(oldIndex);
    _testSelection(table, selectedIndex);
    MockFContext.clearContext();
  }

  private CoreTable _createComponent()
  {
    String[] data = {"1", "2", "3", "4", "5", "6", "7", "8", "9"};
    CoreTable table = new TestTable();
    table.setId(_TABLE_ID);
    table.setValue(data);
    table.setRowIndex(_INIT_SELECTION);
    table.getSelectedRowKeys().add();
    table.setRowIndex(-1);
    table.setRowSelection("single");
    return table;
  }

  private void _testSelection(CoreTable table,
                              int expectedSelectedIndex)
  {
    table.setRowIndex(expectedSelectedIndex);
    RowKeySet state = table.getSelectedRowKeys();
    assertTrue("row is selected", state.isContained());
    Iterator selection = state.iterator();
    // make sure there is exactly one selected item:
    assertTrue("has one selected item", selection.hasNext());
    selection.next();
    assertFalse("has one selected item", selection.hasNext());
  }

  private void _doDecode(CoreTable table,
                         int selectedIndex)
  {
    MockFContext context = (MockFContext) FacesContext.getCurrentInstance();
    context.setupGetViewRoot(new UIViewRoot());
    MockExternalContext external =
      (MockExternalContext) context.getExternalContext();

    if (selectedIndex >= 0)
    {
      int oldIndex = table.getRowIndex();
      table.setRowIndex(selectedIndex);
      String selectedParam = table.getCurrencyString();
      table.setRowIndex(oldIndex);

      Map requestParams = new HashMap(2);
      String selectionParam =
        TableSelectOneRenderer.__getSelectionParameterName(context, table);
      requestParams.put(selectionParam, selectedParam);
      external.setupGetRequestParameterMap(requestParams);
    }
    else
    {
      external.setupGetRequestParameterMap(Collections.EMPTY_MAP);
    }


    TableSelectOneRenderer renderer = new TableSelectOneRenderer();
    renderer.decode(context, table);

    external.verify();
    context.verify();
  }

  private static class TestTable extends CoreTable
  {
    public TestTable()
    {
      super();
      setSelectedRowKeys(new RowKeySetImpl());
      setDisclosedRowKeys(new RowKeySetImpl());
    }

    public void queueEvent(FacesEvent event)
    {
      this.event = event;
    }

    public FacesEvent event = null;
  }

  private static final String _TABLE_ID = "table1";
  private static final int _INIT_SELECTION = 3;
}
