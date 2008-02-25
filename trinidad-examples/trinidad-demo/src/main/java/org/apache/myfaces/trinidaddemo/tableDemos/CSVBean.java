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
package org.apache.myfaces.trinidaddemo.tableDemos;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.ValueHolder;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.trinidad.component.UIXColumn;
import org.apache.myfaces.trinidad.component.UIXTable;


/**
 */
public class CSVBean
{
  public void sendContent(FacesContext context, OutputStream out)
    throws IOException
  {
    Writer outw = new OutputStreamWriter(out, "UTF-8");
    UIXTable table = getTable();
    Object rowKey = table.getRowKey();
    for (int i = 0; i < table.getRowCount(); i++)
    {
      table.setRowIndex(i);
      if (!table.isRowAvailable())
        break;
      _exportRow(table, outw);
    }

    outw.flush();
    table.setRowKey(rowKey);
  }

  
  private void _exportRow(UIXTable table, Writer out) throws IOException
  {
    List children = table.getChildren();
    boolean needsComma = false;
    for (Object o : children)
    {
      UIComponent child = (UIComponent) o;
      if (!child.isRendered())
        continue;

      if (needsComma)
        out.write(',');
      
      if (_exportCell(child, out))
        needsComma = true;
    }
    
    out.write('\n');
  }

  private boolean _exportCell(UIComponent cell, Writer out) throws IOException
  {
    boolean foundValue = false;
    if (cell instanceof ValueHolder)
    {
      foundValue = true;
      Object value = ((ValueHolder)cell).getValue();
      if (value != null)
      {
        String valueStr;
        Converter converter = ((ValueHolder) cell).getConverter();

        // TODO: add default by-type converter support
        if (converter != null)
        {
          try
          {
            valueStr = converter.getAsString(FacesContext.getCurrentInstance(),
                                             cell,
                                             value);
          }
          catch (ConverterException ce)
          {
            valueStr = value.toString();
          }
        }
        else
        {
          valueStr = value.toString();
        }

        if (valueStr != null)
        {
          out.write('"');
          out.write(_csvEscape(valueStr));
          out.write('"');
        }
      }
    }

  
    if (cell.getChildCount() > 0)
    {
      List children = cell.getChildren();
      for (Object o : children)
      {
        UIComponent child = (UIComponent) o;
        if (!child.isRendered())
          continue;
        
        if (_exportCell(child, out))
          foundValue = true;
      }
    }

    return foundValue;
  }


  public UIXTable getTable()
  {
    return _table;
  }

  public void setTable(UIXTable table)
  {
    _table = table;
  }

  private String _csvEscape(String value)
  {
    // we put double-quotes aroudn everything,
    // but then the quotes need to be escaped as pairs of quotes
    if (value.indexOf('"') >= 0)
    {
      return value.replace("\\\"", "\\\"\\\"");
    }

    return value;
  }

  private UIXTable _table;
}
