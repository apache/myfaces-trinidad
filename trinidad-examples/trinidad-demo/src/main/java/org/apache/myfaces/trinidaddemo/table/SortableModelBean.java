/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidaddemo.table;

import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.myfaces.trinidad.model.SortableModel;


public class SortableModelBean
  extends SortableModel
{
  public SortableModelBean()
  {
    super(_createData());

    setComparator("size", new SizeComparator());
    setCaseSensitive(true);
  }

  private static List<FileInfo> _createData()
  {
    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");
    try
    {
      return Arrays.asList(
        new FileInfo("Documents", dateFormat.parse("20081123"),
          dateFormat.parse("20101015"), "folder", ""),
        new FileInfo("Music", dateFormat.parse("20081123"),
          dateFormat.parse("20101228"), "folder", ""),
        new FileInfo(".bashrc", dateFormat.parse("20081124"),
          dateFormat.parse("20090208"), "shell script", "90 bytes"),
        new FileInfo("Products.pdf", dateFormat.parse("20100416"),
          dateFormat.parse("20100416"), "PDF document", "1.2 MB"),
        new FileInfo("groceries.xls", dateFormat.parse("20101024"),
          dateFormat.parse("20101228"), "Excel spreadsheet", "38.0 KB"),
        new FileInfo("demo.mp3", dateFormat.parse("20072312"),
          dateFormat.parse("20090612"), "MP3 file", "5.4 MB"));
    }
    catch (ParseException e)
    {
      return Collections.emptyList();
    }
  }

  public final void setCaseSensitive(boolean caseSensitive)
  {
    _caseSensitive = caseSensitive;

    if (_caseSensitive)
    {
      setComparator("filename", null);
      setComparator("type", null);
    }
    else
    {
      setCollator("filename", Strength.TERTIARY, null);
      setCollator("type", Strength.TERTIARY, null);
    }
  }

  public final boolean isCaseSensitive()
  {
    return _caseSensitive;
  }

  public static class FileInfo
  {
    private FileInfo(
      String filename,
      Date   created,
      Date   modified,
      String type,
      String size)
    {
      this._filename = filename;
      this._created = created;
      this._type = type;
      this._size = size;
      this._modified = modified;
    }

    public final String getFilename()
    {
      return _filename;
    }

    public final String getType()
    {
      return _type;
    }

    public final String getSize()
    {
      return _size;
    }

    public final Date getModified()
    {
      return _modified;
    }

    public final Date getCreated()
    {
      return _created;
    }

    private final String _filename;
    private final String _type;
    private final String _size;
    private final Date _modified;
    private final Date _created;
  }

  private static final class SizeComparator
    implements Comparator<String>
  {
    @Override
    public int compare(
      String s1,
      String s2)
    {
      if (s1.equals(s2))
      {
        return 0;
      }
      else if (s1.length() == 0)
      {
        return -1;
      }
      else if (s2.length() == 0)
      {
        return 1;
      }

      Matcher m1 = _SIZE_PATTERN.matcher(s1);
      m1.find();

      Matcher m2 = _SIZE_PATTERN.matcher(s2);
      m2.find();

      String type1 = m1.group(2);
      String type2 = m2.group(2);

      if (type1.equals(type2))
      {
        double d1 = Double.parseDouble(m1.group(1));
        double d2 = Double.parseDouble(m2.group(1));

        return Double.compare(d1, d2);
      }
      else
      {
        if ("bytes".equals(type1))
        {
          return -1;
        }
        else if ("bytes".equals(type2))
        {
          return 1;
        }
        return type1.compareTo(type2);
      }
    }

    private final static Pattern _SIZE_PATTERN = Pattern.compile("(\\d+(?:\\.\\d+)?)\\s+(\\w+)");
  }

  private boolean _caseSensitive;
}
