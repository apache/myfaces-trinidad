/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfinternal.agent.parse;

import java.util.Iterator;


/**
 * A parsed version string
 */
public class VersionId
{

  public VersionId(String version)
  {
    if (version == null)
      return;
    _versionString = version;
    _version = _parseVersion(version);
  }

  public String toString()
  {
    return _versionString;
  }

  public double getVersion()
  {
    double version = 0;
    if (_versionString != null)
    {
      boolean hasDecimal = false;
      double divisor = 10.0;

      int sourceLength = _versionString.length();
      int currIndex = 0;

      while (currIndex < sourceLength)
      {
        char currChar = _versionString.charAt(currIndex);
        if ((currChar >= '0') && (currChar <= '9'))
        {
          double addValue = (double) (currChar - '0');
          if (hasDecimal)
          {
            // handle digits to right of decimal
            addValue /= divisor;
            divisor = divisor * 10.0;
          }
          else
          {
            // handle digits to left of decimal
            version *= 10.0;
          }
          version += addValue;
        }
        else
        {

          if (currChar == '.')
          {
            // found decimal place
            hasDecimal = true;
          }
          else
          {
            break;
          }
        }
        // read next char
        currIndex++;
      }
    }
    return version;
  }


  public Iterator iterator()
  {
    return new VIterator();
  }

  private Entry _parseVersion(String value)
  {

    int start = 0;
    int length = value.length();
    char[] data = new char[length + 1];
    int i = 0;

    value.getChars(0, length, data, 0);
    data[length] = (char) 0x1000000; //untype-able ascii char;
    char ch = data[i];

    Entry head, curr;
    head = curr = new Entry();
    curr.next = curr;

    while (i < length + 1)
    {
      //Ignore spaces. TODO: This should happen only for start and end (trim).
      while (Character.isSpace(ch))
        ch = data[++i];

      switch (ch)
      {
        case '.':
          if (start != i)
          {
            curr.next.text = new String(data, start, i - start);
            curr = curr.next;
            curr.next = new Entry();
          }
          ch = data[++i];
          start = i;
          break;
        case ((char) 0x1000000):
          if (start != i)
          {
            curr.next.text = new String(data, start, i - start);
            curr = curr.next;
            curr.next = new Entry();
          }
          i++;
          break;
        default:
          ch = data[++i];
      }
    }

    curr.next = null;
    return head;
  }

  private class Entry
  {
    String text;
    Entry next;
  }

  private class VIterator implements Iterator
  {
    public VIterator()
    {
      _current = _version;
    }

    public void remove()
    {
      throw new UnsupportedOperationException();
    }

    public boolean hasNext()
    {
      return (null != _current);
    }

    public Object next()
    {
      String part = _current.text;
      _current = _current.next;
      return part;
    }

    private Entry _current;
  }

  private String _versionString;
  private Entry _version;
}

