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
package org.apache.myfaces.trinidadinternal.io;

import java.io.IOException;
import java.io.Writer;

/**
 * Utility class for escaping XML text.
 * <p>
 */
public class XMLEscapes
{
  static public void writeAttribute(
    Appendable   out,
    CharSequence text) throws IOException
  {
    int length = text.length();
    for (int i = 0; i < length; i++)
    {
      // =-=AEW It could be more efficient using append(CharSequence, int, int)
      // to write out large subsequences.  However, Writers don't seem to optimize
      // that method, so would it really be better?
      char ch = text.charAt(i);
      if (ch <= 0x7f)
      {
        if (ch == '>')
          out.append("&gt;");
        else if (ch == '<')
          out.append("&lt;");
        // AEW We probably shouldn't be doing this (unless we're
        // in an attribute)
        else if (ch == '"')
          out.append("&quot;");
        else if (ch == '&')
          out.append("&amp;");
        else
          out.append(ch);
      }
      else
      {
        _writeHexRef(out, ch);
      }
    }
    
  }
  
  
  // =-=AEW Performance - look at whether text and attributes
  // should be stored as character arrays or strings (might be
  // different decision for each), and make this class conform.

  static public void writeText(Writer out, char[] text)
    throws IOException
  {
    writeText(out, text, 0, text.length);
  }

  static public void writeText(
    Writer  out,
    char[]  text,
    int     start,
    int     length) throws IOException
  {
    int end = start + length;
    for (int i = start; i < end; i++)
    {
      char ch = text[i];

      if (ch <= 0x7f)
      {
        if (ch == '>')
          out.write("&gt;");
        else if (ch == '<')
          out.write("&lt;");
        // AEW We probably shouldn't be doing this (unless we're
        // in an attribute)
        else if (ch == '"')
          out.write("&quot;");
        else if (ch == '&')
          out.write("&amp;");
        else
        {
          out.write(ch);
        }
      }
      else
      {
        _writeHexRef(out, ch);
      }
    }
  }

  static public void writeAttribute(Writer out, char[] text)
    throws IOException
  {
    writeText(out, text);
  }

  static public void writeAttribute(
    Writer  out,
    char[]  text,
    int     start,
    int     length) throws IOException
  {
    writeText(out, text, start, length);
  }

  static private void _writeHexRef(
    Appendable  out,
    char        ch
    ) throws IOException
  {
    out.append("&#x");
    // =-=AEW Could easily be more efficient.
    out.append(Integer.toHexString(ch));
    out.append(';');
  }

  private XMLEscapes()
  {
  }
}
