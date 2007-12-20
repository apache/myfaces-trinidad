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
 * Utility class for escaping HTML text.
 * <p>
 */
public class HTMLEscapes
{
  /**
   * Returns an escaped String of text
   */
  public static String escapeText(
    String text
    )
  {
    int length = text.length();

    int escapedIndex = _getEscapedIndex(text);

    if (escapedIndex == length)
    {
      // no escaping necessary
      return text;
    }

    //
    // copy the text up to here in a buffer and escape the rest
    //
    char[] buffer = new char[length * 8];

    int buffIndex = 0;

    for (; buffIndex < escapedIndex; buffIndex++)
    {
      buffer[buffIndex] = text.charAt(buffIndex);
    }


    for (int i = escapedIndex; i < length; i++)
    {
      char ch = text.charAt(i);

      if (ch < 0xA0)
      {
        // If "?" or over...
        if (ch >= 0x3f)
        {
          buffer[buffIndex++] = ch;
        }
        // If "'" or over...
        else if (ch >= 0x27)
        {
          if (ch < 0x3c)
          {
            buffer[buffIndex++] = ch;
          }
          else if (ch == '<')
          {
            // =-= bts argh!! "<" isn't supposed to be escaped in attributes,
            //         as per HTML spec
            buffIndex = _addStringToBuffer(buffer, buffIndex, "&lt;");
          }
          else if (ch == '>')
          {
            buffIndex = _addStringToBuffer(buffer, buffIndex, "&gt;");
          }
          else
          {
            buffer[buffIndex++] = ch;
          }
        }
        else
        {
          if (ch == '&')
          {
            // HTML 4.0, section B.7.1: ampersands followed by
            // an open brace don't get escaped
            if ((i + 1 < length) &&
                (text.charAt(i + 1) == '{'))
            {
              buffer[buffIndex++] = ch;
            }
            else
            {
              buffIndex = _addStringToBuffer(buffer, buffIndex, "&amp;");
            }
          }
          else if (ch == '"')
          {
            buffIndex = _addStringToBuffer(buffer, buffIndex, "&quot;");
          }
          else
          {
            buffer[buffIndex++] = ch;
          }
        }
      }
      else
      {
        buffIndex = _addDecRefToBuffer(buffer, buffIndex, ch);
      }
    }

    return new String(buffer, 0, buffIndex);
  }

  // =-=AEW Performance - look at whether text and attributes
  // should be stored as character arrays or strings (might be
  // different decision for each), and make this class conform.

  static public void writeText(
    Writer out,
    char[]      buffer,
    char[]      text
    )
    throws IOException
  {
    writeText(out, buffer, text, 0, text.length);
  }



  /**
   * Write char array text.  Note that this code is duplicated below
   * for Strings - change both places if you make any changes!!!
   */
  static public void writeText(
    Writer out,
    char[]      buff,
    char[]      text,
    int         start,
    int         length
    ) throws IOException
  {
    int buffLength = buff.length;
    int buffIndex = 0;

    int end = start + length;
    for (int i = start; i < end; i++)
    {
      char ch = text[i];

      if (ch < 0xA0)
      {
        // If "?" or over...
        if (ch >= 0x3f)
        {
          buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
        }
        // If "'" or over...
        else if (ch >= 0x27)
        {
          if (ch < 0x3c)
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          }
          else if (ch == '<')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            out.write("&lt;");
          }
          else if (ch == '>')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            out.write("&gt;");
          }
          else
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          }
        }
        else
        {
          if (ch == '&')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            out.write("&amp;");
          }
          else if (ch == 0x0A)
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            _println(out);
            if ((i + 1 < end) && (text[i + 1] == 0x0D))
              i++;
          }
          else if (ch == 0x0D)
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            _println(out);
            if ((i + 1 < end) && (text[i + 1] == 0x0A))
              i++;
          }
          else
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          }
        }
      }
      else if (ch <= 0xff)
      {
        buffIndex = _flushBuffer(out, buff, buffIndex);

        out.write('&');
        out.write(_sISO8859_1_Entities[ch - 0xA0]);
        out.write(';');
      }
      else
      {
        buffIndex = _flushBuffer(out, buff, buffIndex);

        // RFE 1709817:  we need a way to force a line break in
        // random strings.  And here this Unicode codepoint lies,
        // all waiting for us - it means "force a line break",
        // unambiguously, and the odds are about 10-zillion-to-one
        // that anyone's using it.  So, treat this special character,
        // well, specially.
        if (ch == _UNICODE_LINE_BREAK)
          out.write("<br>");
        else if (ch == _UNICODE_HYPHENATION_POINT)
          out.write("<wbr>");
        else
          _writeDecRef(out, ch);
      }
    }

    _flushBuffer(out, buff, buffIndex);
  }



  /**
   * Write String text.  Note that this code is duplicated above for
   * character arrays - change both places if you make any changes!!!
   */
  static public void writeText(
    Writer out,
    char[]      buff,
    String      text
    ) throws IOException
  {
    int buffLength = buff.length;
    int buffIndex = 0;

    int length = text.length();

    for (int i = 0; i < length; i++)
    {
      char ch = text.charAt(i);

      if (ch < 0xA0)
      {
        // If "?" or over...
        if (ch >= 0x3f)
        {
          buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
        }
        // If "'" or over...
        else if (ch >= 0x27)
        {
          if (ch < 0x3c)
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          }
          else if (ch == '<')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            out.write("&lt;");
          }
          else if (ch == '>')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            out.write("&gt;");
          }
          else
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          }
        }
        else
        {
          if (ch == '&')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            out.write("&amp;");
          }
          else if (ch == 0x0A)
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            _println(out);
            if ((i + 1 < length) && (text.charAt(i + 1) == 0x0D))
              i++;
          }
          else if (ch == 0x0D)
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            _println(out);
            if ((i + 1 < length) && (text.charAt(i + 1) == 0x0A))
              i++;
          }
          else
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          }
        }
      }
      else if (ch <= 0xff)
      {
        buffIndex = _flushBuffer(out, buff, buffIndex);

        out.write('&');
        out.write(_sISO8859_1_Entities[ch - 0xA0]);
        out.write(';');
      }
      else
      {
        buffIndex = _flushBuffer(out, buff, buffIndex);

        // See above for what _UNICODE_LINE_BREAK means...
        if (ch == _UNICODE_LINE_BREAK)
          out.write("<br>");
        else if (ch == _UNICODE_HYPHENATION_POINT)
          out.write("<wbr>");
        else
          _writeDecRef(out, ch);
      }
    }

    _flushBuffer(out, buff, buffIndex);
  }


  /**
   * Write a string attribute.  Note that this code
   * is duplicated below for character arrays - change both
   * places if you make any changes!!!
   */
  static public void writeAttribute(
    Writer out,
    char[]      buff,
    String      text
    )
    throws IOException
  {
    int buffLength = buff.length;
    int buffIndex = 0;

    int length = text.length();
    for (int i = 0; i < length; i++)
    {
      char ch = text.charAt(i);

      if (ch < 0xA0)
      {
        // If "?" or over...
        if (ch >= 0x3f)
        {
          buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
        }
        // If "'" or over...
        else if (ch >= 0x27)
        {
          if (ch < 0x3c)
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          // Note - "<" isn't escaped in attributes, as per HTML spec
          }
          else if (ch == '>')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            out.write("&gt;");
          }
          else
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          }
        }
        else
        {
          if (ch == '&')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            // HTML 4.0, section B.7.1: ampersands followed by
            // an open brace don't get escaped
            if ((i + 1 < length) &&
                (text.charAt(i + 1) == '{'))
              out.write(ch);
            else
              out.write("&amp;");
          }
          else if (ch == '"')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            out.write("&quot;");
          }
          else if (ch == 0x0A)
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            _println(out);
            if ((i + 1 < length) && (text.charAt(i + 1) == 0x0D))
              i++;
          }
          else if (ch == 0x0D)
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            _println(out);
            if ((i + 1 < length) && (text.charAt(i + 1) == 0x0A))
              i++;
          }
          else
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          }
        }
      }
      else if (ch <= 0xff)
      {
        buffIndex = _flushBuffer(out, buff, buffIndex);

        out.write('&');
        out.write(_sISO8859_1_Entities[ch - 0xA0]);
        out.write(';');
      }
      else
      {
        buffIndex = _flushBuffer(out, buff, buffIndex);

        _writeDecRef(out, ch);
      }
    }

    _flushBuffer(out, buff, buffIndex);
  }


  static public void writeAttribute(
    Writer out,
    char[]      buffer,
    char[]      text
    )
    throws IOException
  {
    writeAttribute(out, buffer, text, 0, text.length);
  }


  /**
   * Write a character array attribute.  Note that this code
   * is duplicated below for character arrays - change both
   * places if you make any changes!!!
   */
  static public void writeAttribute(
    Writer out,
    char[]      buff,
    char[]      text,
    int         start,
    int         length) throws IOException
  {
    int buffLength = buff.length;
    int buffIndex = 0;

    int end = start + length;
    for (int i = start; i < end; i++)
    {
      char ch = text[i];

      if (ch < 0xA0)
      {
        // If "?" or over...
        if (ch >= 0x3f)
        {
          buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
        }
        // If "'" or over...
        else if (ch >= 0x27)
        {
          if (ch < 0x3c)
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          }
          // Note - "<" isn't escaped in attributes, as per HTML spec
          else if (ch == '>')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            out.write("&gt;");
          }
          else
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          }
        }
        else
        {
          if (ch == '&')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            // HTML 4.0, section B.7.1: ampersands followed by
            // an open brace don't get escaped
            if ((i + 1 < end) &&
                (text[i + 1] == '{'))
              out.write(ch);
            else
              out.write("&amp;");
          }
          else if (ch == '"')
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            out.write("&quot;");
          }
          else if (ch == 0x0A)
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            _println(out);
            if ((i + 1 < end) && (text[i + 1] == 0x0D))
              i++;
          }
          else if (ch == 0x0D)
          {
            buffIndex = _flushBuffer(out, buff, buffIndex);

            _println(out);
            if ((i + 1 < end) && (text[i + 1] == 0x0A))
              i++;
          }
          else
          {
            buffIndex = _addToBuffer(out, buff, buffIndex, buffLength, ch);
          }
        }
      }
      else if (ch <= 0xff)
      {
        buffIndex = _flushBuffer(out, buff, buffIndex);

        out.write('&');
        out.write(_sISO8859_1_Entities[ch - 0xA0]);
        out.write(';');
      }
      else
      {
        buffIndex = _flushBuffer(out, buff, buffIndex);

        _writeDecRef(out, ch);
      }
    }

    _flushBuffer(out, buff, buffIndex);
  }


  /**
   * Returns the index at which the first escaped character appears
   */
  private static int _getEscapedIndex(
    String text
    )
  {
    int length = text.length();

    int i = 0;

    for (; i < length; i++)
    {
      char ch = text.charAt(i);

      if (ch < 0xA0)
      {
        // If "?" or over...
        if (ch >= 0x3f)
        {
          continue;
        }
        // If "'" or over...
        else if (ch >= 0x27)
        {
          if (ch < 0x3c)
          {
            continue;
          }
          else if (ch == '<')
          {
            // =-= bts argh!! "<" isn't supposed to be escaped in attributes,
            //         as per HTML spec
            break;
          }
          else if (ch == '>')
          {
            break;
          }
          else
          {
            continue;
          }
        }
        else
        {
          if (ch == '&')
          {
            // HTML 4.0, section B.7.1: ampersands followed by
            // an open brace don't get escaped
            if ((i + 1 < length) &&
                (text.charAt(i + 1) == '{'))
            {
              continue;
            }
            else
            {
              break;
            }
          }
          else if (ch == '"')
          {
            break;
          }
          else
          {
            continue;
          }
        }
      }
      else
      {
        break;
      }
    }

    return i;
  }

  /**
   * Append a String to a buffer at an offset and return the new buffer offset
   */
  static private int _addStringToBuffer(
    char[] buffer,
    int    offset,
    String text
    )
  {
    int length = text.length();

    for (int i = 0; i < length; i++)
    {
      buffer[offset++] = text.charAt(i);
    }

    return offset;
  }


  /**
   * Append a decimal character entitity to a buffer at an offset and return
   * the new buffer offset
   */
  static private int _addDecRefToBuffer(
    char[] buffer,
    int    offset,
    char   ch
    )
  {
    buffer[offset++] = '&';
    buffer[offset++] = '#';

    // Formerly used String.valueOf().  This version tests out
    // about 40% faster (and on systems where GC is going gonzo,
    // it should be massively better)
    int i = ch;
    if (i > 10000)
    {
      buffer[offset++] = (char)('0' + (i / 10000));
      i = i % 10000;
      buffer[offset++] = (char)('0' + (i / 1000));
      i = i % 1000;
      buffer[offset++] = (char)('0' + (i / 100));
      i = i % 100;
      buffer[offset++] = (char)('0' + (i / 10));
      i = i % 10;
      buffer[offset++] = (char)('0' + i);
    }
    else if (i > 1000)
    {
      buffer[offset++] = (char)('0' + (i / 1000));
      i = i % 1000;
      buffer[offset++] = (char)('0' + (i / 100));
      i = i % 100;
      buffer[offset++] = (char)('0' + (i / 10));
      i = i % 10;
      buffer[offset++] = (char)('0' + i);
    }
    else
    {
      buffer[offset++] = (char)('0' + (i / 100));
      i = i % 100;
      buffer[offset++] = (char)('0' + (i / 10));
      i = i % 10;
      buffer[offset++] = (char)('0' + i);
    }

    buffer[offset++] = ';';

    return offset;
  }

  /**
   * Writes the output as a decimal escape.  This is larger than the hex
   * equivalent but works on versions of Netscape before 4.74, and should
   * be faster than the hex version.  See bug #1491321.
   * <p>
   */
  static private void _writeDecRef(
    Writer out,
    char        ch
    ) throws IOException
  {
    out.write("&#");
    // Formerly used String.valueOf().  This version tests out
    // about 40% faster (and on systems where GC is going gonzo,
    // it should be massively better)
    int i = ch;
    if (i > 10000)
    {
      out.write('0' + (i / 10000));
      i = i % 10000;
      out.write('0' + (i / 1000));
      i = i % 1000;
      out.write('0' + (i / 100));
      i = i % 100;
      out.write('0' + (i / 10));
      i = i % 10;
      out.write('0' + i);
    }
    else if (i > 1000)
    {
      out.write('0' + (i / 1000));
      i = i % 1000;
      out.write('0' + (i / 100));
      i = i % 100;
      out.write('0' + (i / 10));
      i = i % 10;
      out.write('0' + i);
    }
    else
    {
      out.write('0' + (i / 100));
      i = i % 100;
      out.write('0' + (i / 10));
      i = i % 10;
      out.write('0' + i);
    }

    out.write(';');
  }


  /**
   * Add a character to the buffer, flushing the buffer if the buffer is full,
   * and returning the new buffer index
   */
  private static int _addToBuffer(
    Writer out,
    char[]      buffer,
    int         bufferIndex,
    int         bufferLength,
    char        ch
    ) throws IOException
  {
    if (bufferIndex >= bufferLength)
    {
      out.write(buffer, 0, bufferIndex);
      bufferIndex = 0;
    }

    buffer[bufferIndex] = ch;

    return bufferIndex + 1;
  }


  /**
   * Flush the contents of the buffer to the output stream
   * and return the reset buffer index
   */
  private static int _flushBuffer(
    Writer out,
    char[]      buffer,
    int         bufferIndex
    ) throws IOException
  {
    if (bufferIndex > 0)
    {
      out.write(buffer, 0, bufferIndex);
      bufferIndex = 0;
    }

    return bufferIndex;
  }

  private static void _println(Writer out) throws IOException
  {
    out.write('\n');
  }

  private HTMLEscapes()
  {
  }

  //
  // Entities from HTML 4.0, section 24.2.1; character codes 0xA0 to 0xFF
  //
  static private String[] _sISO8859_1_Entities = new String[]
  {
    "nbsp",
    "iexcl",
    "cent",
    "pound",
    "curren",
    "yen",
    "brvbar",
    "sect",
    "uml",
    "copy",
    "ordf",
    "laquo",
    "not",
    "shy",
    "reg",
    "macr",
    "deg",
    "plusmn",
    "sup2",
    "sup3",
    "acute",
    "micro",
    "para",
    "middot",
    "cedil",
    "sup1",
    "ordm",
    "raquo",
    "frac14",
    "frac12",
    "frac34",
    "iquest",
    "Agrave",
    "Aacute",
    "Acirc",
    "Atilde",
    "Auml",
    "Aring",
    "AElig",
    "Ccedil",
    "Egrave",
    "Eacute",
    "Ecirc",
    "Euml",
    "Igrave",
    "Iacute",
    "Icirc",
    "Iuml",
    "ETH",
    "Ntilde",
    "Ograve",
    "Oacute",
    "Ocirc",
    "Otilde",
    "Ouml",
    "times",
    "Oslash",
    "Ugrave",
    "Uacute",
    "Ucirc",
    "Uuml",
    "Yacute",
    "THORN",
    "szlig",
    "agrave",
    "aacute",
    "acirc",
    "atilde",
    "auml",
    "aring",
    "aelig",
    "ccedil",
    "egrave",
    "eacute",
    "ecirc",
    "euml",
    "igrave",
    "iacute",
    "icirc",
    "iuml",
    "eth",
    "ntilde",
    "ograve",
    "oacute",
    "ocirc",
    "otilde",
    "ouml",
    "divide",
    "oslash",
    "ugrave",
    "uacute",
    "ucirc",
    "uuml",
    "yacute",
    "thorn",
    "yuml"
  };

  // =-=AEW Need entities from 24.3.1 and 24.4.1

  // Constant for the Unicode line break character
  static private final char _UNICODE_LINE_BREAK = 0x2028;

  // Constant for the Unicode hyphenation point character.
  // UIFunctions.hyphenate(..) uses this character to indicate where to insert
  // the <wbr> tag. This tag inserts a no-width-space so that the browser may
  // break the line at that point.
  static private final char _UNICODE_HYPHENATION_POINT = 0x2027;
}
