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
package org.apache.myfaces.trinidadinternal.renderkit.core.ppr;


import java.io.IOException;
import java.io.Writer;

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

public class XmlResponseWriter extends ResponseWriter
{
  public XmlResponseWriter(
    Writer writer,
    String encoding)
  {
    _out = writer;
    _encoding = encoding;
  }

  public String getCharacterEncoding()
  {
    return _encoding;
  }

  public String getContentType()
  {
    return "text/xml";
  }

  public void startDocument() throws IOException
  {
    //TODO include character encoding
    _out.write("<?xml version=\"1.0\" ?>\n");
  }

  public void endDocument() throws IOException
  {
  }

  public void startElement(
    String      name,
    UIComponent component) throws IOException
  {
    closeStartIfNecessary();

    Writer out = _out;
    out.write('<');
    out.write(name);
    _closeStart = true;
  }

  public void writeAttribute(
    String name,
    Object value,
    String attrName) throws IOException
  {
    if (value == null)
      return;

    Writer out = _out;
    // write the attribute value
    out.write(' ');
    out.write(name);
    out.write("=\"");
    writeAttributeText(value, attrName);
    out.write("\"");
  }

  public void writeComment(
    Object comment) throws IOException
  {
    if (comment != null)
    {
      closeStartIfNecessary();

      Writer out = _out;
      out.write("<!-- ");
      out.write(comment.toString());
      out.write(" -->");
    }
  }

  public void writeText(
    char[] buffer,
    int    offset,
    int    length) throws IOException
  {
    if (buffer != null)
    {
      closeStartIfNecessary();
      _writeBodyText(buffer, offset, length);
    }
  }

  public void writeText(
    Object text,
    String attrName) throws IOException
  {
    if (text != null)
    {
      closeStartIfNecessary();
      //_openCDATAIfNecessary();
      writeBodyText(text, attrName);
    }
  }

  public void writeURIAttribute(
    String name,
    Object value,
    String attrName) throws IOException
  {
    writeAttribute(name, value, attrName);
  }

  public void endElement(
    String name) throws IOException
  {
    Writer out = _out;
    if (_closeStart)
    {
      out.write("/>");
      _closeStart = false;
    }
    else
    {
      //_closeCDATAIfNecessary();
      out.write("</");
      out.write(name);
      out.write(">");
    }
  }

  public ResponseWriter cloneWithWriter(
    Writer writer)
  {
    return new XmlResponseWriter(writer, getCharacterEncoding());
  }

  public void write(
    char[] cbuf,
    int    off,
    int    len) throws IOException
  {
    closeStartIfNecessary();
    _out.write(cbuf, off, len);
  }

  public void write(char[] cbuf) throws IOException
  {
    closeStartIfNecessary();
    _out.write(cbuf);
  }

  public void write(
    int c) throws IOException
  {
    closeStartIfNecessary();
    _out.write(c);
  }

  public void write(
    String str) throws IOException
  {
    closeStartIfNecessary();
    _out.write(str);
  }

  public void write(
    String str, 
    int    off, 
    int    len) throws IOException
  {
    closeStartIfNecessary();
    _out.write(str, off, len);
  }

  public void close() throws IOException
  {
    _out.close();
  }

  public void flush() throws IOException
  {
    _out.flush();
  }

  protected void writeAttributeText(
    Object text,
    String attrName) throws IOException
  {
    char[] buffer = text.toString().toCharArray();
    _writeAttributeText(buffer, 0, buffer.length);
  }

  protected void writeBodyText(
    Object text,
    String attrName) throws IOException
  {
    char[] buffer = text.toString().toCharArray();
    _writeBodyText(buffer, 0, buffer.length);
  }

  protected void closeStartIfNecessary() throws IOException
  {
    if (_closeStart)
    {
      Writer out = _out;
      out.write('>');
      _closeStart = false;
    }
  }

  /*
  private void _openCDATAIfNecessary() throws IOException
  {
    if (!_closeCDATA)
    {
      Writer out = _out;
      out.write("<![CDATA[");
      _closeCDATA = true;
    }
  }

  private void _closeCDATAIfNecessary() throws IOException
  {
    if (_closeCDATA)
    {
      Writer out = _out;
      out.write("]]>");
      _closeCDATA = false;
    }
  }
  */

  private void _writeAttributeText(
    char[]  text,
    int     start,
    int     length) throws IOException
  {
    Writer out = _out;
    int end = start + length;
    for (int i = start; i < end; i++)
    {
      char ch = text[i];

      if (ch <= 0x7f)
      {
        switch (ch)
        {
          case '>':
            out.write("&gt;");
            break;
          case '<':
            out.write("&lt;");
            break;
          case '"':
            out.write("&quot;");
            break;
          case '&':
            out.write("&amp;");
            break;
          default:
            out.write(ch);
            break;
        }
      }
      else
      {
        _writeHexRef(ch);
      }
    }
  }

  private void _writeBodyText(
    char[]  text,
    int     start,
    int     length) throws IOException
  {
    Writer out = _out;
    int end = start + length;
    for (int i = start; i < end; i++)
    {
      char ch = text[i];

      if (ch <= 0x7f)
      {
        switch (ch)
        {
          case '>':
            out.write("&gt;");
            break;
          case '<':
            out.write("&lt;");
            break;
          case '&':
            out.write("&amp;");
            break;
          default:
            out.write(ch);
            break;
        }
      }
      else
      {
        _writeHexRef(ch);
      }
    }
  }

  private void _writeHexRef(
    char ch) throws IOException
  {
    Writer out = _out;
    out.write("&#x");
    // =-=AEW Could easily be more efficient.
    out.write(Integer.toHexString(ch));
    out.write(';');
  }
  
  private final Writer      _out;
  private final String      _encoding;
  private       boolean     _closeStart;
  //  private       boolean     _closeCDATA;
}
