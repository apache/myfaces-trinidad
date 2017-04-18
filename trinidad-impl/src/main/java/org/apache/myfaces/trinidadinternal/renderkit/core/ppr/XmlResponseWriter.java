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
package org.apache.myfaces.trinidadinternal.renderkit.core.ppr;


import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Map;

import javax.el.ValueExpression;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidadinternal.io.XMLEscapes;

public class XmlResponseWriter extends ResponseWriter
{
  public XmlResponseWriter(
    Writer writer,
    String encoding)
  {
    _out = writer;
    _encoding = encoding;
    _cdataCount = 0;
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

  /**
   * Writes out CDATA start.
   * @throws IOException on any read/write error
   */
  public void startCDATA() throws IOException 
  {
    closeStartIfNecessary();
    // Ignore all nested calls to start a CDATA section except the first - a CDATA section cannot contain the string 
    // "]]>" as the section ends ends with the first occurrence of this sequence.
    _cdataCount++;
    
    if (_cdataCount == 1)
      _out.write("<![CDATA[");
  }

  /**
   * Writes out an end CDATA element.
   * @throws IOException on any read/write error
   */
  public void endCDATA() throws IOException 
  {
    // Only close the outermost CDATA section and ignore nested calls to endCDATA(). 
    if (_cdataCount == 1)
      _out.write("]]>");
    
    _cdataCount--;
  }

  public void endDocument() throws IOException
  {
  }

  public void startElement(
    String      name,
    UIComponent component) throws IOException
  {
    closeStartIfNecessary();
    Map<String, Object> passThroughAttributes =
            (component != null) ? component.getPassThroughAttributes(false) : null;
    name = _processPassThroughAttributes(name, passThroughAttributes);
    _pushElement(name);
    Writer out = _out;
    out.write('<');
    out.write(name);
    _closeStart = true;
    _writePassThroughAttributes(passThroughAttributes);
  }

  private String _processPassThroughAttributes(String name, Map<String, Object> passThroughAttributes)
  {
    if (passThroughAttributes != null)
    {
      Object value = passThroughAttributes.get(Renderer.PASSTHROUGH_RENDERER_LOCALNAME_KEY);
      if (value instanceof ValueExpression)
      {
        value = ((ValueExpression)value).getValue(FacesContext.getCurrentInstance().getELContext());
      }
      if (value != null)
      {
        String elementName = value.toString();
        if (!name.equals(elementName)) {
          name = elementName;
        }
      }
    }
    return name;
  }

  private void _writePassThroughAttributes(Map<String, Object> passThroughAttributes) throws IOException {
    if (passThroughAttributes != null)
    {
      for (Map.Entry<String, Object> entry : passThroughAttributes.entrySet())
      {
        String key = entry.getKey();
        Object value = entry.getValue();
        if (Renderer.PASSTHROUGH_RENDERER_LOCALNAME_KEY.equals(key))
        {
          // Special attribute stored in passthrough attribute map,
          // skip rendering
          continue;
        }
        if (value instanceof ValueExpression)
        {
          value = ((ValueExpression)value).getValue(FacesContext.getCurrentInstance().getELContext());
        }
        // encodeAndWriteURIAttribute(key, value, key);
        // JSF 2.2 In the renderkit javadoc of jsf 2.2 spec says this
        // (Rendering Pass Through Attributes):
        // "... The ResponseWriter must ensure that any pass through attributes are
        // rendered on the outer-most markup element for the component. If there is
        // a pass through attribute with the same name as a renderer specific
        // attribute, the pass through attribute takes precedence. Pass through
        // attributes are rendered as if they were passed to
        // ResponseWriter.writeURIAttribute(). ..."
        // Note here it says "as if they were passed", instead say "... attributes are
        // encoded and rendered as if ...". Black box testing against RI shows that there
        // is no URI encoding at all in this part, so in this case the best is do the
        // same here. After all, it is resposibility of the one who set the passthrough
        // attribute to do the proper encoding in cases when a URI is provided. However,
        // that does not means the attribute should not be encoded as other attributes.
        // According to tests done, if passthrough attribute is null, the attribute must not
        // be rendered.
        if (value != null)
        {
          writeAttribute(key, value, null);
        }
      }
    }
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
    XMLEscapes.writeAttribute(out, value.toString());
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
    char[] text,
    int    offset,
    int    length) throws IOException
  {
    if (text != null)
    {
      closeStartIfNecessary();
      XMLEscapes.writeText(_out, text, offset, length);
    }
  }

  public void writeText(
    Object text,
    String attrName) throws IOException
  {
    if (text != null)
    {
      closeStartIfNecessary();
      XMLEscapes.writeText(_out, text.toString());
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
    name = _popElement();
    Writer out = _out;
    if (_closeStart)
    {
      out.write("/>");
      _closeStart = false;
    }
    else
    {
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

  protected void closeStartIfNecessary() throws IOException
  {
    if (_closeStart)
    {
      Writer out = _out;
      out.write('>');
      _closeStart = false;
    }
  }

  /**
   * Retrieves the name of the last output element.  If it is null,
   * something is wrong
   */
  private String _popElement()
  {
    int size = _elements.size();
    if (size == 0)
      return null;

    return _elements.remove(size - 1);
  }

  /**
   * Marks that we have outputted a real element so that the ordering of
   * the outputted and skipped elements can be maintained.
   */
  private void _pushElement(String name)
  {
    _elements.add(name);
  }
  
  private final Writer      _out;
  private final String      _encoding;
  private       boolean     _closeStart;
  private       int         _cdataCount;
  private final ArrayList<String> _elements = new ArrayList<String>(20);

}
