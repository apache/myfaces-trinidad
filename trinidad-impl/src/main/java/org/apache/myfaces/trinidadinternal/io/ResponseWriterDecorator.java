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

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

/**
 * ResponseWriter abstract base used to decorate another ResponseWriter.
 * 
 */
abstract public class ResponseWriterDecorator extends ResponseWriter
{
  /**
   * Create a ResponseWriterDecorator.
   * 
   * @param decorated the decorated ResponseWriter
   */
  public ResponseWriterDecorator(ResponseWriter decorated)
  {
    _decorated = decorated;
  }

  /**
   * Return the decorated ResponseWriter.
   */
  protected ResponseWriter getResponseWriter()
  {
    return _decorated;
  }

  @Override
  public String getCharacterEncoding()
  {
    return getResponseWriter().getCharacterEncoding();
  }

  @Override
  public String getContentType()
  {
    return getResponseWriter().getContentType();
  }

  @Override
  public void startDocument() throws IOException
  {
    getResponseWriter().startDocument();
  }


  @Override
  public void endDocument() throws IOException
  {
    getResponseWriter().endDocument();
  }

  @Override
  public void flush() throws IOException
  {
    getResponseWriter().flush();
  }


  @Override
  public void close()throws IOException
  {
    getResponseWriter().close();
  }

  @Override
  public void startElement(String name,
                           UIComponent component) throws IOException
  {
    getResponseWriter().startElement(name, component);
  }

  
  @Override
  public void endElement(String name) throws IOException
  {
    getResponseWriter().endElement(name);
  }


  @Override
  public void writeAttribute(String name,
                             Object value,
                             String componentPropertyName)
        throws IOException
  {
    getResponseWriter().writeAttribute(name, value, componentPropertyName);
  }


  @Override
  public void writeURIAttribute(String name,
                                Object value,
                                String componentPropertyName)
    throws IOException
  {
    getResponseWriter().writeURIAttribute(name, value, componentPropertyName);
  }

  @Override
  public void writeComment(Object comment) throws IOException
  {
    getResponseWriter().writeComment(comment);
  }



  @Override
  public void writeText(Object text, UIComponent component, 
                        String propertyName)
    throws IOException
  {
    getResponseWriter().writeText(text, component, propertyName);
  }
  
  @Override
  public void writeText(Object text, String componentPropertyName) throws IOException
  {
    getResponseWriter().writeText(text, componentPropertyName);
  }


  @Override
  public void writeText(char text[], int off, int len)
        throws IOException
  {
    getResponseWriter().writeText(text, off, len);
  }

  @Override
  public void write(char cbuf[], int off, int len) throws IOException
  {
    getResponseWriter().write(cbuf, off, len);
  }

  @Override
  public void write(String str) throws IOException
  {
    getResponseWriter().write(str);
  }

  @Override
  public void write(int c) throws IOException
  {
    getResponseWriter().write((char) c);
  }

  @Override
  public void write(char[] cbuf)
    throws IOException
  {
    getResponseWriter().write(cbuf);
  }

  @Override
  public void write(String str, int off, int len)
    throws IOException
  {
    getResponseWriter().write(str, off, len);
  }

  @Override
  public ResponseWriter cloneWithWriter(Writer writer)
  {
    return getResponseWriter().cloneWithWriter(writer);
  }

  public String toString()
  {
    return super.toString() + "[" + _decorated.toString() + "]";
    
  }

  private final ResponseWriter _decorated;
}
