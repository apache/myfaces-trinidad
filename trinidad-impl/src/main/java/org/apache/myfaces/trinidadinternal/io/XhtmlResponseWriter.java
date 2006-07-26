/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.io;

import java.io.IOException;
import java.io.Writer;
import java.io.UnsupportedEncodingException;

import java.util.ArrayList;

import javax.faces.component.UIComponent;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.util.IntegerUtils;

import org.apache.myfaces.trinidad.logging.ADFLogger;

import org.apache.myfaces.trinidadinternal.share.util.CaboHttpUtils;

import org.apache.myfaces.trinidadinternal.ui.io.EscapedText;
import org.apache.myfaces.trinidadinternal.ui.io.EscapedTextFactory;
import org.apache.myfaces.trinidadinternal.ui.io.XMLEscapes;

/**
 * Implementation of ResponseWriter for outputting XHTML.
 *
 * @author The Oracle ADF Faces Team
 */
public class XhtmlResponseWriter extends ResponseWriter
{
  static public final String XHTML_CONTENT_TYPE = "application/xhtml+xml";
  /**
   * Creates an XhtmlResponseWriter.
   * @param out a Writer to write to
   * @param contentType the xhtml content type
   * @param encoding the character encoding the Writer uses
   */
  public XhtmlResponseWriter(
    Writer out,
    String contentType,
    String encoding) throws UnsupportedEncodingException
  {
    _out = out;
    _contentType = contentType;
    _encoding = encoding;
    CaboHttpUtils.validateEncoding(encoding);
  }

  public String getCharacterEncoding()
  {
    return _encoding;
  }

  public String getContentType()
  {
    return XHTML_CONTENT_TYPE;
  }

  public void startDocument() throws IOException
  {
  }


  public void endDocument() throws IOException
  {
    _out.flush();
  }

  public void flush() throws IOException
  {
    _closeStartIfNecessary();
  }


  public void close()throws IOException
  {
    flush();
    // =-=AEW And anything else?
  }

  public void startElement(String name,
                           UIComponent component) throws IOException
  {
    // =-=AEW Should we force all lowercase?
    if (name.charAt(0) == 's')
    {
      // Optimization (see bug 2009019): our code has a tendency
      // to write out unnecessary empty <span> elements.  So,
      // when we start a "span" HTML element, don't actually write
      // out anything just yet;  mark it pending.
      if ("span".equals(name))
      {
        // push any pending element onto the stack of skipped elements
        _markPendingElements();

        // make this the current pending element
        _pendingElement = name;
        return;
      }
      else if ("script".equals(name) ||
               "style".equals(name))
      {
        _dontEscape = true;
      }
    }

    // start writing the element
    _startElementImpl(name);
  }


  public void endElement(String name) throws IOException
  {
    // eliminate any <pending></pending> combinations
    if (_pendingElement != null)
    {
      // we need to return immedediately because in this
      // case, the element was never pushed onto the
      // element stack.
      _pendingElement = null;
      return;
    }

    // get the name of the last outputted element
    String element = _popSkippedElement();

    // non-null names indicate that the element was ouput, so its
    // end tag should be output as well
    if (element != null)
    {
      if (!element.equals(name))
      {
        _LOG.severe("Element End name:"           +
                    name                          +
                    " does not match start name:" +
                    element);
      }

      Writer out = _out;

      // always turn escaping back on once an element ends
      _dontEscape = false;

      if (_closeStart)
      {
        out.write('>');
        _closeStart = false;
      }

      out.write("</");
      out.write(name);
      out.write('>');
    }
  }


  public void writeAttribute(String name,
                             Object value,
                             String componentPropertyName)
        throws IOException
  {
    if (value == null)
      return;

    // if we have a pending element, flush it because
    // it has an attribute, and is thus needed
    _outputPendingElements();

    Writer out = _out;

    Class valueClass = value.getClass();

    // Output Boolean values specially
    if (valueClass == _BOOLEAN_CLASS)
    {
      if (Boolean.TRUE.equals(value))
      {
        out.write(' ');
        out.write(name);
        out.write("=\"");
        out.write(name);
        out.write("\"");
      }
    }
    else
    {
      out.write(' ');
      out.write(name);
      out.write("=\"");

      // write the attribute value
      _writeValue(valueClass, value, true);
      out.write('"');
    }
  }


  public void writeURIAttribute(String name,
                                Object value,
                                String componentPropertyName)
    throws IOException
  {
    // XML attributes are XML attributes, etc.
    writeAttribute(name, value, componentPropertyName);
  }

  public void writeComment(Object comment) throws IOException
  {
    if (comment != null)
    {
      _closeStartIfNecessary();
      _out.write("<!-- ");
      _out.write(comment.toString());
      _out.write(" -->");
    }
  }


  public void writeText(Object text, String componentPropertyName)
     throws IOException
  {
    if (text != null)
    {
      if (_dontEscape)
      {
        write(text.toString());
      }
      else
      {
        _closeStartIfNecessary();

        XMLEscapes.writeText(_out, text.toString().toCharArray());
      }
    }
  }


  public void writeText(char text[], int off, int len)
        throws IOException
  {
    if (_dontEscape)
    {
      write(text, off, len);
    }
    else
    {
      _closeStartIfNecessary();
      XMLEscapes.writeText(_out, text, off, len);
    }
  }

  public void write(char cbuf[], int off, int len) throws IOException
  {
    _closeStartIfNecessary();
    _out.write(cbuf, off, len);
  }

  public void write(String str) throws IOException
  {
    _closeStartIfNecessary();
    _out.write(str);
  }

  public void write(int c) throws IOException
  {
    _closeStartIfNecessary();
    _out.write((char) c);
  }


  public ResponseWriter cloneWithWriter(Writer writer)
  {
    try
    {
      return new XhtmlResponseWriter(writer, getContentType(),
                                     getCharacterEncoding());
    }
    catch (UnsupportedEncodingException e)
    {
      // this can't happen;  the character encoding should already
      // be legal.
      assert(false);
      throw new IllegalStateException();
    }
  }

  //
  // Private methods
  //

  private void _startElementImpl(String name) throws IOException
  {
    // close any previously stated element, if necessary
    _closeStartIfNecessary();

    // note that we started a non-skipped element
    _pushOutputtedElement(name);

    Writer out = _out;
    out.write('<');
    out.write(name);
    _closeStart = true;

  }


  /**
   * Writes the value of an object
   */
  private void _writeValue(
    Class       valueClass,
    Object      value,
    boolean     isAttribute
    ) throws IOException
  {
    assert(valueClass != _CHAR_ARRAY_CLASS) :
           "Character arrays not supported as HTML attributes";

    if (valueClass == _INTEGER_CLASS)
    {
      // Integers never need to be escaped - and
      // we can cache common instances.
      _out.write(IntegerUtils.getString((Integer) value));

      return;
    }

    if (valueClass == _ESCAPED_TEXT_CLASS)
    {
      EscapedText escapedText = (EscapedText)value;

      if (EscapedTextFactory.HTML_MIME_TYPE == escapedText.getMimeType())
      {
        if (isAttribute)
        {
          _out.write(escapedText.getAttributeText());
        }
        else
        {
          _out.write(escapedText.getContentText());
        }

        return;
      }
    }

    if (isAttribute)
    {
      XMLEscapes.writeAttribute(_out, value.toString().toCharArray());
    }
    else
    {
      XMLEscapes.writeText(_out, value.toString().toCharArray());
    }
  }


  private void _closeStartIfNecessary() throws IOException
  {
    _markPendingElements();

    if (_closeStart)
    {
      _out.write('>');
      _closeStart = false;
    }
  }


  /**
   * Flushes out any pending element, celaring the pending
   * entry.
   */
  private void _outputPendingElements() throws IOException
  {
    String pendingElement = _pendingElement;

    if (pendingElement != null)
    {
      // we clear the pending element BEFORE calling
      // startElementImpl to prevent _startElementImpl's indirect call
      // to _markPendingElements from pushing our element onto
      // the skipped stack, imbalancing the stack
      _pendingElement = null;

      // start the pending element
      _startElementImpl(pendingElement);
    }
  }

  /**
   * If an element is pending, push it onto the stack of skipped
   * elements because it doesn't have any attributes
   */
  private void _markPendingElements()
  {
    String pendingElement = _pendingElement;
    if (pendingElement != null)
    {
      _pushSkippedElement();
      _pendingElement = null;
    }
  }


  /**
   * Retrieves the name of the last output element.  If it is null,
   * that element was skipped and thus its end tag should be suppressed
   * as well
   */
  private String _popSkippedElement()
  {
    int size = _skippedElements.size();
    if (size == 0)
      return null;

    return (String)_skippedElements.remove(size - 1);
  }

  /**
   * Marks the skipped element so that the output of its
   * end tag can also be suppressed
   */
  private void _pushSkippedElement()
  {
    _skippedElements.add(null);
  }


  /**
   * Marks that we have outputted a real element so that the ordering of
   * the outputted and skipped elements can be maintained.  This
   * also aids debuggin by ensuring that the element being ended matches
   * the actual ouputted name.
   */
  private void _pushOutputtedElement(
    String name
    )
  {
    _skippedElements.add(name);
  }

  private boolean     _closeStart;
  private boolean     _dontEscape;

  private Writer       _out;
  private String       _contentType;
  private String       _encoding;

  // holds an element that will only be started if it has attributes
  private String      _pendingElement;

  // stack of skipped and unskipped elements used to determine when
  // to suppress the end tag of a skipped element
  private final ArrayList   _skippedElements = new ArrayList(20);


  private static final Class _CHAR_ARRAY_CLASS = (new char[0]).getClass();
  private static final Class _BOOLEAN_CLASS = Boolean.class;
  private static final Class _INTEGER_CLASS = Integer.class;
  private static final Class _ESCAPED_TEXT_CLASS = EscapedText.class;

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(XhtmlResponseWriter.class);
}
