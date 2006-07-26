/*
 * Copyright  2002-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.renderkit.core.ppr;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.logging.ADFLogger;

import org.apache.myfaces.trinidad.util.ArrayMap;
import org.apache.myfaces.trinidadinternal.io.ResponseWriterDecorator;

/**
 * ResponseWriter which buffers up/filters out any deferred scripts.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/ppr/ScriptBufferingResponseWriter.java#0 $) $Date: 10-nov-2005.19:03:31 $
 * @author The Oracle ADF Faces Team
 */
public class ScriptBufferingResponseWriter extends ResponseWriterDecorator
{
  public ScriptBufferingResponseWriter(
     FacesContext   context,
     ResponseWriter output)
  {
    this(output, null);

    Map requestScope = context.getExternalContext().getRequestMap();
    // To support multiple Partial Roots, always look for past
    // data from a rendered partial request.
    _data = (Data) requestScope.get(_STORED_DATA_KEY);
    if (_data == null)
    {
      _data = new Data();
      requestScope.put(_STORED_DATA_KEY, _data);
    }
  }

  private ScriptBufferingResponseWriter(ResponseWriter output, Data data)
  {
    super(output);

    _data = data;
  }


  /**
   * Returns the buffered script contents
   */
  public String getBufferedScripts()
  {
    StringBuffer buffer = _data.buffer;

    if (buffer == null)
      return null;

    // We need to strip out any /* */ style comments in the scripts,
    // since these scripts will themselves be wrapped inside of a
    // comment to avoid execution in the iframe.
    StringBuffer strippedBuffer = _stripJSComments(buffer);

    // Actually, it is pointless for these scripts to contain comments -
    // since this just bloats the response.  So, let's make sure we
    // are notified if we do come across comments in our scripts - at
    // least in debug mode...
    // The buffers should be the same unless commments were found.
    if (strippedBuffer != buffer)
    {
      _LOG.fine("Script with comments found: {0}", buffer);
    }


    return strippedBuffer.toString();
  }

  /**
   * Returns objects representing any JavaScript libraries that
   * have been rendered during buffering.
   */
  public Iterator getBufferedLibraries()
  {
    if (_data.libraries == null)
      return null;

    return _data.libraries.iterator();
  }

  public ResponseWriter cloneWithWriter(Writer writer)
  {
    return new ScriptBufferingResponseWriter(
      getResponseWriter().cloneWithWriter(writer),
      _data);
  }

  public void writeComment(Object text) throws IOException
  {
    // Don't bother write out comments if we are buffering
    if (!_checkBuffer())
      super.writeComment(text);
  }

  public void writeText(Object text, String property) throws IOException
  {
    if (_checkBuffer())
      _data.buffer.append(text);
    else
      super.writeText(text, property);
  }

  public void writeText(
    char[]      text,
    int         start,
    int         length) throws IOException
  {
    if (_checkBuffer())
      _data.buffer.append(text, start, length);
    else
      super.writeText(text, start, length);
  }


  public void write(String text) throws IOException
  {
    if (_checkBuffer())
      _data.buffer.append(text);
    else
      super.write(text);
  }

  public void write(
    char[]      text,
    int         start,
    int         length) throws IOException
  {
    if (_checkBuffer())
      _data.buffer.append(text, start, length);
    else
      super.write(text, start, length);
  }

  public void write(int ch) throws IOException
  {
    if (_checkBuffer())
      _data.buffer.append(ch);
    else
      super.write(ch);
  }

  public void startElement(String name,
                           UIComponent component) throws IOException
  {
    // We should never start a new element if we inside a buffered script
    assert (!_checkBuffer());

    if (_isScript(name))
      _data.inScriptStart = true;
    else if (_isObject(name))
    {
      // ppr does not work with the object element (our media component).
      // the object can only be initialized
      // once on a page, so we need to comment it out in the iframe, then
      // uncomment it and set it to the span element on the target document.
      // When we get an object start element, output a span and a script
      // with the html commented out.
      super.startElement("span", null);
      super.writeAttribute("id", _PPR_OBJECT_SPAN+_data.objectSuffix, null);
      super.startElement("script", null);
      super.writeAttribute("id", _PPR_OBJECT_SCRIPT+_data.objectSuffix++, null);
      super.write("/*");
      super.startElement(name, component);
    }
    else
      super.startElement(name, component);
  }

  public void endElement(String name) throws IOException
  {
    if (_isScript(name))
    {
      boolean deferred = _isDeferred();

      // Check to see if we've got a library include
      Object source = _getSourceAttr();
      if (source != null)
      {
        // Add the library to the our list
        if (_data.libraries == null)
          _data.libraries = new ArrayList(10);

        _data.libraries.add(source);
      }

      if (_data.inScriptStart && !deferred)
      {
        // this script has been started, but the buffer has never been flushed
        _checkBuffer();
      }

      _data.inScriptStart = false;
      _data.buffering = false;

      if (_data.attrs != null)
        _data.attrs.clear();
      if (_data.uriAttrs != null)
        _data.uriAttrs.clear();

      // If this script was buffered, don't render the end element
      if (deferred)
        return;
      else
        super.endElement(name);

    }
    else if (_isObject(name))
    {
        super.endElement(name);
        super.write("*/");
        super.endElement("script");
        super.endElement("span");
    }
    else
      super.endElement(name);
  }

  public void writeAttribute(
    String     name,
    Object     value,
    String     property) throws IOException
  {
    if (_data.inScriptStart)
      _data.attrs.put(name, value);
    else
      super.writeAttribute(name, value, property);
  }

  public void writeURIAttribute(
    String     name,
    Object     value,
    String     property) throws IOException
  {
    if (_data.inScriptStart)
      _data.uriAttrs.put(name, value);
    else
      super.writeURIAttribute(name, value, property);
  }

  // Tests whether content should be buffered
  private boolean _checkBuffer() throws IOException
  {
    if (_data.buffering)
      return true;

    if (!_data.inScriptStart)
      return false;

    // We're done with the script start tag
    _data.inScriptStart = false;

    // We have just finished a script start tag.
    // Check to see if we need to buffer the script.
    if (_isDeferred())
    {
      // Start buffering
      if (_data.buffer == null)
        _data.buffer = new StringBuffer();
      else
      {
        // Tack on a separator between scripts just to be safe
        _data.buffer.append(';');
      }

      _data.buffering = true;
      return true;
    }

    // If we aren't buffering this script, we just output
    // the script start element now.
    super.startElement("script", null);

    Iterator keys = _data.attrs.keys();
    if (keys != null)
    {
      while (keys.hasNext())
      {
        String key = (String)keys.next();
        super.writeAttribute(key, _data.attrs.get(key), null);
      }
    }

    keys = _data.uriAttrs.keys();
    if (keys != null)
    {
      while (keys.hasNext())
      {
        String key = (String)keys.next();
        super.writeURIAttribute(key, _data.uriAttrs.get(key), null);
      }
    }

    return false;
  }

  // Returns the src attribute of the current script
  private Object _getSourceAttr()
  {
    Object source = _data.attrs.get("src");
    if (source == null)
      source = _data.uriAttrs.get("src");

    return source;
  }

  // Tests whether the script is deferred
  private boolean _isDeferred()
  {
    return Boolean.TRUE.equals(_data.attrs.get("defer"));
  }

  // Tests whether the element name is "script"
  private boolean _isScript(String name)
  {
    // Is this going to be too slow?
    return ("script".equalsIgnoreCase(name));
  }

  // Tests whether the element name is "object"
  private boolean _isObject(String name)
  {
    // Is this going to be too slow?
    return ("object".equalsIgnoreCase(name));
  }

  private Data _data;

  static private class Data
  {
    public Data()
    {
      attrs = new ArrayMap(3);
      uriAttrs = new ArrayMap(1);
    }

    public boolean      inScriptStart;  // Inside a script start element?
    public ArrayMap     attrs;          // Attrs of the script element
    public ArrayMap     uriAttrs;       // URI attrs of the script element
    public boolean      buffering;      // Are currently buffering?
    public StringBuffer buffer;         // The buffer
    public ArrayList    libraries;      // Imported JavaScript libraries
    public int          objectSuffix=0; // For the object element ppr bug
  }

  // This utility method is used to strip /**/ style comments out of
  // JavaScript code.  We strip comments out of scripts that are
  // included in the partial page response, since we actually comment
  // out all of these scripts to prevent them from being executed in
  // the hidden iframe.  If no comments are found, returns the provided
  // buffer.
  // Note: This method is only called by BodyRenderer and
  // ScriptBufferingResponseWriter, so we're leaving in
  // package-private.
  private static StringBuffer _stripJSComments(StringBuffer buffer)
  {
    // We avoid reallocating the buffer until we actually
    // find a comment.  Actually, we should never find any
    // comments in production code.  This method really shouldn't
    // be needed, but we do all of this work just to be extra safe.
    StringBuffer strippedBuffer = null;

    // We use a simple state machine to track whether or not
    // we are inside a comment or opening/closing a comment.
    int state = _STRIP_STATE_START;

    // The start index of the portion of the string to copy
    int startIndex = 0;

    // The total buffer length
    int length = buffer.length();

    for (int i = 0; i < length; i++)
    {
      char c = buffer.charAt(i);

      switch (state)
      {
        case _STRIP_STATE_START:
          // Check for the opening '/'
          if (c == '/')
            state = _STRIP_STATE_SLASH;
          break;

        case _STRIP_STATE_SLASH:
          // We've seen a potential comment opening '/'.  Check
          // to see if this is really the start of a comment.
          if (c == '*')
          {
            state = _STRIP_STATE_COMMENT;

            // Copy the contents up to the start of the
            // comment into the strippedBuffer.
            if (strippedBuffer == null)
              strippedBuffer = new StringBuffer(length);

            strippedBuffer.append(buffer.substring(startIndex, i - 1));
          }
          else
          {
            state = _STRIP_STATE_START;
          }
          break;

        case _STRIP_STATE_COMMENT:
          // We're inside a comment.  Just for a closing '*'.
          if (c == '*')
            state = _STRIP_STATE_STAR;
          break;

        case _STRIP_STATE_STAR:
          // We've seen a potential comment closing '*'.  Check
          // to see if this is really the end of the comment.
          if (c == '/')
          {
            state = _STRIP_STATE_START;
            startIndex = i + 1;
          }
          else
          {
            state = _STRIP_STATE_COMMENT;
          }
          break;
      }
    }

    // We should never end in any state other than start.  Anything
    // else would indicate an invalid script!
    assert (state == _STRIP_STATE_START);

    // Check for anything left in the pipeline
    if (strippedBuffer != null)
    {
      if (state == _STRIP_STATE_START)
        strippedBuffer.append(buffer.substring(startIndex, length));

      return strippedBuffer;
    }

    // If there were no comments, just return the original buffer.
    return buffer;
  }


  // Constants used by _stripJSComments()
  private static final int _STRIP_STATE_START   = 0;  // Start state
  private static final int _STRIP_STATE_SLASH   = 1;  // Open '/' is seen
  private static final int _STRIP_STATE_COMMENT = 2;  // Inside comment
  private static final int _STRIP_STATE_STAR    = 3;  // Closing '*' is seen


  private static final String _PPR_OBJECT_SPAN = "_pprObjectSpan";
  private static final String _PPR_OBJECT_SCRIPT = "_pprObjectScript";
  private static final String _STORED_DATA_KEY =
    "org.apache.myfaces.trinidadinternal.renderkit.core.STORED_PPR_DATA";
  private static final ADFLogger _LOG = ADFLogger.createADFLogger(
     ScriptBufferingResponseWriter.class);

}
