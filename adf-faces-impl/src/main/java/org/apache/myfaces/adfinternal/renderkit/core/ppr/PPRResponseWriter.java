/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.core.ppr;

import java.io.IOException;
import java.io.Writer;
import java.util.List;
import java.util.ArrayList;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adfinternal.io.ResponseWriterDecorator;

public class PPRResponseWriter extends ResponseWriterDecorator
{
  public PPRResponseWriter(ResponseWriter     out,
                           PartialPageContext pprContext,
                           boolean            useXMLDom)
  {
    super(out);
    _pprContext = pprContext;
    _useXMLDom = useXMLDom;
  }

  public ResponseWriter cloneWithWriter(Writer writer)
  {
    return new PPRResponseWriter(
      getResponseWriter().cloneWithWriter(writer),
      _pprContext,
      _useXMLDom);
  }

  public void startDocument() throws IOException
  {
    if (_isInsideTarget())
      super.startDocument();
  }

  public void endDocument() throws IOException
  {
    if (_isInsideTarget())
      super.endDocument();
  }

  public void flush() throws IOException
  {
    if (_isInsideTarget())
      super.flush();
  }

  public void writeComment(Object text) throws IOException
  {
    if (_isInsideTarget())
      super.writeComment(text);
  }


  public void writeText(Object text, String property) throws IOException
  {
    if (_isInsideTarget())
      super.writeText(text, property);
  }

  public void writeText(
                        char[]      text,
                        int         start,
                        int         length) throws IOException
  {
    if (_isInsideTarget())
      super.writeText(text, start, length);
  }


  public void write(String text) throws IOException
  {
    if (_isInsideTarget())
      super.write(text);
  }

  public void write(
                    char[]      text,
                    int         start,
                    int         length) throws IOException
  {
    if (_isInsideTarget())
      super.write(text, start, length);
  }

  public void write(int ch) throws IOException
  {
    if (_isInsideTarget())
      super.write(ch);
  }

  public void startElement(String name, UIComponent component)
     throws IOException
  {
    if (_pushPartialTarget(name, component))
    {
      _enteringPPR = component;
    }

    if (_isInsideTarget())
    {
      if (_LOG.isFinest())
      {
        _LOG.finest("PPR: Using element {0} in component {1}",
                    new Object[]{name, component});
      }

      super.startElement(name, component);
    }
    else
    {
      if (_LOG.isFinest())
      {
        _LOG.finest("PPR: Ignoring element {0} in component {1}",
                    new Object[]{name, component});
      }
    }
  }


  public void endElement(String name) throws IOException
  {
    if (_isInsideTarget())
    {
      super.endElement(name);
    }

    _popPartialTarget();
  }

  public void writeAttribute(String     name,
                             Object     value,
                             String     property) throws IOException
  {
    if (value == null)
      return;

    // Write out attributes when we're inside a target and outputting
    // normally - or if we're writing out a form at all, because
    // the action attribute is important
    if (_isInsideTarget() || _writingForm)
    {
      _handleIdAttribute(name, value);
      super.writeAttribute(name, value, property);
    }
  }

  public void writeURIAttribute(
                                String     name,
                                Object     value,
                                String     property) throws IOException
  {
    // Write out attributes when we're inside a target and outputting
    // normally - or if we're writing out a form at all, because
    // the action attribute is important
    if (_isInsideTarget() || _writingForm)
    {
      // We actually use writeURIAttribute() to write out the "id"
      // of our links, because "name" is actually kind of a URI
      // property, and "id" is required to be the same as "name".
      // A strange decision that should be revisited, but for now,
      // trap writeURIAttribute() too
      _handleIdAttribute(name, value);
      super.writeURIAttribute(name, value, property);
    }
  }

  private boolean _pushPartialTarget(String element, UIComponent component)
    throws IOException
  {
    _writingForm = false;
    boolean enteringPPR = false;

    PPRTag tag = null;

    // If we're already inside a target, don't bother
    if (!_isInsideTarget())
    {
      if (component != null)
      {
        String clientId = component.getClientId(_facesContext);
        if (_pprContext.isPartialTarget(clientId))
        {
          if ("td".equals(element))
            tag = new AddTags(clientId, _ADD_TABLE_AND_TR);
          else if ("tr".equals(element))
            tag = new AddTags(clientId, _ADD_TABLE);
          else
            tag = new PPRTag(clientId);
          enteringPPR = true;
        }
      }

      if ((tag == null) && "form".equals(element))
      {
        // Remember when we're writing out a form that isn't a true target
        _writingForm = true;
        tag = new AddTags(null, _ADD_FORM);
      }
    }

    if (tag != null)
      tag.start(_pprContext);

    _componentStack.add(tag);

    return enteringPPR;
  }

  private void _popPartialTarget() throws IOException
  {
    int pos = _componentStack.size() - 1;
    PPRTag tag = (PPRTag) _componentStack.get(pos);
    _componentStack.remove(pos);

    if (tag != null)
      tag.finish(_pprContext);
  }

  private boolean _isInsideTarget()
  {
    // Only use the real ResponseWriter when we are rendering
    // a partial target subtree.  Otherwise, we discard all
    // output.
    return _pprContext.isInsidePartialTarget();
  }

  private void _handleIdAttribute(String name, Object value)
  {
    if ((_enteringPPR != null) && "id".equals(name))
    {
      if (_LOG.isFine())
      {
        _LOG.fine("Using id {1} for element of {0}",
                  new Object[]{_enteringPPR, value});
      }

      _pprContext.addRenderedPartialTarget(value.toString());
      _enteringPPR = null;
    }
  }

  //
  // Class representing PPR behavior associated with a tag.  The
  // base class simply tells PPR when it's working with a partial target
  //
  private class PPRTag
  {
    public PPRTag(String id)
    {
      _id = id;
    }

    public void start(PartialPageContext pprContext) throws IOException
    {
      if (_id != null)
      {
        pprContext.pushRenderedPartialTarget(_id);

        if (_useXMLDom)
        {
          PPRResponseWriter.super.startElement("ppr",null);
          PPRResponseWriter.super.writeAttribute("target_id", _id,null);
          PPRResponseWriter.super.write("<![CDATA[");
        }

        if (_LOG.isFine())
        {
          _LOG.fine("Entering partial target id {0}", _id);
        }

      }
    }

    public void finish(PartialPageContext pprContext) throws IOException
    {
      if (_id != null)
      {
        if (_enteringPPR != null)
        {
          _LOG.warning("No PPR-capable 'id' found for elements of {0}."+
                       " This component has not written-out an 'id' attribute.",
                       _enteringPPR);
          _enteringPPR = null;
        }

        if (_useXMLDom)
        {
          PPRResponseWriter.super.write("]]>");
          PPRResponseWriter.super.endElement("ppr");
        }

        pprContext.popRenderedPartialTarget();
        _LOG.finer("Leaving partial target id {0}", _id);
      }
    }

    private String _id;
  }

  //
  // Subclass adding support for automatic output of tags
  //
  private class AddTags extends PPRTag
  {
    public AddTags(String id, String[] tags)
    {
      super(id);
      _tags = tags;
    }

    public void start(PartialPageContext pprContext) throws IOException
    {
      super.start(pprContext);

      // Start the elements in order...
      for (int i = 0; i < _tags.length; i++)
      {
        PPRResponseWriter.super.startElement(_tags[i], null);
      }
    }

    public void finish(PartialPageContext pprContext) throws IOException
    {
      // And then end them in reverse order...
      for (int i = _tags.length - 1; i >= 0; i--)
      {
        PPRResponseWriter.super.endElement(_tags[i]);
      }

      super.finish(pprContext);
    }

    private String[] _tags;
  }

  private UIComponent _enteringPPR;

  private boolean _writingForm;
  private final boolean _useXMLDom;
  private final List _componentStack = new ArrayList(50);
  private final PartialPageContext _pprContext;
  private final FacesContext _facesContext =
     FacesContext.getCurrentInstance();

  static private final String[] _ADD_TABLE_AND_TR =
     new String[]{"table", "tr"};
  static private final String[] _ADD_TABLE =
     new String[]{"table"};
  static private final String[] _ADD_FORM =
     new String[]{"form"};

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(PPRResponseWriter.class);
}
