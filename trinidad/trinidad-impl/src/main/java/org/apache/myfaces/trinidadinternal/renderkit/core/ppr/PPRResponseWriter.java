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

import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.ExtendedRenderKitService;
import org.apache.myfaces.trinidad.util.Service;

import org.apache.myfaces.trinidadinternal.renderkit.core.pages.GenericEntry;

/**
 * Write out a PPR response in the following form:
 * <content action="http://new-action-url">
 *   <fragment><![CDATA[....html....]]></fragment>
 *   <fragment><![CDATA[....more html....]]></fragment
 * </content>
 *
 * TODO: write out fragments only once we've detected the
 * ID, to avoid sending unnecessary fragments
 */
public class PPRResponseWriter extends ScriptBufferingResponseWriter
{
  public PPRResponseWriter(ResponseWriter     out,
                           RenderingContext   rc)
  {
    super(out);
    PartialPageContext pprContext = rc.getPartialPageContext();
    if (!(pprContext instanceof PartialPageContextImpl))
        throw new IllegalArgumentException();

    _state = new State((PartialPageContextImpl) pprContext);
    _xml        = new XmlResponseWriter(out, out.getCharacterEncoding());
  }

  /**
   * Constructor for use when cloning.
   */
  PPRResponseWriter(ResponseWriter out,
                    PPRResponseWriter base)
  {
    super(out, base);
    // New XmlResponseWriter
    _xml        = new XmlResponseWriter(out, out.getCharacterEncoding());
    // But the rest of the state is shared
    _state = base._state;
  }
  
  
  public ResponseWriter cloneWithWriter(Writer writer)
  {
    PPRResponseWriter ppr = new PPRResponseWriter(
      getResponseWriter().cloneWithWriter(writer),
      this);

    // BIG HACK: The JSF 1.1
    // ViewTag is going to clone the ResponseWriter so that
    // it can write its BodyContent out.  But that'll clone a
    // PPRResponseWriter, which then will adamantly refuse to write
    // out any content!  To "fix" this, we make the lousy assumption
    // that if we're being cloned, and our element depth is zero,
    // then we're probably being used in this way, and we should just
    // let everything through.
    // HOWEVER, in Facelets, cloneWithWriter() is called eaaaaarly.
    // Catch that by ignoring this unless we've at least had a call
    // to startDocument()
    _state.forceInsideTarget = (_state.documentStarted &&
                                 (_state.elementDepth == 0));
    return ppr;
  }

  public void startDocument() throws IOException
  {
    // Mark that we've started the document, so cloneWithWriter()
    // can start activating "forceInsideTarget" correctly
    _state.documentStarted = true;

    // Mark that we're in the middle of starting the document,
    // so that _disableIfNeeded() can do its thing - see that
    // function for more info
    _state.documentStarting = true;

    // We force the encoding to be text/xml in XmlHttpServletResponse
    // because setContentType is ignored when inside an included page (bug 5591124)
    _xml.startDocument();
    
    // Stick another PI indicating that this is a rich reponse
    // Used for an Iframe based communication channel, since it cannot 
    // read response headers
    super.write("<?Tr-XHR-Response-Type ?>\n");

    _xml.startElement("content", null);
    String viewId = _facesContext.getViewRoot().getViewId();
    // HACK: don't write out an "action" for PPR on a GenericEntry page
    // (basically entirely for the InlineDatePicker case)
    if (!GenericEntry.getViewId().equals(viewId))
    {
      String actionURL = _facesContext.getApplication().
        getViewHandler().getActionURL(_facesContext, viewId);
      ExternalContext external = _facesContext.getExternalContext();
      
      _xml.writeURIAttribute("action", external.encodeActionURL(actionURL), null);
    }

    // TODO: Portlet support for PPR?

    _xml.writeText(" ", null);

    _state.documentStarting = false;
  }

  public void endDocument() throws IOException
  {
    // Write out any buffered <script src=""> or inline scripts
    writeBufferedScripts();
    
    // Write out all of the framework-level scripts
    writeFrameworkScripts();
    
    _xml.endElement("content");
    _xml.endDocument();

    // Force "inside target mode" - this is for Facelets,
    // where the state is rendered after endDocument() is called
    _state.forceInsideTarget = true;
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
    _disableIfNeeded();
    if (_isInsideTarget())
      super.write(text);
  }

  public void write(
                    char[]      text,
                    int         start,
                    int         length) throws IOException
  {
    _disableIfNeeded();
    if (_isInsideTarget())
      super.write(text, start, length);
  }

  public void write(int ch) throws IOException
  {
    if (_isInsideTarget())
      super.write(ch);
  }

  @Override
  public void write(char[] c) throws IOException
  {
    _disableIfNeeded();
    if (_isInsideTarget())
      super.write(c);
  }

  @Override
  public void write(String text, int off, int len) throws IOException
  {
    _disableIfNeeded();
    if (_isInsideTarget())
      super.write(text, off, len);
  }

  /* Needed in JSF 1.2
  @Override
  public void writeText(Object      text,
                        UIComponent component,
                        String      propertyName) throws IOException
  {
    if (_isInsideTarget() && (text != null))
      super.writeText(text, component, propertyName);
  }
  */

  public void startElement(String name, UIComponent component)
     throws IOException
  {
    _state.elementDepth++;
    _pushPartialTarget(component, name);
    
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
    _state.elementDepth--;
    if (_isInsideTarget())
      super.endElement(name);

    _popPartialTarget(name);
    super.flush();
  }

  public void writeAttribute(String     name,
                             Object     value,
                             String     property) throws IOException
  {
    if (value == null)
      return;

    // Write out attributes when we're inside a target and outputting
    // normally
    if (_isInsideTarget())
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
    // normally
    if (_isInsideTarget())
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
  
  /*
   * Allows subclasses to retrieve the underlying response writer and bypass PPR logic
   * allowing content to be written only by partial targets
   * @return underlying ResponseWriter
   */
  protected ResponseWriter getXmlResponseWriter()
  {
    return _xml;
  }
  
  /*
   * Writes out buffered inline scripts and script libraries
   */
  protected void writeBufferedScripts() throws IOException
  {
    List<String> libraries = getBufferedLibraries();
    if (libraries != null)
    {
      for (String library : libraries)
      {
        _xml.startElement("script-library", null);
        _xml.writeText(library, null);
        _xml.endElement("script-library");
      }
    }

    List<String> scripts = getBufferedScripts();
    if (scripts != null)
    {
      for (String script : scripts)
      {
        _xml.startElement("script", null);
        _xml.write("<![CDATA[");
        _xml.write(script);
        _xml.write("]]>");
        _xml.endElement("script");
      }
    }

    // Clear out any buffered scripts/libraries
    clearBufferedContents();
  }
  
  /*
   * Writes out framework-level scripts
   */
  protected void writeFrameworkScripts() throws IOException
  {
    ResponseWriter old = _facesContext.getResponseWriter();
    _facesContext.setResponseWriter(_xml);
    try
    {
      // And also encode ExtendedRenderKitService scripts.  (ERKS
      // renders its own script wrapper element.)
      ExtendedRenderKitService erks =
        Service.getService(_facesContext.getRenderKit(),
                           ExtendedRenderKitService.class);
      if (erks != null)
        erks.encodeScripts(_facesContext);
    }
    finally
    {
      _facesContext.setResponseWriter(old);
    }
  }

  private void _pushPartialTarget(
    UIComponent component,
    String      elementName)
    throws IOException
  {
    PPRTag tag = null;

    // If we're already inside a target, don't bother
    if (!_isInsideTarget())
    {
      if (component != null)
      {
        String clientId = component.getClientId(_facesContext);
        if (_state.pprContext.isPartialTarget(clientId))
        {
          tag = new PPRTag(clientId);
          _state.enteringPPR = component;
        }
      }
    }

    if (tag != null)
    {
      super.flush();
      tag.start(_state.pprContext, elementName);
    }

    _state.componentStack.add(tag);
  }

  private void _popPartialTarget(String elementName) throws IOException
  {
    List<PPRTag> componentStack = _state.componentStack;
    int pos = componentStack.size() - 1;
    PPRTag tag = (PPRTag) componentStack.get(pos);
    componentStack.remove(pos);

    if (tag != null)
      tag.finish(_state.pprContext, elementName);
  }

  private boolean _isInsideTarget()
  {
    // Only use the real ResponseWriter when we are rendering
    // a partial target subtree.  Otherwise, we discard all
    // output.
    return _state.forceInsideTarget ||
           _state.pprContext.isInsidePartialTarget();
           
  }

  
  //
  // Facelets - as of version 1.1.11 - does something
  // very strange with its ResponseWriter stack.
  // It starts with an ordinary stack (which will have 
  // a PPRResponseWriter around an HtmlResponseWriter).
  // Then it treats that *as an ordinary Writer*, and
  // wraps it in a "StateWriter" class, which merely
  // is used to switch between passing output through
  // and buffering it.  Then it takes that StateWriter
  // and uses it to cloneWithWriter() a new ResponseWriter
  // stack!  As a result, we have the following stack
  // outermost to innermost:
  //   PPRResponseWriter
  //   HtmlResponseWriter
  //   StateWriter
  //   PPRResponseWriter
  //   HtmlResponseWriter
  //   ServletResponse's Writer
  // In the end, we have to get that "inner" PPRResponseWriter
  // to just cut it out and pass everything through.  Hence,
  // this hack!  So If I get a "write" call while we're
  // inside of startDocument(), assume that I must be an
  // abused PPRResponseWriter, and just put myself in
  // pass-everything-through mode
  //
  private void _disableIfNeeded()
  {
    if (_state.documentStarting)
    {
      _state = new State(_state.pprContext);
      _state.forceInsideTarget = true;
    }
  }

  private void _handleIdAttribute(String name, Object value)
  {
    if ((_state.enteringPPR != null) && "id".equals(name))
    {
      if (_LOG.isFine())
      {
        _LOG.fine("Using id {1} for element of {0}",
                  new Object[]{_state.enteringPPR, value});
      }

      _state.pprContext.addRenderedPartialTarget(value.toString());
      _state.enteringPPR = null;
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

    public void start(
      PartialPageContextImpl pprContext,
      String             elementName) throws IOException
    {
      if (_id != null)
      {
        pprContext.pushRenderedPartialTarget(_id);
        _xml.startElement("fragment",null);
        _xml.write("<![CDATA[");
        _xml.flush(); // NEW

        if (_LOG.isFine())
        {
          _LOG.fine("Entering partial target id {0}", _id);
        }

        // Write out wrapper elements needed to ensure valid HTML
        _startWrapperElements(elementName);
      }
    }

    public void finish(
      PartialPageContextImpl pprContext,
      String             elementName) throws IOException
    {
      if (_id != null)
      {
        if (_state.enteringPPR != null)
        {
          _LOG.warning("NO_PPR_CAPABLE_ID_FOUND_FOR_COMPONENT",
                       _state.enteringPPR);
          _state.enteringPPR = null;
        }

        // Close up wrapper elements needed to ensure valid HTML
        _endWrapperElements(elementName);
        
        PPRResponseWriter.super.flush();
      
        _xml.write("]]>");
        _xml.endElement("fragment");
        _xml.flush();
        
        pprContext.popRenderedPartialTarget();
        _LOG.finer("Leaving partial target id {0}", _id);
      }
    }

    // Write out wrapper elements needed to generate valid HTML content
    private void _startWrapperElements(String elementName) throws IOException
    {
      // If the partial target has <tr>/<td> as its root element, we
      // need to wrap these contents such that the root element is
      // a <table>.  This allows us to shove this content into a
      // div as innerHTML on the client to produce the DOM tree.  Without
      // this, the resulting HTML is invalid and the DOM tree is not
      // produced.
      boolean isTR = "tr".equalsIgnoreCase(elementName);
      boolean isTD = "td".equalsIgnoreCase(elementName);
      
      if (isTR || isTD)
      {
        PPRResponseWriter.super.startElement("table", null);

        if (isTD)
          PPRResponseWriter.super.startElement("tr", null);
      }      
    }
    
    // Close up wrapper elements needed to generate valid HTML content
    private void _endWrapperElements(String elementName) throws IOException
    {
      // Close up <table>/<tr> tags if necessary.
      boolean isTR = "tr".equalsIgnoreCase(elementName);
      boolean isTD = "td".equalsIgnoreCase(elementName);
      if (isTR || isTD)
      {
        if (isTD)
          PPRResponseWriter.super.endElement("tr");

        PPRResponseWriter.super.endElement("table");
      }
    }
    
    private String _id;
  }

  
  private State _state;
  private ResponseWriter _xml;
  private final FacesContext _facesContext =
   FacesContext.getCurrentInstance();

  static private class State
  {
    public State(PartialPageContextImpl pprContext)
    {
      this.pprContext = pprContext;
    }

    public UIComponent    enteringPPR;
    public boolean       forceInsideTarget;
    public int           elementDepth;
    public boolean       documentStarted;
    public boolean       documentStarting;
    public final List<PPRTag> componentStack = new ArrayList<PPRTag>(50);
    public final PartialPageContextImpl pprContext;
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(PPRResponseWriter.class);
}
