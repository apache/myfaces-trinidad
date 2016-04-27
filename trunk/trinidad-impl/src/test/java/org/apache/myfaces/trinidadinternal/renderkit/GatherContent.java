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
package org.apache.myfaces.trinidadinternal.renderkit;

import java.io.IOException;
import java.io.Writer;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.component.UIComponent;
import javax.faces.component.UIComponentBase;

import org.apache.myfaces.trinidad.context.RenderingContext;

import junit.framework.AssertionFailedError;
import junit.framework.TestResult;
import junit.framework.Test;

import org.apache.myfaces.trinidadinternal.io.ResponseWriterDecorator;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.HiddenLabelUtils;

public class GatherContent extends UIComponentBase
{
  @SuppressWarnings("unchecked")
  public GatherContent(
    Writer      writer,
    UIComponent child,
    TestResult result,
    Test       test,
    boolean    lenient)
  {
    _writer = writer;
    _result = result;
    _child  = child;
    _test   = test;
    _lenient = lenient;
    getChildren().add(child);
  }

  @Override
  public String getFamily()
  {
    return "org.apache.myfaces.trinidadtest.GatherContent";
  }

  @Override
  public void encodeAll(FacesContext context)
  {
    try
    {
      super.encodeAll(context);
    }
    catch (Throwable t)
    {
      Throwable failure = new Throwable("Exception during rendering", t);
      _result.addError(_test, failure);
      
    }
  }
  
  @Override
  public void encodeBegin(FacesContext context)
  {
    _savedWriter = context.getResponseWriter();
    
    ResponseWriter wrappedWriter = _savedWriter.cloneWithWriter(_writer);
    
    if (!_lenient)
      wrappedWriter = new CheckUIComponent(wrappedWriter);
        
    context.setResponseWriter(wrappedWriter);
  }

  @Override
  public void encodeEnd(FacesContext context)
  {
    // check that we have balanced elements in the CheckUIComponent
    if (!_lenient)
    {
      ResponseWriter wrappedWriter = context.getResponseWriter();
      
      if (wrappedWriter instanceof CheckUIComponent)
      {
        int finishingDepth = ((CheckUIComponent)wrappedWriter).getDepth();
        
        if (finishingDepth != 0)
        {
          AssertionFailedError failure = new AssertionFailedError("Elements not completely popped during rendering");
          
          _result.addError(_test, failure);          
        }
      }
    }
    
    context.setResponseWriter(_savedWriter);
    _savedWriter = null;
    
    RenderingContext arc = RenderingContext.getCurrentInstance();

    // Our hidden label utility code makes sure it never writes out
    // the same label twice consecutively - but between batches of
    // gathered content, that's a bad thing
    HiddenLabelUtils.rememberLabel(arc, "forgetTheLabel");
  }

  private class CheckUIComponent extends ResponseWriterDecorator
  {
    public CheckUIComponent(ResponseWriter out)
    {
      super(out);
    }

    public int getDepth()
    {
      return _depth;
    }

    @Override
    public void startElement(String name,
                             UIComponent component) throws IOException
    {
      // Look for root elements that do not have the right associated
      // UIComponent.  However, ignore <script> elements, since
      // those are often output as a dependency that isn't
      // directly attached to this component.
      // =-=AEW Is that right?  Aren't these latent bugs?
      int depth = _depth++;
      if (depth == 0)
      {
        if (!"script".equals(name) && (!"link".equals(name)))
        {
          if (component == null)
          {
            AssertionFailedError failure = new AssertionFailedError(
              "No UIComponent was passed to startElement(\"" + name +
              "\",...)");
            _result.addError(_test, failure);
          }
          else if (component != _child)
          {
            AssertionFailedError failure = new AssertionFailedError(
              "The wrong UIComponent was passed to startElement(\"" + name +
              "\",...)");
            _result.addError(_test, failure);
          }
        }
      }

      super.startElement(name, component);
    }

    @Override
    public void endElement(String name) throws IOException
    {
      super.endElement(name);

      _depth--;
    }

    private int _depth = 0;
  }

  private final UIComponent _child;
  private final TestResult _result;
  private final Test       _test;
  private final boolean    _lenient;
  private final Writer _writer;
  private ResponseWriter _savedWriter;
}
