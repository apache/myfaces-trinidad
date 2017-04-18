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
package org.apache.myfaces.trinidadinternal.io;


import java.io.StringWriter;

import javax.faces.component.html.HtmlInputText;
import javax.faces.context.ResponseWriter;
import javax.faces.render.Renderer;

import org.apache.myfaces.test.el.MockValueExpression;
import org.apache.myfaces.trinidadbuild.test.FacesTestCase;
import org.junit.Assert;

public class HtmlResponseWriterTest extends FacesTestCase
{

    private StringWriter _stringWriter;
    private ResponseWriter _responseWriter;

    public HtmlResponseWriterTest(String testName)
    {
        super(testName);
    }

    @Override
    public void setUp() throws Exception
    {
        super.setUp();
        _stringWriter = new StringWriter();
        _responseWriter = new HtmlResponseWriter(_stringWriter, "UTF-8");
    }

    public void testWithoutPassThroughAttribute() throws Exception
    {
        _responseWriter.startDocument();
        HtmlInputText inputText = new HtmlInputText();
        _responseWriter.startElement("div", inputText);
        _responseWriter.startElement("div", null);
        _responseWriter.startElement("input", null);
        _responseWriter.writeAttribute("name", "test", null);
        _responseWriter.endElement("input");
        _responseWriter.endElement("div");
        _responseWriter.endElement("div");
        _responseWriter.endDocument();
        Assert.assertEquals("<div><div><input name=\"test\"></div></div>", _stringWriter.toString());
    }

    public void testSimplePassThroughAttribute() throws Exception
    {
        _responseWriter.startDocument();
        HtmlInputText inputText = new HtmlInputText();
        inputText.getPassThroughAttributes().put("data-test", "test");
        _responseWriter.startElement("div", inputText);
        _responseWriter.startElement("div", null);
        _responseWriter.startElement("input", null);
        _responseWriter.writeAttribute("name", "test", null);
        _responseWriter.endElement("input");
        _responseWriter.endElement("div");
        _responseWriter.endElement("div");
        _responseWriter.endDocument();
        Assert.assertEquals("<div data-test=\"test\"><div><input name=\"test\"></div></div>", _stringWriter.toString());
    }

    public void testValueExpressionPassThroughAttribute() throws Exception
    {
        externalContext.getRequestMap().put("test", Boolean.TRUE);
        _responseWriter.startDocument();
        HtmlInputText inputText = new HtmlInputText();
        inputText.getPassThroughAttributes().put("data-test", new MockValueExpression("#{test}", Boolean.TYPE));
        _responseWriter.startElement("div", inputText);
        _responseWriter.startElement("div", null);
        _responseWriter.startElement("input", null);
        _responseWriter.writeAttribute("name", "test", null);
        _responseWriter.endElement("input");
        _responseWriter.endElement("div");
        _responseWriter.endElement("div");
        _responseWriter.endDocument();
        Assert.assertEquals("<div data-test><div><input name=\"test\"></div></div>", _stringWriter.toString());
    }

    public void testRendererLocalNamePassThroughAttribute() throws Exception
    {
        _responseWriter.startDocument();
        HtmlInputText inputText = new HtmlInputText();
        inputText.getPassThroughAttributes().put(Renderer.PASSTHROUGH_RENDERER_LOCALNAME_KEY, "test");
        _responseWriter.startElement("div", inputText);
        _responseWriter.startElement("div", null);
        _responseWriter.startElement("input", null);
        _responseWriter.writeAttribute("name", "test", null);
        _responseWriter.endElement("input");
        _responseWriter.endElement("div");
        _responseWriter.endElement("div");
        _responseWriter.endDocument();
        Assert.assertEquals("<test><div><input name=\"test\"></div></test>", _stringWriter.toString());
    }
}
