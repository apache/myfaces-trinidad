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
package org.apache.myfaces.trinidaddemo.support.jsf;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.OutputFormattedRenderer;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidaddemo.support.util.HighlightXhtmlSource;
import org.apache.myfaces.trinidaddemo.support.util.HighlightJavaSource;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.component.UIComponent;
import javax.servlet.ServletContext;

import com.uwyn.jhighlight.renderer.Renderer;

import java.io.IOException;
import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.FileInputStream;

/**
 *
 */
public class OutputSourceRenderer extends OutputFormattedRenderer {

    @Override
    protected void encodeAll(FacesContext context, RenderingContext arc, UIComponent comp, FacesBean bean) throws IOException {
        if (canSkipRendering(context, arc, comp)) {
            return;
        }

        String path = (String)getValue(bean);
        String sourceType = path.substring(path.lastIndexOf('.') + 1, path.length());

        Renderer hlight;

        if (sourceType.equals("xhtml")) {
            hlight = new HighlightXhtmlSource();
        }
        else if (sourceType.equals("java")) {
            hlight = new HighlightJavaSource();
        }
        else {
            hlight = null;
        }

        ResponseWriter rw = context.getResponseWriter();
        rw.startElement("span", comp);

        renderId(context, comp);
        renderAllAttributes(context, arc, bean);

        renderSourceHighlighted(context, (OutputSource)comp, bean, hlight);

        rw.endElement("span");
    }

    /**
     * @param context
     * @param outputSource
     * @param bean
     * @param hlight
     * @throws IOException
     */
    protected void renderSourceHighlighted(FacesContext context, OutputSource outputSource, FacesBean bean, Renderer hlight) throws IOException {
        String sourceContent = readSource(context, outputSource, bean);

        ResponseWriter rw = context.getResponseWriter();

        rw.write("<span class=\"line_counter\">");

        for(int i=0;i<sourceContent.split("\n").length;i++)
            rw.write("<span>"+(i+1)+"</span><br/>");

        rw.write("</span><span class=\"code_body_wrap\"><span class=\"code_body\">");
        rw.write(hlight.highlight(null, sourceContent, "UTF-8", true));
        rw.write("</span></span>");
    }

    /**
     * @param context
     * @param outputSource
     * @param bean
     * @return
     */
    protected String readSource(FacesContext context, OutputSource outputSource, FacesBean bean) {
        String path = (String)getValue(bean);

        String pathPrefix = outputSource.getPathPrefix();
        if (pathPrefix == null) {
            pathPrefix = "";
        }

        String realPath = ((ServletContext)context.getExternalContext().getContext()).getRealPath(pathPrefix + path);

        String source = "";
        InputStream in = null;

        try {
            in = new BufferedInputStream(new FileInputStream(realPath));
            int ch;
            while ((ch = in.read()) !=-1) {
               source += ((char)ch);
            }
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        finally {
            if (in != null)
                try {
                    in.close();
                }
                catch (IOException e) {
                    e.printStackTrace();
                }
        }

        return source;
    }
}
