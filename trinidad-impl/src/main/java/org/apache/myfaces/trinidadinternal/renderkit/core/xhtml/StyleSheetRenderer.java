/*
 * Copyright  2001-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.CoreStyleSheet;

import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;

import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;

/**
 * Renderer for meta data section of the document--a.k.a <head>.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/StyleSheetRenderer.java#0 $) $Date: 10-nov-2005.19:02:29 $
 * @author The Oracle ADF Faces Team
 */
public class StyleSheetRenderer extends XhtmlRenderer
{ 
  /**
   * Disables optimizations that are normally performed by the
   * ADF Renderers to reduce content size.
   * <p>
   * This Boolean property controls whether or not ADF Renderer
   * implementations should attempt to reduce the size of generated
   * content, for example, by compressing style class names.  These
   * optimizations are enabled by default.  In general,
   * clients should not need to disable these optimizations.  However,
   * clients that want to disable this functionality for testing or
   * debugging purposes can do so by setting this property to Boolean.TRUE.
   */
  static public final String DISABLE_CONTENT_COMPRESSION =
    "org.apache.myfaces.trinidadinternal.DISABLE_CONTENT_COMPRESSION";

  public StyleSheetRenderer()
  {
    this(CoreStyleSheet.TYPE);
  }

  protected StyleSheetRenderer(FacesBean.Type type)
  {
    super(type);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    StyleContext sContext = arc.getStyleContext();
    StyleProvider provider = sContext.getStyleProvider();
    if (provider != null)
    {
      String href = provider.getStyleSheetURI(sContext);
      if (href != null)
      {
        String contextUri = context.getExternalContext().getRequestContextPath();
        String baseURL = contextUri + XhtmlConstants.STYLES_CACHE_DIRECTORY;
        
        String outputMode = arc.getOutputMode();
        // =-=AEW Don't like hardcoding facet names...
        if (XhtmlConstants.OUTPUT_MODE_PORTLET.equals(outputMode) &&
            supportsScripting(arc))
        {
          writer.startElement("script", null);
          writer.writeText("var _adfSS;if(!_adfSS){_adfSS=1;document.write(\"" +
                        "<link rel=\\\"stylesheet\\\" "+
                        "charset=\\\"UTF-8\\\" type=\\\"text/css\\\" " +
                        "href=\\\"",
						null);
          writer.writeText(baseURL, null);
          writer.writeText(href, null);
          writer.writeText("\\\">\")}", null);
          writer.endElement("script");
        }
        else
        {
          writer.startElement("link", null);
          renderId(context, comp);
          writer.writeAttribute("rel", "stylesheet", null);
          writer.writeAttribute("charset", "UTF-8", null);
          
          String type = provider.getContentStyleType(sContext);
          writer.writeAttribute("type", type, null);
          
          writer.writeAttribute("href", baseURL + href, null);
          writer.endElement("link");
        }
      }
      else
      {
        if (arc.getSkin() == null)
          writer.writeComment("ERROR: Could not create stylesheet, because " +
                              "no skin is available");
        else
          writer.writeComment("ERROR: could not create stylesheet for " +
                              arc.getSkin().getStyleSheetName());
      }


      // Hand the Faces-major renderers the style Map for compressing.
      // Oddly enough, this code has to be after provider.getStyleSheetURI(),
      // because that call boostraps up the style provider in general.
      if (!"true".equals(
          context.getExternalContext().getInitParameter(
                                 DISABLE_CONTENT_COMPRESSION)))
      {
        if (arc instanceof CoreRenderingContext)
        {
          Map<String, String> shortStyles = 
            StyleUtils.getShortStyleClasses(sContext, provider);
          
          ((CoreRenderingContext) arc).setStyleMap(shortStyles);
        }
      }
    }
  }

}
