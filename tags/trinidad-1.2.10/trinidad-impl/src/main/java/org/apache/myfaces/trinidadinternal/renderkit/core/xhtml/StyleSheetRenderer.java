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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.CoreStyleSheet;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;


/**
 * Renderer for meta data section of the document--a.k.a &lt;head&gt;.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/StyleSheetRenderer.java#0 $) $Date: 10-nov-2005.19:02:29 $
 */
public class StyleSheetRenderer extends XhtmlRenderer
{
  /**
   * Disables optimizations that are normally performed by the
   * Trinidad Renderers to reduce content size.
   * <p>
   * This Boolean property controls whether or not Trinidad Renderer
   * implementations should attempt to reduce the size of generated
   * content, for example, by compressing style class names.  These
   * optimizations are enabled by default.  In general,
   * users should not need to disable these optimizations.  However,
   * users who want to build custom skins for Trinidad will find this
   * setting essential.  Use Boolean.TRUE to disable compression.
   */
  static public final String DISABLE_CONTENT_COMPRESSION =
    "org.apache.myfaces.trinidad.DISABLE_CONTENT_COMPRESSION";

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

    StyleContext sContext = ((CoreRenderingContext) arc).getStyleContext();
    StyleProvider provider = sContext.getStyleProvider();
    if (provider != null)
    {
      List<String> uris = provider.getStyleSheetURIs(sContext);

      // If the requestMap has a skin-id, a skin's stylesheet's id and suppressStylesheet
      // is true, and the skin information matches our current skin, then it is safe
      // to not write out the css. This means that it will be written out by the external
      // source, like the portal container.
      boolean suppressStylesheet = _isSuppressStylesheet(context, arc);
      if (!suppressStylesheet)
      {
        if (uris != null && !uris.isEmpty())
        {
          ExternalContext externalContext = context.getExternalContext();
          String contextUri = externalContext.getRequestContextPath();
          String baseURL = contextUri + XhtmlConstants.STYLES_CACHE_DIRECTORY;

          String outputMode = arc.getOutputMode();
          // =-=AEW Don't like hardcoding facet names...
          if (XhtmlConstants.OUTPUT_MODE_PORTLET.equals(outputMode) &&
              supportsScripting(arc))
          {
            writer.startElement("script", null);
            writer.writeText("var _adfSS;if(!_adfSS){_adfSS=1;", null);
            for (String uri : uris)
            {
              writer.writeText("document.write(\"" +
                            "<link rel=\\\"stylesheet\\\" "+
                            "charset=\\\"UTF-8\\\" type=\\\"text/css\\\" " +
                            "href=\\\"",
                null);
              uri = context.getExternalContext().encodeResourceURL(baseURL + uri);
              writer.writeText(uri, null);
              writer.writeText("\\\">\");", null);
            }
            writer.writeText("}", null);
            writer.endElement("script");
          }
          else
          {
            for (String uri : uris)
            {
              writer.startElement("link", null);
              renderId(context, comp);
              writer.writeAttribute("rel", "stylesheet", null);
              writer.writeAttribute("charset", "UTF-8", null);

              String type = provider.getContentStyleType(sContext);
              writer.writeAttribute("type", type, null);

              renderEncodedResourceURI(context, "href", baseURL + uri);
              writer.endElement("link");
            }
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
      }


      // Hand the Faces-major renderers the style Map for compressing.
      // Oddly enough, this code has to be after provider.getStyleSheetURI(),
      // because that call boostraps up the style provider in general.
      if (arc instanceof CoreRenderingContext)
      {
        Map<String, String> shortStyles = arc.getSkin().getStyleClassMap(arc);
        ((CoreRenderingContext) arc).setStyleMap(shortStyles);
      }
    }
  }

  // returns true if we want to suppress the stylesheet.
  // It checks for suppressStylesheet flag on the request map and
  // for the skin-id on the request map to exist identically on the server.
  //
  // This is usually called in a portal environment when we want to suppress the
  // producer (portlet)'s stylesheet and use the consumer (portal container)'s
  // instead for performance enhancements.
  private boolean _isSuppressStylesheet(FacesContext context,  RenderingContext arc)
  {

    Map<String, Object> requestMap = context.getExternalContext().getRequestMap();

    boolean suppressStylesheet = "true".equals(requestMap.get(_SUPPRESS_STYLESHEET_ID_PARAM));
    if (suppressStylesheet)
    {
      // getRequestMapSkin --
      // See if a skin-id is requested on the requestMap. If so, then see if the Skin
      // with that id exists on the server, and if it does, and if it is an exact match
      // (styleSheetDocumentIds match), then it returns the Skin. Otherwise, it returns null
      Skin requestMapSkin = ((CoreRenderingContext) arc).getRequestMapSkin();
      return (requestMapSkin != null) ? true : false;
    }
    return false;
  }

  static private final String _SUPPRESS_STYLESHEET_ID_PARAM =
    "org.apache.myfaces.trinidad.skin.suppressStylesheet";

}
