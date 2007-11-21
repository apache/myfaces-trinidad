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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.render.RenderKit;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.ExtendedRenderKitService;
import org.apache.myfaces.trinidad.util.Service;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.PPRResponseWriter;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.ScriptBufferingResponseWriter;


/**
 * Renderer for the panelPartialRoot.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/PanelPartialRootRenderer.java#0 $) $Date: 10-nov-2005.19:01:40 $
 */
public class PanelPartialRootRenderer extends XhtmlRenderer
{
  protected PanelPartialRootRenderer(FacesBean.Type type)
  {
    super(type);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  protected void renderContent(
    FacesContext        context,
    RenderingContext    arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    encodeAllChildren(context, component);
  }

  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    XhtmlUtils.addLib(context, arc, "openWindow()");

    if (PartialPageUtils.isPartialRenderingPass(arc))
    {
      // Mark that PPR is in fact active
      PartialPageUtils.markPPRActive(context);

      try
      {
        renderContent(context, arc, component, bean);
      }
      // For RuntimeExceptions and Errors, make sure we don't
      // just drop the error on the ground during PPR requests.
      // ViewHandler.renderView() would be a much, much better place to
      // put this code.  But sadly, ServletExceptions generally
      // swallow whatever they contain instead of exposing it
      // as a cause (at least in OC4J 9.0.4 and Tomcat 5.0)
      catch (RuntimeException re)
      {
        _LOG.severe("ERR_PARTIAL_PAGE_RENDERING", re);
        throw re;
      }
      catch (Error error)
      {
        _LOG.severe("ERR_PARTIAL_PAGE_RENDERING", error);
        throw error;
      }

      renderAtEnd(context, arc);
    }
    else
    {
      boolean alreadyRenderedPPR = PartialPageUtils.isPPRActive(context);
      // @TODO: Find out the reason for the second half of this "or"
      if (!(alreadyRenderedPPR ||
            PartialPageUtils.isPartialRenderingPass(arc)))
      {
        // Render the iframe that we use to make partial page requests
        if (PartialPageUtils.supportsPartialRendering(arc))
        {
          PartialPageUtils.markPPRActive(context);
          renderPPRSupport(context, arc, component, bean);
        }
      }

      renderContent(context, arc, component, bean);
      renderAtEnd(context, arc);
    }
  }

  protected void renderAtEnd(
    FacesContext     context,
    RenderingContext arc) throws IOException
  {
  }

  protected boolean isEmbedded()
  {
    return true;
  }

  // Is this a partial page rendering pass?
  protected static boolean isPartialPass(RenderingContext arc)
  {
    return (PartialPageUtils.isPartialRenderingPass(arc));
  }

  protected void renderPPRSupport(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    // Render anything that will be needed to block clicks when the
    // partial render is in progress
    _renderPartialBlocking(context, arc, component);
  }


  // Renders the DIV element which is used to block user input during the
  // handling of a partial update.
  private static void _renderPartialBlocking(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component)
    throws IOException
  {
    if (PartialPageUtils.supportsBlocking(arc))
    {
      ResponseWriter writer = context.getResponseWriter();

      writer.startElement("div",  component);

      writer.writeAttribute("id", _PARTIAL_DIV_ID,
                            null);
      writer.writeAttribute("onclick",
                            _PARTIAL_DIV_CLICK_HANDLER, null);
      writer.writeAttribute("style",
                            _PARTIAL_DIV_STYLE, null);
      writer.writeAttribute("onkeydown", _PARTIAL_DIV_EAT_KEY_HANDLER, null);
      writer.writeAttribute("onkeyup", _PARTIAL_DIV_EAT_KEY_HANDLER, null);
      writer.writeAttribute("onmousedown", _PARTIAL_DIV_EAT_KEY_HANDLER, null);
      writer.writeAttribute("onmouseup", _PARTIAL_DIV_EAT_KEY_HANDLER, null);
      writer.writeAttribute("onkeypress", _PARTIAL_DIV_EAT_KEY_HANDLER, null);

      writer.endElement("div");
    }
  }



  // Div element used for blocking
  private static final String _PARTIAL_DIV_ID  = "tr_pprBlockingDiv";
  private static final String _PARTIAL_DIV_CLICK_HANDLER =
          "return _pprConsumeClick(event);";
  private static final String _PARTIAL_DIV_EAT_KEY_HANDLER = "return false;";
  private static final String _PARTIAL_DIV_STYLE =
          "position:absolute;left:0;top:0;width:0;height:0;cursor:wait;";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
   PanelPartialRootRenderer.class);
}
