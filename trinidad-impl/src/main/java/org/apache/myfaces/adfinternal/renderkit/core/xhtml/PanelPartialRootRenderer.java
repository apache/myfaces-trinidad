/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import javax.faces.render.RenderKit;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.component.core.layout.CorePanelPartialRoot;
import org.apache.myfaces.adf.render.ExtendedRenderKitService;
import org.apache.myfaces.adf.util.Service;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.renderkit.core.ppr.PPRResponseWriter;
import org.apache.myfaces.adfinternal.renderkit.core.ppr.ScriptBufferingResponseWriter;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.jsLibs.LibraryScriptlet;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.jsLibs.Scriptlet;
import org.apache.myfaces.adfinternal.renderkit.core.ppr.PartialPageContext;


/**
 * Renderer for the panelPartialRoot.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/PanelPartialRootRenderer.java#0 $) $Date: 10-nov-2005.19:01:40 $
 * @author The Oracle ADF Faces Team
 */
public class PanelPartialRootRenderer extends XhtmlRenderer
{
  public PanelPartialRootRenderer()
  {
    this(CorePanelPartialRoot.TYPE);
  }

  protected PanelPartialRootRenderer(FacesBean.Type type)
  {
    super(type);
  }

  public boolean getRendersChildren()
  {
    return true;
  }

  protected void renderContent(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    encodeAllChildren(context, component);
  }


  protected void encodeAll(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    XhtmlUtils.addLib(context, arc, "openWindow()");

    if (PartialPageUtils.isPartialRenderingPass(arc))
    {
      // Mark that PPR is in fact active
      PartialPageUtils.markPPRActive(context);
      PartialPageContext pprContext = arc.getPartialPageContext();

      XhtmlRenderer.enableScriptDeferring(arc, true);

      ResponseWriter saved = context.getResponseWriter();
      ScriptBufferingResponseWriter scriptBufferingWriter =
        new ScriptBufferingResponseWriter(context, saved);

      boolean useXMLDom = supportsXMLDOM(arc);
      ResponseWriter pprOut = new PPRResponseWriter(scriptBufferingWriter,
                                                    pprContext,
                                                    useXMLDom);

      context.setResponseWriter(pprOut);

      try
      {
        _encodeServiceScripts(context);

        renderContent(context, arc, component, bean);

        XhtmlRenderer.enableScriptDeferring(arc, false);
      }
      // For RuntimeExceptions and Errors, make sure we don't
      // just drop the error on the ground during PPR requests.
      // ViewHandler.renderView() would be a much, much better place to
      // put this code.  But sadly, ServletExceptions generally
      // swallow whatever they contain instead of exposing it
      // as a cause (at least in OC4J 9.0.4 and Tomcat 5.0)
      catch (RuntimeException re)
      {
        _LOG.severe("Error during partial-page rendering", re);
        throw re;
      }
      catch (Error error)
      {
        _LOG.severe("Error during partial-page rendering", error);
        throw error;
      }
      finally
      {
        context.setResponseWriter(saved);
      }

      renderAtEnd(context, arc);
      _renderPartialScripts(context, arc, scriptBufferingWriter);
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

      _encodeServiceScripts(context);

      renderContent(context, arc, component, bean);
      renderAtEnd(context, arc);
    }
  }

  protected void renderAtEnd(
    FacesContext context,
    AdfRenderingContext arc) throws IOException
  {
  }

  static private void _encodeServiceScripts(FacesContext context)
    throws IOException
  {
    RenderKit rk = context.getRenderKit();
    ExtendedRenderKitService service =
      Service.getService(rk, ExtendedRenderKitService.class);
    if (service != null)
    {
      service.encodeScripts(context);
    }
  }

  /**
   * Called to render the PPR loading scripts after the close
   * of the element.
   */
  private void _renderPartialScripts(
    FacesContext                  context,
    AdfRenderingContext           arc,
    ScriptBufferingResponseWriter scriptBufferingWriter) throws IOException
  {
    PartialPageContext pprContext = arc.getPartialPageContext();
    if (_shouldRenderPartialScripts(pprContext))
    {
      Iterator targets = pprContext.getRenderedPartialTargets();
      String scripts = scriptBufferingWriter.getBufferedScripts();

      ResponseWriter writer = context.getResponseWriter();
      // For XMLDOM, write out all the PPR scripts and the
      // PPR targets as XML elements
      if (supportsXMLDOM(arc))
      {
        writer.startElement("pprscripts", null);
        if (scripts != null)
        {
          writer.write("<![CDATA[");
          writer.writeText(scripts, null);
            writer.write("]]>");
        }
        writer.endElement("pprscripts");

        writer.startElement("pprtargets",null);
        while (targets.hasNext())
        {
          String target = (String) targets.next();
          if (pprContext.isPartialTargetRendered(target))
          {
            writer.startElement("pprtarget", null);
            writer.writeAttribute("targetid",target,null);
            writer.endElement("pprtarget");
          }
        }

        writer.endElement("pprtargets");
      }
      // Otherwise, write out the targets in a Javascript array,
      // and add a Javascript load handler to load everything up
      else
      {
        // Render the rest of the scripts if necessary
        writer.startElement("script", null);
        writer.writeAttribute("id", _PARTIAL_SCRIPTS_ID, null);
        renderScriptTypeAttribute(context, arc);

        // We comment out all of the script contents to avoid
        // executing the scripts in the iframe.  Our
        // _partialChange() onload handler will explicitly execute
        // the scripts in the parent window's context.
        writer.writeText("/*", null);

        // Render scripts
        if (scripts != null)
          writer.writeText(scripts, null);

        // Close the comment
        writer.writeText("*/", null);

        writer.endElement("script");

        Iterator libraries = scriptBufferingWriter.getBufferedLibraries();

        writer.startElement("script", null);
        XhtmlRenderer.renderScriptTypeAttribute(context, arc);

        writer.writeText("var ", null);
        writer.writeText(_PARTIAL_PAGE_LIBRARIES_VAR, null);
        writer.writeText("=[", null);

        boolean firstRenderedLibrary = true;
        if (libraries != null)
        {
          while (libraries.hasNext())
          {
            if (firstRenderedLibrary)
              firstRenderedLibrary = false;

            String libraryURI = libraries.next().toString();
            writer.writeText("'", null);
            writer.writeText(libraryURI, null);
            writer.writeText("',", null);
          }
        }

        // And include ScriptEval too.
        writer.writeText("'", null);
        writer.writeText(context.getExternalContext().getRequestContextPath(),
                         null);
        writer.writeText(LibraryScriptlet.getBaseLibURL(), null);
        String versionedLibraryName =
          LibraryScriptlet.getLibraryNameWithVersion(context,
                                                     _SCRIPT_EVAL_LIBRARY_NAME);
        writer.writeText(versionedLibraryName, null);
        writer.writeText(".js'", null);

        writer.writeText("];", null);

        writer.writeText("var ", null);
        writer.writeText(_PARTIAL_PAGE_TARGETS_VAR, null);
        writer.writeText("=[", null);

        // Loop through the partial targets and write out ids for any
        // rendered targets.
        boolean firstRenderedTarget = true;

        while (targets.hasNext())
        {
          String target = (String) targets.next();
          if (pprContext.isPartialTargetRendered(target))
          {
            if (firstRenderedTarget)
              firstRenderedTarget = false;
            else
              writer.writeText(",", null);

            writer.writeText("\'", null);
            writer.writeText(target, null);
            writer.writeText("\'", null);
          }
        }
        writer.writeText("];", null);

        writer.writeText(_ON_LOAD_START, null);
        // =-=AEW Deleted navForm code once here
        writer.writeText(")", null);


        writer.endElement("script");
      }
    }
  }

  static private boolean _shouldRenderPartialScripts(
    PartialPageContext pprContext)
  {
    return pprContext.getRenderedPartialTargets().hasNext();
  }

  protected boolean isEmbedded()
  {
    return true;
  }

  // Is this a partial page rendering pass?
  protected static boolean isPartialPass(AdfRenderingContext arc)
  {
    return (PartialPageUtils.isPartialRenderingPass(arc));
  }

  protected void renderPPRSupport(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    _renderPartialIFrame(context, arc, component);

    // Render anything that will be needed to block clicks when the
    // partial render is in progress
    _renderPartialBlocking(context, arc, component);

    // Render the element that is needed to download scripts from
    // the iframe into the main page.
    _renderLibraryDownloadElement(context, arc);

    // render var for caching downloaded libraries on client
    // so that we only load them once.
    XhtmlUtils.addLib(context, arc, _PARTIAL_CACHE_LIBRARY_SCRIPTLET);
  }

  // Renders the iframe used to make partial page requests.
  private void _renderPartialIFrame(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component) throws IOException
  {
    // Note: For now we just use the simplest possible strategy -
    // we always use a single iframe for all PPR requests.  We
    // might want to consider using more than one iframe, if we
    // expect that we'll need to support multiple simultaneous requests.
    // Also, we might want to avoid rendering the iframe at all
    // if we can determine that the page does not contain any
    // PPR targets.  But for now we always render one iframe since
    // this seems easiest and shouldn't add any significant overhead
    // to the generated content.
    ResponseWriter writer = context.getResponseWriter();
    if (supportsXMLDOM(arc))
    {
      //If it's Pocket PC 2003, iframe is not supported so generate
      // span which should be hidden
      writer.startElement("span", component);
      writer.writeAttribute("id", _PARTIAL_IFRAME_ID, null);
      writer.writeAttribute("style", "visibility:hidden", null);
      writer.endElement("span");
    }
    else
    {
      writer.startElement("iframe", component);
      writer.writeAttribute("id", _PARTIAL_IFRAME_ID, null);
      writer.writeAttribute("name", _PARTIAL_IFRAME_ID, null);
      writer.writeAttribute("frameborder", "0", null);

      // Apps bug 3324943 appsacess title attribute required for iframe reported
      // by oac with ppr - it also complains about missing attr 'longdesc'
      writer.writeAttribute("longdesc","#", null);    //accessibility
      writer.writeAttribute("title","", null);        //accessibility

      // Apps bug #2824158, users were getting a security warning with an empty
      // iframe when using https. This just puts some dummy content in the
      // iframe.
      if (isIE(arc))
      {
        writer.writeAttribute("src",
                              getBaseImageUri(context, arc)
                                + XhtmlRenderer.TRANSPARENT_GIF,
                              null);
      }
      else
      {
        writer.writeAttribute("src", "about:blank", null);
      }

      Object width = "0";
      Object height = "0";

      ExternalContext external = context.getExternalContext();
      boolean visible = "true".equalsIgnoreCase(external.getInitParameter(
                             XhtmlConstants.DEBUG_PARTIAL_RESPONSES_PARAM));
      if (visible)
      {
        width = "500";
        height = "100";
      }
      else
      {
        // Just to make sure that the iframe doesn't take up any
        // space at the top of the page, use absolute positioning
        // to force it off the screen
        writer.writeAttribute("style",
                "position:absolute;top:-100px;visibility:hidden",
                null);
      }

      writer.writeAttribute("width", width, null);
      writer.writeAttribute("height", height, null);

      writer.endElement("iframe");

      if (visible)
      {
        writer.startElement("div", component);
        writer.endElement("div");
      }
    }
  }

  // Renders the DIV element which is used to block user input during the
  // handling of a partial update.
  private static void _renderPartialBlocking(
    FacesContext        context,
    AdfRenderingContext arc,
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


  // Renders the DIV element which is used to download script libraries
  // for IE.
  private static void _renderLibraryDownloadElement(
    FacesContext context,
    AdfRenderingContext arc) throws IOException
  {
    // This is to download the libraries from the iframe into the
    // main page on IE.
    if (isIE(arc))
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("div", null);
      writer.writeAttribute("id", "_adfDownload", null);
      writer.writeAttribute("style", "behavior:url(#default#download)", null);

      // Hide the div so that it doesn't occupy any real estate
      renderStyleClass(context, arc, XhtmlConstants.HIDDEN_LABEL_STYLE_CLASS);

      writer.endElement("div");
    }
  }

  private static class PartialCacheLibraryScriptlet extends Scriptlet
  {
    static public Scriptlet sharedInstance()
    {
      return _sInstance;
    }

    public Object getScriptletKey()
    {
      return _PARTIAL_CACHE_LIBRARY_SCRIPTLET;
    }

    protected void outputScriptletContent(
      FacesContext context,
      AdfRenderingContext arc) throws IOException
    {
      ResponseWriter writer = context.getResponseWriter();

      writer.writeText("var _cachedLibs;", null);
    }

    static private final Scriptlet _sInstance =
            new PartialCacheLibraryScriptlet();
  }

  // Name for our Scriptlet
  private static final String _PARTIAL_CACHE_LIBRARY_SCRIPTLET =
          "PartialCacheLibraryScriptlet";
  static
  {
    // Register our scriptlet
    PartialCacheLibraryScriptlet.sharedInstance().registerSelf();
  }


  // ID for the single iframe used for partial page rendering
  private static final String _PARTIAL_IFRAME_ID  = "_pprIFrame";
  private static final String _PARTIAL_SCRIPTS_ID = "_pprScripts";
  // Div element used for blocking
  private static final String _PARTIAL_DIV_ID  = "_pprBlockingDiv";
  private static final String _PARTIAL_DIV_CLICK_HANDLER =
          "return _pprConsumeClick(event);";
  private static final String _PARTIAL_DIV_EAT_KEY_HANDLER = "return false;";
  private static final String _PARTIAL_DIV_STYLE =
          "position:absolute;left:0;top:0;width:0;height:0;cursor:wait;";

  // JS variable used to store partial targets
  private static final String _PARTIAL_PAGE_TARGETS_VAR = "_pprTargets";

  // JS variable used to store javascript libraries
  private static final String _PARTIAL_PAGE_LIBRARIES_VAR = "_pprLibraries";

  private static final String _ON_LOAD_START = "_partialChange(";
  private static final String _SCRIPT_EVAL_LIBRARY_NAME = "ScriptEval";

  private static final ADFLogger _LOG = ADFLogger.createADFLogger(
   PanelPartialRootRenderer.class);
}
