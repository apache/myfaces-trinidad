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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.share.config.Configuration;
import org.apache.myfaces.trinidad.context.LocaleContext;

import org.apache.myfaces.trinidadinternal.ui.ElementRenderer;
import org.apache.myfaces.trinidadinternal.ui.NodeRole;
import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.RoledRenderer;
import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.UINode;
import org.apache.myfaces.trinidadinternal.ui.partial.PartialPageRendererUtils;

import org.apache.myfaces.trinidadinternal.ui.laf.base.PreAndPostRenderer;
import org.apache.myfaces.trinidadinternal.ui.laf.base.BaseLafRenderer;

/**
 * Renderer for rendering the root document element
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/HtmlRenderer.java#0 $) $Date: 10-nov-2005.18:53:56 $
 * @author The Oracle ADF Faces Team
 */
public class HtmlRenderer extends ElementRenderer
  implements RoledRenderer, PreAndPostRenderer
{
  public NodeRole getNodeRole(
    UIXRenderingContext context,
    UINode           node)
  {
    return XhtmlLafConstants.DOCUMENT_ROLE;
  }

  @Override
  public void prerender(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    // write the correct doctype - this has to be in render(),
    // because the doctype must be written before _anything_ -
    // including comments
    String docType = getDocType(context, node);

    if (docType != null)
    {
      if ( PartialPageRendererUtils.isPartialRenderingPass(context)
              && XhtmlLafRenderer.supportsPartialRendering(context)
              && BaseLafRenderer.supportsXMLDOM(context))
      {
        // do nothing
        ;
      }
      else
      {
        context.getResponseWriter().write(docType);
      }
    }

    super.prerender(context, node);

    // render the document meta data named child
    renderNamedChild(context, node, UIConstants.META_CONTAINER_CHILD);
  }

  @Override
  public void postrender(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    super.postrender(context, node);
  }

  /**
   * Subclasses should override to return their doctype
   */
  protected String getDocType(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // See bug 1893192 - we don't want to render the DTD
    // in Mozilla until we can fix our code to work in their
    // no-quirks mode.
    // however, the fix for bug 2342217 allows us to render the DTD again
    //      if (context.getAgent().getAgentApplication() ==
    //          Agent.APPLICATION_MOZILLA)
    //        return null;

    if (_hasFrameSet(context, node))
    {
      return getFrameSetDocType(context);
    }
    else
    {
      return getDocumentDocType(context);
    }
  }


  /**
   * Returns the document type to use when rendering a frame set, as
   * opposed to a document.
   */
  protected String getFrameSetDocType(
    UIXRenderingContext context
    )
  {
    if (isXMLDocument(context))
    {
      return XHTML_FRAMESET_DOCTYPE;
    }
    else
    {
      return HTML_FRAMESET_DOCTYPE;
    }
  }


  /**
   * Returns the document type to use when rendering a document, as opposed
   * to a frameset.
   */
  protected String getDocumentDocType(
    UIXRenderingContext context
    )
  {
    // default to transitional, rather than strict
    if (isXMLDocument(context))
    {
      return XHTML_TRANSITIONAL_DOCTYPE;
    }
    else
    {

      if (Boolean.TRUE.equals(context.getConfiguration().
                getProperty(Configuration.DISABLE_STANDARDS_MODE)))
        return HTML_TRANSITIONAL_DOCTYPE;
      else
        return HTML_STRICT_DOCTYPE;
    }
  }

  @Override
  protected String getElementName(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return "html";
  }

  /**
   * Determines whether we have a frameset UINode as a child
   * for determining which doctype to return
   */
  private boolean _hasFrameSet(
    UIXRenderingContext context,
    UINode           node
    )
  {
    if (XhtmlLafRenderer.supportsFrames(context))
    {
      int childCount = node.getIndexedChildCount(context);

      for (int i = 0; i < childCount; i++)
      {
        UINode currChild = node.getIndexedChild(context, i);

        if ((currChild.getNamespaceURI() == UIConstants.MARLIN_NAMESPACE) &&
            UIConstants.FRAME_BORDER_LAYOUT_NAME.equals(currChild.getLocalName()))
        {
          return true;
        }
      }
    }

    return false;
  }

  @Override
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    //
    // Render the document namespace
    //
    if (isXMLDocument(context))
    {
      String documentNamespace = getDocumentNamespace();

      if (documentNamespace != null)
      {
        writer.writeAttribute("xmlns", documentNamespace, null);
      }
    }

    LocaleContext localeContext = context.getLocaleContext();

    String direction = localeContext.isRightToLeft()
          ? "rtl"
          : "ltr";
    writer.writeAttribute("dir", direction, null);

    // render the correct language
    String ianaLocalString = localeContext.getIANALocaleString();

    if (ianaLocalString != null)
    {
      writer.writeAttribute(getLangAttrName(context), ianaLocalString, null);
    }
  }


  /**
   * Returns the name of the language attribute
   */
  protected String getLangAttrName(
    UIXRenderingContext context)
  {
    return "xml:lang";
  }

  /**
   * Returns the XML namespace to use for this document
   */
  protected String getDocumentNamespace()
  {
    return "http://www.w3.org/1999/xhtml";
  }


  /**
   * Returns true if we are rendering an XML document
   */
  protected boolean isXMLDocument(
    UIXRenderingContext context
    )
  {
    String contentType = null;
  	contentType = context.getResponseWriter().getContentType();

    return "text/xml".equals(contentType) ||
           "application/xhtml+xml".equals(contentType) ||
           "application/xml".equals(contentType);
  }


  protected static final String HTML_TRANSITIONAL_DOCTYPE =
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">";
  /**
   * Note that the following docType does not work in IE6. see bug 2342217
   *
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">";
  **/

  protected static final String HTML_STRICT_DOCTYPE =
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">";

  // loose plus FRAMESET instead of BODY
  protected static final String HTML_FRAMESET_DOCTYPE =
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">";

  protected static final String XHTML_STRICT_DOCTYPE =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">";

  protected static final String XHTML_TRANSITIONAL_DOCTYPE =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">";

  protected static final String XHTML_FRAMESET_DOCTYPE =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">";

  // basic xhtml doctype
  protected static final String BASIC_XHTML_DOCTYPE =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Basic 1.0//EN\" \"http://www.w3.org/TR/xhtml-basic/xhtml-basic10.dtd\">";

  protected static final String XHTML_NAMESPACE = "http://www.w3.org/1999/xhtml";
}
