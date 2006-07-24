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

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.component.html.HtmlFrameBorderLayout;
import org.apache.myfaces.adf.component.html.HtmlHtml;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;


/**
 * Renderer for rendering the root document element
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/HtmlRenderer.java#0 $) $Date: 10-nov-2005.19:01:30 $
 * @author The Oracle ADF Faces Team
 */
public class HtmlRenderer extends XhtmlRenderer
{
  /**
   * Identify if standards mode has been disabled.
   * =-=AEW Is this the correct place?
   */
  static public boolean isStandardsModeDisabled(FacesContext context)
  {
    String disableStandardsMode = 
      context.getExternalContext().getInitParameter(_DISABLE_STANDARDS_MODE);
    
    return ((disableStandardsMode != null) &&
            disableStandardsMode.equalsIgnoreCase("true"));
  }

  public HtmlRenderer()
  {
    this(HtmlHtml.TYPE);
  }

  protected HtmlRenderer(FacesBean.Type type)
  {
    super(type);
  }

  protected void encodeBegin(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    String docType = getDocType(context, comp);
    if (docType != null)
    {
      if ( PartialPageUtils.isPartialRenderingPass(arc) &&
           PartialPageUtils.supportsPartialRendering(arc) &&
           supportsXMLDOM(arc))
      {
        // don't write a doctype during XMLDOM PPR
        ;
      }
      else
      {
        writer.write(docType);
      }
    }

    writer.startElement("html", comp);

    boolean isXML = isXMLDocument(context);

    //
    // Render the document namespace
    //
    if (isXML)
    {
      String documentNamespace = getDocumentNamespace();

      if (documentNamespace != null)
      {
        writer.writeAttribute("xmlns", documentNamespace, null);
      }
    }

    String direction = arc.isRightToLeft() ? "rtl" : "ltr";
    writer.writeAttribute("dir", direction, null);

    // render the correct language
    String lang = arc.getLocaleContext().getIANALocaleString();
    if (lang != null)
    {
      if (isXML)
        writer.writeAttribute("xml:lang", lang, null);
      else
        writer.writeAttribute("lang", lang, null);
    }
  }

  public void encodeEnd(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.endElement("html");
  }

  /**
   * Subclasses should override to return their doctype
   */
  protected String getDocType(
    FacesContext context,
    UIComponent  component)
  {
    // See bug 1893192 - we don't want to render the DTD
    // in Mozilla until we can fix our code to work in their
    // no-quirks mode.
    // however, the fix for bug 2342217 allows us to render the DTD again
    //      if (context.getAgent().getAgentApplication() ==
    //          Agent.APPLICATION_MOZILLA)
    //        return null;

    if (_hasFrameSet(component))
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
    FacesContext context
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
    FacesContext context
    )
  {
    // default to transitional, rather than strict
    if (isXMLDocument(context))
    {
      return XHTML_TRANSITIONAL_DOCTYPE;
    }
    else
    {
      if (isStandardsModeDisabled(context))
        return HTML_TRANSITIONAL_DOCTYPE;
      else
        return HTML_STRICT_DOCTYPE;
    }
  }


  /**
   * Determines whether we have a frameset component as a child
   * for determining which doctype to return
   */
  private boolean _hasFrameSet(UIComponent component)
  {
    List children = component.getChildren();
    int childCount = component.getChildCount();
    
    for (int i = 0; i < childCount; i++)
    {
      UIComponent currChild = (UIComponent) children.get(i);
      
      if (HtmlFrameBorderLayout.COMPONENT_FAMILY.equals(currChild.getFamily()))
      {
        return true;
      }
    }

    return false;
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
    FacesContext context
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

  static private final String _DISABLE_STANDARDS_MODE=
    "org.apache.myfaces.adf.ENABLE_QUIRKS_MODE";
}
