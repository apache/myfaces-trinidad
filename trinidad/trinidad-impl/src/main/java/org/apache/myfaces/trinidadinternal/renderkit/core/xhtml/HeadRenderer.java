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
import java.util.TimeZone;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.html.HtmlHead;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.myfaces.trinidadinternal.share.config.UIXCookie;

import org.apache.myfaces.trinidadinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderer;

/**
 * Renderer for meta data section of the document--a.k.a <head>.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/HeadRenderer.java#0 $) $Date: 10-nov-2005.19:01:29 $
 * @author The Oracle ADF Faces Team
 */
public class HeadRenderer extends XhtmlRenderer
{ 
  public HeadRenderer()
  {
    this(HtmlHead.TYPE);
  }

  protected HeadRenderer(FacesBean.Type type)
  {
    super(type);
  }
  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _titleKey = type.findKey("title");
  }

  protected void encodeBegin(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    if (_skipRendering(arc))
      return;

    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("head", comp);
    renderId(context, comp);

    String title = getTitle(bean);
    if (title != null)
    {
      rw.startElement("title", null);
      rw.writeText(title, null);
      rw.endElement("title");
    }

    // Write the META generator tag        
    _writeGeneratorTag(context);

    delegateRenderer(context, arc, comp, bean, _styleSheetRenderer);

    _writeCookieScript(context, arc);
  }


  protected void encodeEnd(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    if (_skipRendering(arc))
      return;

    ResponseWriter rw = context.getResponseWriter();
    rw.endElement("head");
  }

  protected String getTitle(FacesBean bean)
  {
    return toString(bean.getProperty(_titleKey));
  }

  private boolean _skipRendering(AdfRenderingContext arc)
  {
    return (PartialPageUtils.isPartialRenderingPass(arc) &&
            PartialPageUtils.supportsPartialRendering(arc) &&
            supportsXMLDOM(arc));
  }

  /**
   * Writes the META generator tag that identifies the technology
   * generating the page.
   */
  static private void _writeGeneratorTag(FacesContext context)
    throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    
    writer.startElement("meta", null);
    writer.writeAttribute("name", "generator", null);
    writer.writeAttribute("content", "Oracle ADF Faces", null);
    writer.endElement("meta");
  }



  static private void _writeCookieScript(
   FacesContext context, AdfRenderingContext arc)
    throws IOException
  { 
    if (_needsCookieScript(context, arc))
    {
      XhtmlUtils.addLib(context, arc, "defaultTimeZone");
    }
  }

  static private boolean _needsCookieScript(
    FacesContext context,
    AdfRenderingContext arc)
  {
    // Disable the Cookie script for portlets
    // =-=AEW Right or wrong?
    if (XhtmlConstants.OUTPUT_MODE_PORTLET.equals(arc.getOutputMode()))
      return false;

    // Do not need the cookie script when we have a PartialPageContext
    if (arc.getPartialPageContext() != null)
      return false;
    
    Object request = context.getExternalContext().getRequest();
    Object response = context.getExternalContext().getResponse();
    if ((request instanceof HttpServletRequest) &&
        (response instanceof HttpServletResponse))
    {
      UIXCookie cookie = UIXCookie.getUIXCookie(
         (HttpServletRequest) request,
         (HttpServletResponse) response, false);

      return (((cookie == null) || 
               _timeZoneIsDefaulting(cookie)) &&
              _supportsUIXCookie(arc));
    }
    
    return false;
  }

  static private boolean _timeZoneIsDefaulting(UIXCookie cookie)
  {
    TimeZone tz = cookie.getTimeZone();
    return ((tz == null) || 
            tz.getID().startsWith("GMT"));
  }

  static private boolean _supportsUIXCookie(AdfRenderingContext arc)
  {
    // =-=AEW We used to have a configuration hook for disabling
    // the cookie.
    return true;
  }

  private CoreRenderer _styleSheetRenderer = new StyleSheetRenderer()
  {
    // Don't render the ID
    protected void renderId(
      FacesContext context,
      UIComponent  component)
    {
    }
  };

  private PropertyKey _titleKey;
}
