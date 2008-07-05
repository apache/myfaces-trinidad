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
import java.util.TimeZone;

import javax.faces.component.UIComponent;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.html.HtmlHead;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.myfaces.trinidadinternal.share.config.UIXCookie;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.render.CoreRenderer;

/**
 * Renderer for meta data section of the document--a.k.a <head>.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/HeadRenderer.java#0 $) $Date: 10-nov-2005.19:01:29 $
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
  
  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _titleKey = type.findKey("title");
  }

  @Override
  protected void encodeBegin(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
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

  @Override
  protected void encodeEnd(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.endElement("head");
  }

  protected String getTitle(FacesBean bean)
  {
    return toString(bean.getProperty(_titleKey));
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
    writer.writeAttribute("content", "Apache MyFaces Trinidad", null);
    writer.endElement("meta");
  }



  static private void _writeCookieScript(
   FacesContext context, RenderingContext arc)
    throws IOException
  { 
    if (_needsCookieScript(context, arc))
    {
      XhtmlUtils.addLib(context, arc, "defaultTimeZone");
    }
  }

  static private boolean _needsCookieScript(
    FacesContext context,
    RenderingContext arc)
  {
    ExternalContext externalContext = context.getExternalContext();
    
    // Disable the Cookie script for portlets
    // =-=AEW Right or wrong?
    String outputMode = arc.getOutputMode();
    if (XhtmlConstants.OUTPUT_MODE_PORTLET.equals(outputMode))
      return false;

    // Do not need the cookie script when we have a PartialPageContext
    if (arc.getPartialPageContext() != null)
      return false;
    
    Object request = externalContext.getRequest();
    Object response = externalContext.getResponse();
    if ((request instanceof HttpServletRequest) &&
        (response instanceof HttpServletResponse))
    {
      UIXCookie cookie = UIXCookie.getUIXCookie(
         (HttpServletRequest) request,
         (HttpServletResponse) response, false);

      return (((cookie == null) || 
               _timeZoneIsDefaulting(cookie)) &&
              _supportsUIXCookie());
    }
    
    return false;
  }

  static private boolean _timeZoneIsDefaulting(UIXCookie cookie)
  {
    TimeZone tz = cookie.getTimeZone();
    return ((tz == null) || 
            tz.getID().startsWith("GMT"));
  }

  static private boolean _supportsUIXCookie()
  {
    // =-=AEW We used to have a configuration hook for disabling
    // the cookie.
    return true;
  }

  private CoreRenderer _styleSheetRenderer = new StyleSheetRenderer()
  {
    // Don't render the ID
    @Override
    protected void renderId(
      FacesContext context,
      UIComponent  component)
    {
    }
  };

  private PropertyKey _titleKey;
}
