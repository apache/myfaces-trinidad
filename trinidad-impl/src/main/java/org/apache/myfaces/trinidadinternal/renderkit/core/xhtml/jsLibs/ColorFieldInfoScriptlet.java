/*
 * Copyright  2002-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.core.xhtml.jsLibs;

import java.awt.Color;

import java.io.IOException;

import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.OutputUtils;

import org.apache.myfaces.adfinternal.style.util.CSSUtils;

/**
 * Scriptlet for adding color field information.

 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/jsLibs/ColorFieldInfoScriptlet.java#0 $) $Date: 10-nov-2005.19:02:43 $
 * @author The Oracle ADF Faces Team
 */
public class ColorFieldInfoScriptlet extends Scriptlet
{
  static public final String COLOR_FIELD_INFO_KEY = "ColorFieldInfo";

  static public Scriptlet sharedInstance()
  {
    return _sInstance;
  }

  public Object getScriptletKey()
  {
    return COLOR_FIELD_INFO_KEY;
  }

  /**
   * @todo Rebuild in Faces-major land
   */
  protected void outputScriptletContent(
    FacesContext        context,
    AdfRenderingContext arc) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    
    writer.writeText("_cfTransIconURL='", null);
    writer.writeText(
       XhtmlRenderer.getAbsoluteImageUri(context,
                       arc,
                       XhtmlConstants.COLOR_PALETTE_TRANSPARENT_ICON_NAME),
       null);

    writer.writeText("';", null);

    writer.writeText("_cfOpaqueIconURL='", null);
    writer.writeText(
             XhtmlRenderer.getAbsoluteImageUri(context,
                                               arc,
                                               XhtmlRenderer.TRANSPARENT_GIF),
             null);
    writer.writeText("';", null);

    writer.writeText("_cfBgColor='", null);
    Color bgColor = OutputUtils.getBackgroundColor(arc);

    writer.writeText(CSSUtils.getColorValue(bgColor), null);
    writer.writeText("';", null);
  }

  private ColorFieldInfoScriptlet()
  {
  }

  static private final Scriptlet _sInstance = new ColorFieldInfoScriptlet();
}
