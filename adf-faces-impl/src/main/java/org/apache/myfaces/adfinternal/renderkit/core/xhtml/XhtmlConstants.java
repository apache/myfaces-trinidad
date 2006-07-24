/*
 * Copyright  2005,2006 The Apache Software Foundation.
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

public interface XhtmlConstants
  extends org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafConstants,
          org.apache.myfaces.adfinternal.ui.UIConstants
{
  public static final String DEBUG_PARTIAL_RESPONSES_PARAM =
    "org.apache.myfaces.adfinternal.DEBUG_PARTIAL_RESPONSES";

  static public String STYLES_CACHE_DIRECTORY = "/adf/styles/cache/";
  static public String OUTPUT_MODE_PORTLET = FACET_PORTLET;

  //
  // Copied from BaseDesktopConstants
  //

  // =-=AEW Is this still a legit style class???
  public static final String HEADER_NEST_STYLE_CLASS = 
    "p_OraHeaderNest";
  // Header style classes
  // if icon attribute is set, this is the style for it.
  // I wouldn't normally have style in the name, but I don't want it to be
  // confused with a icon.
  public static final String AF_PANEL_HEADER_ICON_STYLE_CLASS =
    "af|panelHeader::icon-style";
  
  public static final String AF_PANEL_BOX_LIGHT_STYLE_CLASS =
    "af|panelBox::light";
  public static final String AF_PANEL_BOX_MEDIUM_STYLE_CLASS =
    "af|panelBox::medium";
  public static final String AF_PANEL_BOX_DARK_STYLE_CLASS =
    "af|panelBox::dark";
  public static final String AF_PANEL_BOX_TRANSPARENT_STYLE_CLASS =
    "af|panelBox::transparent";
  public static final String AF_PANEL_BOX_CONTENT_LIGHT_STYLE_CLASS =
    "af|panelBox::content-light";
  public static final String AF_PANEL_BOX_CONTENT_MEDIUM_STYLE_CLASS =
    "af|panelBox::content-medium";
  public static final String AF_PANEL_BOX_CONTENT_DARK_STYLE_CLASS =
    "af|panelBox::content-dark";
  public static final String AF_PANEL_BOX_CONTENT_TRANSPARENT_STYLE_CLASS =
    "af|panelBox::content-transparent";
  public static final String AF_PANEL_BOX_HEADER_STYLE_CLASS =
    "af|panelBox::header";
  public static final String AF_PANEL_BOX_BODY_STYLE_CLASS =
    "af|panelBox::body";
  
  // ============= Output Document ==============
  public static final String AF_OUTPUT_DOCUMENT_STYLE_CLASS = 
    "af|outputDocument";
  public static final String AF_OUTPUT_DOCUMENT_PARAGRAPH_STYLE_CLASS = 
    "af|outputDocument::paragraph";
  public static final String AF_OUTPUT_DOCUMENT_SEPARATOR_STYLE_CLASS = 
    "af|outputDocument::separator";
  public static final String AF_OUTPUT_DOCUMENT_TITLE_STYLE_CLASS = 
    "af|outputDocument::title";

  // =============PROPERTY NAMES================

  public static final String AF_PANELHEADER_INDENT_CONTENT = 
    "af|panelHeader-ora-indent-content";
  
  // ============= Html elements ================
  public static final String PARAGRAPH_ELEMENT = "p";
}
