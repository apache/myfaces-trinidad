/*
 * Copyright  2006,2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.html.layout;

import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafConstants;

/**
 * Renderer for ShowManyAccordion
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/html/layout/CoreShowManyAccordionRenderer.java#0 $) $Date: 10-nov-2005.19:01:13 $
 * @author The Oracle ADF Faces Team
 */
public class CoreShowManyAccordionRenderer extends CoreShowOneAccordionRenderer
{

  String getContainerStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWMANYACCORDION_CONTAINER_STYLE_CLASS;
  }

  String getContentStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWMANYACCORDION_CONTENT_STYLE_CLASS;
  }

  String getHeaderDisabledStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWMANYACCORDION_HEADER_DISABLED_STYLE_CLASS;
  }

  String getHeaderExpanedStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWMANYACCORDION_HEADER_EXPANDED_STYLE_CLASS;
  }

  String getHeaderCollapsedStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWMANYACCORDION_HEADER_COLLAPSED_STYLE_CLASS;
  }

  String getLinkDisabledStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWMANYACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS;
  }

  String getLinkEnabledStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWMANYACCORDION_TITLE_LINK_STYLE_CLASS;
  }

  boolean isDiscloseMany()
  {
    return true;
  }
}
