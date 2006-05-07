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
package org.apache.myfaces.adfinternal.ui.laf.base.desktop;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/BorderLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:55:10 $
 * @author The Oracle ADF Faces Team
 */
public class BorderLayoutRenderer extends 
                                org.apache.myfaces.adfinternal.ui.laf.base.xhtml.BorderLayoutRenderer
{

  /**
   * Returns the default marign indent to use if no CELL_PADDING_ATTR
   * is specified
   */
  protected int getDefaultMarginIndent(
    RenderingContext context,
    UINode           node
    )
  {
    return _MARGIN_INDENT;
  }

  // # of pixels to use for the margin
  private static final int _BASE_MARGIN_INDENT = 12;
  private static final int _IE_DEFAULT_MARGIN = 10;
  private static final int _MARGIN_INDENT = _BASE_MARGIN_INDENT - 
                                            _IE_DEFAULT_MARGIN;
}
