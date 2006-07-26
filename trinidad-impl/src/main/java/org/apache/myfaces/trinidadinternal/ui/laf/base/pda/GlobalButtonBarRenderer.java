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
package org.apache.myfaces.adfinternal.ui.laf.base.pda;

import java.io.IOException;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;
import org.apache.myfaces.adfinternal.ui.beans.MarlinBean;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/pda/GlobalButtonBarRenderer.java#0 $) $Date: 10-nov-2005.18:54:54 $
 * @author The Oracle ADF Faces Team
 */
public class GlobalButtonBarRenderer extends org.apache.myfaces.adfinternal.ui.laf.base.xhtml.GlobalButtonBarRenderer
{

  protected void renderBetweenIndexedChildren(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    renderChild(context, _SPACER);
  }

  protected void renderDefaultCellAttributes(
    RenderingContext context,
    UINode           child) throws IOException
  {
    renderAttribute(context, "valign", "bottom");
  }

  // space to render between each child
  private static final MarlinBean _SPACER = new MarlinBean(SPACER_NAME);
  static
  {
    _SPACER.setAttributeValue(WIDTH_ATTR, "10");
    _SPACER.setAttributeValue(HEIGHT_ATTR, "1");
  }

}
