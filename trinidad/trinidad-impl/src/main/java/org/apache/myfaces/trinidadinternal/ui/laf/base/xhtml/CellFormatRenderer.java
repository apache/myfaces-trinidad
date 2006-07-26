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

import org.apache.myfaces.trinidadinternal.ui.RenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/CellFormatRenderer.java#0 $) $Date: 10-nov-2005.18:53:45 $
 * @author The Oracle ADF Faces Team
 */
public class CellFormatRenderer extends XhtmlLafRenderer
{  
  /**
   * Renders attributes of the current node.
   */
  protected void renderAttributes(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);
    
    renderHAlign(context, node);
    
    if (supportsVAlign(context))
    {
      renderAttribute(context, node, "valign",  V_ALIGN_ATTR);
    }    
    
    renderAttribute(context, node, "abbr", SHORT_TEXT_ATTR);
    renderAttribute(context, node, "headers", HEADERS_ATTR);
    renderAttribute(context, node, "colspan", COLUMN_SPAN_ATTR);
    renderAttribute(context, node, "rowspan", ROW_SPAN_ATTR);
  }

  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {    
    return isHeaderCell(context, node) 
            ? TABLE_HEADER_ELEMENT
            : TABLE_DATA_ELEMENT ; 
  }
  
  private boolean isHeaderCell(
    RenderingContext context,
    UINode           node
    )
  {
    return Boolean.TRUE.equals(node.getAttributeValue(context, HEADER_ATTR));    
  }
}

