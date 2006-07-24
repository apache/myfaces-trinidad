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
package org.apache.myfaces.adfinternal.ui.laf.base.xhtml;

import java.io.IOException;

import org.apache.myfaces.adfinternal.agent.AdfFacesAgent;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;


/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/TableLayoutRenderer.java#0 $) $Date: 10-nov-2005.18:54:16 $
 * @author The Oracle ADF Faces Team
 */
public class TableLayoutRenderer extends XhtmlLafRenderer
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
        
    renderAttribute(context, node, "width",  WIDTH_ATTR);

    // For bug 1772696, if summary has not been set we render an empty summary
    // attribute on TableLayoutBeans
    if (!isInaccessibleMode(context))
    {
      Object summary = node.getAttributeValue(context, SUMMARY_ATTR);
      renderAttribute(context, "summary", (summary==null) ? "" : summary);
    }

    Object tableCap = getAgentCapability(context, AdfFacesAgent.CAP_TABLES);
    
    boolean supportsAdvancedAttrs = false;
    boolean supportsAdvanced      = false;
    
    if (tableCap != null)
    {
      supportsAdvanced = (AdfFacesAgent.TABLES_CAP_ADVANCED == tableCap);
      supportsAdvancedAttrs = supportsAdvanced ||
                              (AdfFacesAgent.TABLES_CAP_ADVANCED_ATTRS == tableCap);
                              
      if (supportsAdvancedAttrs)
      {          
        renderAttribute(context, node, "border", BORDER_WIDTH_ATTR, ZERO);
        renderAttribute(context, node, "cellspacing", CELL_SPACING_ATTR, ZERO);
        renderAttribute(context, node, "cellpadding", CELL_PADDING_ATTR, ZERO);
        
        if (supportsAdvanced)
        {
          renderAttribute(context, node, "height", HEIGHT_ATTR);
        }
      }
    }
  }

  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    return "table";
  }
}
