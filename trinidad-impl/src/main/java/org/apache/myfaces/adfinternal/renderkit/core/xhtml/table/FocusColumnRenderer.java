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
package org.apache.myfaces.adfinternal.renderkit.core.xhtml.table;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.component.UIXTreeTable;

import org.apache.myfaces.adf.component.core.data.CoreColumn;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.OutputUtils;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.XhtmlConstants;
import org.apache.myfaces.adfinternal.skin.icon.Icon;


public class FocusColumnRenderer extends SpecialColumnRenderer
{
  protected String getHeaderText(FacesBean bean)
  {
    AdfRenderingContext arc = AdfRenderingContext.getCurrentInstance();
    return arc.getTranslatedString("af_treeTable.FOCUS_COLUMN_HEADER");
  }
  
  protected String getHeaderStyleClass(TableRenderingContext tContext)
  {
    return XhtmlConstants.AF_COLUMN_HEADER_ICON_STYLE;
  }

  protected String getFormatType(FacesBean bean)
  {
    return CoreColumn.ALIGN_CENTER;
  }

  protected void renderKids(FacesContext          context,
                            AdfRenderingContext   arc,
                            TableRenderingContext trc,
                            UIComponent           column) throws IOException
  {
    TreeTableRenderingContext ttrc = (TreeTableRenderingContext) trc;
  
    ResponseWriter writer = context.getResponseWriter();
    UIXTreeTable hGrid = ttrc.getUIXTreeTable();

    Object focusRowKey = hGrid.getFocusRowKey();
    assert focusRowKey != null;
    int focusPathSize = hGrid.getDepth(focusRowKey) + 1;

    // do not render a focus icon if the node is not expandable
    // do not render the focus icon if this is the first row
    if (hGrid.isContainer() && 
        (hGrid.getDepth() >= focusPathSize))
    {
      writer.startElement(XhtmlConstants.LINK_ELEMENT, null);

      if (supportsNavigation(arc))
      {
        String onclick = 
          TreeUtils.callJSFocusNode(hGrid, ttrc.getJSVarName());
        writer.writeAttribute(XhtmlConstants.HREF_ATTRIBUTE, "#", null);
        writer.writeAttribute(XhtmlConstants.ONCLICK_ATTRIBUTE, onclick, null);
      }

      _renderFocusIcon(context, arc, arc.getTranslatedString("af_treeTable.FOCUS_TIP"));

      writer.endElement(XhtmlConstants.LINK_ELEMENT);
    }
  }

  // Renders the focus icon  
  private void _renderFocusIcon(
    FacesContext fc,
    AdfRenderingContext arc,
    String           altText
    ) throws IOException
  {
    Icon icon = arc.getIcon(XhtmlConstants.AF_TREE_TABLE_FOCUS_ICON_NAME);

    if (icon != null)
    {
      // Render focus icon with embedded=true, since 
      // focus icon is always rendered within its own link.
      OutputUtils.renderIcon(fc, arc, icon, altText, null, true);
    }
  }
}
