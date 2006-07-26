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

import java.io.IOException;

import java.util.Map;
import java.util.HashMap;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.PropertyKey;
import org.apache.myfaces.adf.component.core.output.CoreOutputFormatted;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;

public class OutputFormattedRenderer extends ValueRenderer
{
  public OutputFormattedRenderer()
  {
    super(CoreOutputFormatted.TYPE);
  }
  
  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _styleUsageKey = type.findKey("styleUsage");
  }

  protected void encodeBegin(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("span", comp);
    
    renderId(context, comp);
    renderAllAttributes(context, arc, bean);
  }

  public void encodeEnd(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    String value = getConvertedString(context, comp, bean);
    renderFormattedText(context, value);
    rw.endElement("span");
  }

  protected String getStyleUsage(FacesBean bean)
  {
    return toString(bean.getProperty(_styleUsageKey));
  }

  protected String getStyleClass(FacesBean bean)
  {
    String styleClass = super.getStyleClass(bean);
    if (styleClass == null)
    {
      String usage = getStyleUsage(bean);
      if (usage != null)
      {
        styleClass = (String) _USAGES.get(usage);
      }
    }

    return styleClass;
  }

  static private final Map _USAGES = new HashMap();
  static
  {
    _USAGES.put(CoreOutputFormatted.STYLE_USAGE_IN_CONTEXT_BRANDING,
                XhtmlConstants.IN_CONTEXT_TEXT_STYLE_CLASS);
    _USAGES.put(CoreOutputFormatted.STYLE_USAGE_INSTRUCTION,
                XhtmlConstants.INSTRUCTION_TEXT_STYLE_CLASS);
    _USAGES.put(CoreOutputFormatted.STYLE_USAGE_PAGE_STAMP,
                XhtmlConstants.PAGE_STAMP_TEXT_STYLE_CLASS);
  }

  private PropertyKey _styleUsageKey;
}
