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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputText;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidadinternal.util.nls.StringUtils;

public class OutputTextRenderer extends ValueRenderer
{
  public OutputTextRenderer()
  {
    super(CoreOutputText.TYPE);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);

    _truncateAtKey = type.findKey("truncateAt");
    _escapeKey = type.findKey("escape");
    _descriptionKey = type.findKey("description");
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext    arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    if (canSkipRendering(context, arc, comp))
      return;

    ResponseWriter rw = context.getResponseWriter();
    String value = getConvertedString(context, comp, bean);
    boolean escape = getEscape(bean);

    if (escape)
    {
      rw.startElement("span", comp);
      
      renderId(context, comp);
      renderAllAttributes(context, arc, bean);

      _renderDescription(context, arc, bean);

      if (value != null)
      {
        int truncateAt = getTruncateAt(bean);
        if (truncateAt > 0)
        {
          value = StringUtils.truncateString(value, truncateAt);
        }
        
        rw.writeText(value, "value");
      }

      rw.endElement("span");
    }
    else
    {
      if (value != null)
        rw.write(value);
    }
  }


  protected boolean getEscape(FacesBean bean)
  {
    Object o = bean.getProperty(_escapeKey);
    if (o == null)
      o = _escapeKey.getDefault();

    return !Boolean.FALSE.equals(o);
  }

  protected int getTruncateAt(FacesBean bean)
  {
    Object o = bean.getProperty(_truncateAtKey);
    if (o == null)
      o = _truncateAtKey.getDefault();
    
    return ((Number) o).intValue();
  }

  protected Object getDescription(FacesBean bean)
  {
    return bean.getProperty(_descriptionKey);
  }

  private void _renderDescription(
    FacesContext        context,
    RenderingContext arc,
    FacesBean           bean
    ) throws IOException
  {
    if (isInaccessibleMode(arc))
      return;

    Object label = getDescription(bean);
    if (label == null)
      return;

    // Do not attempt to render this label if the underlying
    // platform does not support hidden labels
    if (!HiddenLabelUtils.supportsHiddenLabels(arc))
      return;

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("span", null);
    renderStyleClass(context, arc, SkinSelectors.HIDDEN_LABEL_STYLE_CLASS);
    writer.writeText(label, null);
    writer.endElement("span");
  }

  private PropertyKey _truncateAtKey;
  private PropertyKey _escapeKey;
  private PropertyKey _descriptionKey;
}
