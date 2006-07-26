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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import javax.faces.context.ResponseWriter;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.output.CoreObjectSpacer;

import org.apache.myfaces.trinidadinternal.renderkit.AdfRenderingContext;

public class ObjectSpacerRenderer extends XhtmlRenderer
{
  public ObjectSpacerRenderer()
  {
    super(CoreObjectSpacer.TYPE);
  }
  
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _heightKey = type.findKey("height");
    _widthKey = type.findKey("width");
  }

  protected void encodeBegin(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    String id;
    if (shouldRenderId(context, comp))
      id = getClientId(context, comp);
    else
      id = null;
    
    String width = getWidth(bean);
    String height = getHeight(bean);
    
    if (width == null)
    {
      if (height == null)
      {
        ResponseWriter writer = context.getResponseWriter();
        writer.startElement("span", comp);
        writer.writeAttribute("id", id, null);
        writer.endElement("span");
      }
      else
      {
        renderVerticalSpacer(context, height, id, comp);
      }
    }
    else
    {
      renderDecorativeIcon(context, 
                           arc,
                           XhtmlRenderer.TRANSPARENT_GIF,
                           width,
                           height,
                           id,
                           null, 
                           comp);
    }
  }

  protected String getHeight(FacesBean bean)
  {
    return toString(bean.getProperty(_heightKey));
  }

  protected String getWidth(FacesBean bean)
  {
    return toString(bean.getProperty(_widthKey));
  }

  private PropertyKey _heightKey;
  private PropertyKey _widthKey;
}
