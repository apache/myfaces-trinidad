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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.component.core.input.CoreInputHidden;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;

public class InputHiddenRenderer extends EditableValueRenderer
{
  public InputHiddenRenderer()
  {
    super(CoreInputHidden.TYPE);
  }
  
  protected boolean wasSubmitted(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  public boolean getRendersChildren()
  {
    return true;
  }

  protected final void encodeAll(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("input", component);
    rw.writeAttribute("type", "hidden", null);
    String id = getClientId(context, component);
    rw.writeAttribute("id", id, "id");
    rw.writeAttribute("name", id, "id");
    rw.writeAttribute("value",
                      getConvertedString(context, component, bean),
                      "value");
    rw.endElement("input");

    FormData fd = arc.getFormData();
    if (fd != null)
      fd.addRenderedValue(id);

  }
}
