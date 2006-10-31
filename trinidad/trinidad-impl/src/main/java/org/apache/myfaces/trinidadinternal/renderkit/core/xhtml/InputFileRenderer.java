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

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.input.CoreInputFile;

import org.apache.myfaces.trinidad.context.RenderingContext;

public class InputFileRenderer extends InputLabelAndMessageRenderer
{

  public InputFileRenderer()
  {
    super(CoreInputFile.TYPE);
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _simpleInputFile = new SimpleInputFileRenderer(type);
  }
  
  @Override
  protected final void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    // =-=AEW inputFile is currently disabled for PDAs.  But this should
    // run off of an agent property.
    if (!isPDA(arc))
    {
      super.encodeAll(context, arc, component, bean);
    }
  }

  @Override
  protected String getRootStyleClass(FacesBean bean)  
  {
    return "af|inputFile";
  }
  
  @Override
  protected FormInputRenderer getFormInputRenderer()
  {
    return _simpleInputFile;
  }
  
  /**
   * 
   * @param bean
   * @return false, since inputFile does not support the readOnly attribute
   */
  @Override
  protected boolean isReadOnly(FacesBean bean)
  {
    return false;
  }

  private SimpleInputFileRenderer _simpleInputFile;
}
