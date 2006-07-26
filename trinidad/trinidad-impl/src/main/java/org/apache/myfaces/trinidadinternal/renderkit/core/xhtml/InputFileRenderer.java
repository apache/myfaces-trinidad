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

import org.apache.myfaces.trinidadinternal.agent.AdfFacesAgent;
import org.apache.myfaces.trinidadinternal.renderkit.AdfRenderingContext;

public class InputFileRenderer extends InputLabelAndMessageRenderer
{

  public InputFileRenderer()
  {
    super(CoreInputFile.TYPE);
  }
  
  protected final void encodeAll(
    FacesContext        context,
    AdfRenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    // =-=AEW inputFile is currently disabled for PDAs.  But this should
    // run off of an agent property.
    if (arc.getAgent().getAgentType() != AdfFacesAgent.TYPE_PDA)
    {
      super.encodeAll(context, arc, component, bean);
    }
  }

  protected String getRootStyleClass(FacesBean bean)  
  {
    return "af|inputFile";
  }
  
  protected FormInputRenderer getFormInputRenderer()
  {
    return _simpleInputFile;
  }
  
  /**
   * 
   * @param bean
   * @return false, since inputFile does not support the readOnly attribute
   */
  protected boolean isReadOnly(FacesBean bean)
  {
    return false;
  }
  
  private SimpleInputFileRenderer _simpleInputFile =
     new SimpleInputFileRenderer();
}
