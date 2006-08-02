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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.core.input.CoreInputColor;
import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;

public class InputColorRenderer extends InputLabelAndMessageRenderer
{

  public InputColorRenderer()
  {
    super(CoreInputColor.TYPE);
  }  

  protected String getRootStyleClass(FacesBean bean)  
  {
    return "af|inputColor";
  }  
    
  protected FormInputRenderer getFormInputRenderer()
  {
    return _simpleInputColor;
  }
  
  protected String getLabelFor(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean)
  {
    if (_isCompact(bean))
      return null;
    return super.getLabelFor(context, arc, component, bean);
  }
  
  private boolean _isCompact(FacesBean bean)
  {
    FacesBean.Type type = CoreInputColor.TYPE;
    PropertyKey compactKey = type.findKey("compact");
    Object o = bean.getProperty(compactKey);
    if (o == null)
      o = compactKey.getDefault();

    return Boolean.TRUE.equals(o);
  }
  
  private SimpleInputColorRenderer _simpleInputColor =
     new SimpleInputColorRenderer();
}
