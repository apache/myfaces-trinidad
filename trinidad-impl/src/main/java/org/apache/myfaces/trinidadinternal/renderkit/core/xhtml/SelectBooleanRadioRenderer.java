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

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.component.core.input.CoreSelectBooleanRadio;

public class SelectBooleanRadioRenderer extends InputLabelAndMessageRenderer
{

  public SelectBooleanRadioRenderer()
  {
    super(CoreSelectBooleanRadio.TYPE);
    
  } 
  
  protected String getRootStyleClass(FacesBean bean)  
  {
    return "af|selectBooleanRadio";
  }
  
  /**
   * selectBooleanRadio should not render a &lt;label&gt; on itself
   * if "text" is set.
   */ 
  protected boolean hasOwnLabel(FacesBean bean)
  {
    String text = _simpleSelectBooleanRadio.getText(bean);
    
    if (text != null)
      return true;
      
    return false;
  }

  protected boolean isIndented()
  {
    return true;
  }  
 
  protected FormInputRenderer getFormInputRenderer()
  {
    return _simpleSelectBooleanRadio;
  }  

  private SimpleSelectBooleanRadioRenderer _simpleSelectBooleanRadio =
     new SimpleSelectBooleanRadioRenderer();
}
