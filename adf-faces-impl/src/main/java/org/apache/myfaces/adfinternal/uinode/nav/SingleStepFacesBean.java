/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.uinode.nav;

import org.apache.myfaces.adf.component.UIXSingleStep;
import org.apache.myfaces.adfinternal.uinode.UINodeFacesBean;
import org.apache.myfaces.adfinternal.uinode.bind.ClientIdBoundValue;
import org.apache.myfaces.adfinternal.uinode.bind.PropertyBoundValue;

import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.ui.collection.AttributeMap;

public class SingleStepFacesBean extends UINodeFacesBean
{
  protected AttributeMap createAttributeMap(String componentFamily)
  {
    AttributeMap attrMap = super.createAttributeMap(componentFamily);
    attrMap.setAttribute(UIConstants.NAME_ATTR,
                         new ClientIdBoundValue(getUIXComponent()));
    attrMap.setAttribute(UIConstants.FORM_SUBMITTED_ATTR, Boolean.TRUE);
    attrMap.setAttribute(UIConstants.VALUE_ATTR,
                         new PropertyBoundValue(this,
                                                UIXSingleStep.SELECTED_STEP_KEY));
    attrMap.setAttribute(UIConstants.MAX_VALUE_ATTR,
                         new PropertyBoundValue(this,
                                                UIXSingleStep.MAX_STEP_KEY));
    attrMap.setAttribute(UIConstants.TYPE_TEXT_ATTR,
                         new PropertyBoundValue(this,
                                                UIXSingleStep.TEXT_KEY));
    return attrMap;
  }
}
