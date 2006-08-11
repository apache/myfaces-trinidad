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
package org.apache.myfaces.trinidadinternal.uinode.nav;

import org.apache.myfaces.trinidad.component.core.nav.CoreCommandButton;

import org.apache.myfaces.trinidadinternal.ui.data.BoundValue;
import org.apache.myfaces.trinidadinternal.uinode.FireActionBoundValue;
import org.apache.myfaces.trinidadinternal.uinode.UINodeFacesBean;
import org.apache.myfaces.trinidadinternal.uinode.bind.PropertyBoundValue;

import org.apache.myfaces.trinidadinternal.ui.UIConstants;
import org.apache.myfaces.trinidadinternal.ui.collection.AttributeMap;


public class CommandButtonFacesBean extends UINodeFacesBean
{
  @Override
  protected AttributeMap createAttributeMap(String componentFamily)
  {
    AttributeMap attrMap = super.createAttributeMap(componentFamily);
    BoundValue unvalidatedBV = new PropertyBoundValue(this,
                                             CoreCommandButton.IMMEDIATE_KEY);

    BoundValue partialBV = new PropertyBoundValue(this,
                                CoreCommandButton.PARTIAL_SUBMIT_KEY);

    attrMap.setAttribute(UIConstants.PRIMARY_CLIENT_ACTION_ATTR,
                         new FireActionBoundValue(getUIXComponent(),
                                                  unvalidatedBV,
                                                  partialBV));

    return attrMap;
  }
}
