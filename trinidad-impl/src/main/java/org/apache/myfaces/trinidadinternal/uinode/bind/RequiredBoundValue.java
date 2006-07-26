/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.uinode.bind;

import org.apache.myfaces.adf.component.UIXComponent;
import org.apache.myfaces.adf.component.UIXEditableValue;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.ui.data.BoundValue;

 
/**
 * BoundValue that converts required attribute from 
 * boolean into value uix expects
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/bind/RequiredBoundValue.java#0 $) $Date: 10-nov-2005.18:50:08 $
 * @author The Oracle ADF Faces Team
 */
public class RequiredBoundValue implements BoundValue
{
  public RequiredBoundValue(UIXComponent ev)
  {
    _ev = ev;
  }

  public Object getValue(RenderingContext context)
  { 
    Object requiredObject = _ev.getAttributes().get(UIXEditableValue.REQUIRED_KEY);
    boolean isRequired = Boolean.TRUE.equals(requiredObject);
    Object showRequiredObject = _ev.getAttributes().get("showRequired");
    boolean isShowRequired = Boolean.TRUE.equals(showRequiredObject);

    if (isRequired)
      return UIConstants.REQUIRED_YES;
      
    return (isShowRequired 
                ? UIConstants.REQUIRED_UI_ONLY
                : UIConstants.REQUIRED_NO);
  }
  
  private final UIXComponent _ev;
}

