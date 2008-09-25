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
package org.apache.myfaces.trinidad.sandbox.component;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXComponentBase;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.util.ComponentUtils;

public abstract class UIXPartialRenderedTemplate extends UIXComponentBase
{
/**/  public abstract String[] getPartialTriggers();
/**/  public abstract String getFor();
/**/  public abstract boolean isAlwaysRendered();

  /**
   * @see org.apache.myfaces.trinidad.component.UIXComponentBase#decode(
   * javax.faces.context.FacesContext)
   */
  @Override
  public void decode(FacesContext context)
  {
    UIComponent comp;
    
    String forVal = getFor();
    if (forVal == null)
    {
      comp = getParent();
    }
    else
    {
      comp = ComponentUtils.findRelativeComponent(this, forVal);
    }

    if (comp != null)
    {
      RequestContext adfContext = RequestContext.getCurrentInstance();
      if (adfContext != null)
      {
        if (isAlwaysRendered())
        {
          adfContext.addPartialTarget(comp);
        }
        else
        {
          String[] partialTriggers = getPartialTriggers();
          if (partialTriggers != null)
          {
            adfContext.addPartialTriggerListeners(comp, partialTriggers);
          }
        }
      }
    }
  }
}
