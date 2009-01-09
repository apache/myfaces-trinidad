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
package org.apache.myfaces.trinidaddemo;

import java.util.Collections;

import javax.el.ValueExpression;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

/**
 * A proxy class to ask the e-mail demo for preferences information,
 * but only when we're inside the e-mail demo!
 */
public class PreferencesProxy
{
  public Object getProxy()
  {
    FacesContext context = FacesContext.getCurrentInstance();
    String viewId = _getViewId(context);

    if (viewId != null)
    {
      // Certain views have their own preferences
      // storage.  Figure out which preferences
      // we are proxying.
      String preferencesExpression = null;
      
      if (viewId.indexOf("/email/") >= 0)
        preferencesExpression = "#{email.preferences}";
      else if (viewId.indexOf("SkinDemo") >= 0)
        preferencesExpression = "#{sessionScope}";
      else if (viewId.indexOf("accessibilityProfileDemo") >= 0)
        preferencesExpression = "#{accProfileDemo}";

      if (preferencesExpression != null)
      {
        ValueExpression ve =
          context.getApplication().getExpressionFactory().createValueExpression(context.getELContext(), preferencesExpression, Object.class);
        return ve.getValue(context.getELContext());
      }
    }
    
    return Collections.EMPTY_MAP;
  }

  // Returns the current viewId.
  private String _getViewId(FacesContext context)
  {
    UIViewRoot viewRoot = context.getViewRoot();
    if (viewRoot != null)
      return viewRoot.getViewId();
    
    return null;
  }
}
