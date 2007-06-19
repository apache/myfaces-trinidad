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

import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

/**
 * A proxy class to ask the e-mail demo for preferences information,
 * but only when we're inside the e-mail demo!
 */
public class PreferencesProxy
{
  public Object getProxy()
  {
    // If we're in the e-mail demo, use its preferences
    FacesContext context = FacesContext.getCurrentInstance();
    if ((context.getViewRoot() != null) &&
        (context.getViewRoot().getViewId().indexOf("/email/") >= 0))
    {
      ValueBinding vb =
        context.getApplication().createValueBinding("#{email.preferences}");
      return vb.getValue(context);
    }
    // If we are showing the SkinDemo page, get the skinFamily from the 
    // sessionScope.
    else if ((context.getViewRoot() != null) &&
        (context.getViewRoot().getViewId().indexOf("SkinDemo") >= 0))
    {
      ValueBinding vb =
        context.getApplication().createValueBinding("#{sessionScope}");
      return vb.getValue(context);     
    }
    // Otherwise, go to an empty map (blank preferences)
    else
      return Collections.EMPTY_MAP;

  }
}
