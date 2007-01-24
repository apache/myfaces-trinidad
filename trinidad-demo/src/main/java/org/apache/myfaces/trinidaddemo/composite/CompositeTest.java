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
package org.apache.myfaces.trinidaddemo.composite;

import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;

public class CompositeTest
{
  public CompositeTest()
  {
  }

  public UIComponent getDateInput()
  {
    return _dateInput;
  }

  public void setDateInput(UIComponent dateInput)
  {
    _dateInput = dateInput;
  }

  private UIComponent _dateInput = new ForceRendererType();

  /**
   * Turns out there's no easy way to simply replace the renderer
   * for a control like UIInput;  UIComponentTag will call setRendererType()
   * after consulting the "binding" attribute, overriding anything
   * done in either the getter or setter.  So, here's a subclass
   * that forces the renderer type to a constant.  The alternative
   * is writing a custom JSP tag to do the same, but then you
   * have to re-invent the wheel as far as the input tag goes.
   */
  static public class ForceRendererType extends UIInput
  {
    @Override
    public String getRendererType()
    {
      return "org.apache.myfaces.trinidaddemo.DateField";
    }
  }
}
