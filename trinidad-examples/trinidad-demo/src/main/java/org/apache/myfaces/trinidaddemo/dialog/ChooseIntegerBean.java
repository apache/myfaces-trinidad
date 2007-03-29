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
package org.apache.myfaces.trinidaddemo.dialog;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.event.ReturnEvent;

public class ChooseIntegerBean
{
  public Integer getValue1()
  {
    return _value1;
  }

  public void setValue1(Integer value1)
  {
    _value1 = value1;
  }

  public Integer getValue2()
  {
    return _value2;
  }

  public void setValue2(Integer value2)
  {
    _value2 = value2;
  }

  public void sayHello(ReturnEvent event)
  {
    FacesMessage message = new FacesMessage("Hello!");
    FacesContext.getCurrentInstance().addMessage(null, message);
  }

  private Integer _value1;
  private Integer _value2;
}
