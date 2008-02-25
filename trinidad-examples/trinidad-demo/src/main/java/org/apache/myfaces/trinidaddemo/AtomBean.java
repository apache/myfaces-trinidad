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

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

public class AtomBean implements java.io.Serializable
{
  String name;
  String symbol;
  int number;
  String group;

  public String action()
  {
    FacesContext context = FacesContext.getCurrentInstance();
    FacesMessage message = new FacesMessage("Clicked on Chemical " + getName());
    context.addMessage(null, message);
    return null;
  }

  public AtomBean()
  {
  }

  public String getName()
  {
    return name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public String getSymbol()
  {
    return symbol;
  }

  public void setSymbol(String symbol)
  {
    this.symbol = symbol;
  }

  public int getNumber()
  {
    return number;
  }

  public void setNumber(int number)
  {
    this.number = number;
  }

  public String getGroup()
  {
    return group;
  }

  public void setGroup(String group)
  {
    this.group = group;
  }
  
  
}
