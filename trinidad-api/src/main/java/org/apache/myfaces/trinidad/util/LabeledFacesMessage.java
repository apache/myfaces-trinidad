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
package org.apache.myfaces.trinidad.util;

import javax.faces.application.FacesMessage;
/**
 * Extension to FacesMessage which keeps track of the label on the component
 * that generated the message.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/util/LabeledFacesMessage.java#0 $) $Date: 10-nov-2005.19:08:38 $
 */
public class LabeledFacesMessage extends FacesMessage
{
  public LabeledFacesMessage()
  {
  }

  public LabeledFacesMessage(
    FacesMessage.Severity severity,
    String summary,
    String detail)
  {
    super(severity, summary, detail);
  }

  public LabeledFacesMessage(
    FacesMessage.Severity severity,
    String summary,
    String detail,
    Object label)
  {
    super(severity, summary, detail);
    _label = label;
  }

  public Object getLabel()
  {
    return _label;
  }

  public void setLabel(Object label)
  {
    _label = label;
  }

  private Object _label;
}
