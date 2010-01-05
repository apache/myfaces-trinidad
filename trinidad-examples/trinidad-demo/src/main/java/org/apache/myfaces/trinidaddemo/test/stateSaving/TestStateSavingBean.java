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

package org.apache.myfaces.trinidaddemo.test.stateSaving;

import java.util.Calendar;
import java.util.Date;

import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.component.core.input.CoreChooseDate;
import org.apache.myfaces.trinidad.component.core.layout.CoreShowDetail;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputText;


public class TestStateSavingBean
{

  public void incrementMutable(ActionEvent ae)
  {
    FacesContext context = FacesContext.getCurrentInstance();
    CoreChooseDate cd = (CoreChooseDate)context.getViewRoot().findComponent("cdMutable");
    Date maxDate = cd.getMaxValue();

    Calendar cal = Calendar.getInstance();
    cal.setTime(maxDate);
    cal.add(Calendar.DAY_OF_YEAR, 1);
    maxDate.setTime(cal.getTimeInMillis());
  }
  
  public void incrementWithSet(ActionEvent ae)
  {
    FacesContext context = FacesContext.getCurrentInstance();
    CoreChooseDate cd = (CoreChooseDate)context.getViewRoot().findComponent("cdSet");
    Date maxDate = cd.getMaxValue();

    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(maxDate.getTime());
    cal.add(Calendar.DAY_OF_YEAR, 1);
    cd.setMaxValue(new Date(cal.getTimeInMillis()));
  }
  
  public void addComponent(ActionEvent ae)
  {
    System.out.println("Adding a showDetail child");
    CoreShowDetail showDetail = new CoreShowDetail();
    CoreOutputText outputText = new CoreOutputText();
    outputText.setValue("showDetail Content");
    showDetail.getChildren().add(outputText);
    FacesContext context = FacesContext.getCurrentInstance();
    context.getViewRoot().findComponent("groupLayout").getChildren().add(showDetail);
  }

}
