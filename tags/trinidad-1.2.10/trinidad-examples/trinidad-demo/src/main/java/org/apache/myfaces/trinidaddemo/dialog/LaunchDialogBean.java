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

import java.util.Date;
import java.util.Map;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXCommand;
import org.apache.myfaces.trinidad.component.UIXInput;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.ReturnEvent;
import org.apache.myfaces.trinidad.event.LaunchEvent;
import org.apache.myfaces.trinidad.event.PollEvent;

public class LaunchDialogBean
{
  public UIXInput getInput()
  {
    return _input;
  }

  public void setInput(UIXInput input)
  {
    _input = input;
  }

  public void addParameter(LaunchEvent event)
  {
    // Pass an integer into the dialog.  Some automatic
    // coercion would really help here (coming in JSF 1.2?)
    Object value = getInput().getValue();
    if (value != null)
    {
      try
      {
        Integer i = Integer.valueOf(value.toString());
        event.getDialogParameters().put("value", i);
      }
      catch (Exception e)
      {
      }
    }
  }

  public String doLaunch()
  {
    RequestContext afContext = RequestContext.getCurrentInstance();
    Map<String, Object> process = afContext.getPageFlowScope();
    process.put("lastLaunch", new Date());

    return "dialog:chooseInteger";
  }

  public void returned(ReturnEvent event)
  {
    String dialogViewId;
    if (event.getReturnValue() != null)
    {
      getInput().setSubmittedValue(null);
      getInput().setValue(event.getReturnValue());

      RequestContext afContext = RequestContext.getCurrentInstance();
      afContext.addPartialTarget(getInput());
      dialogViewId =  "/demos/successDialog.jspx";
    }
    else
    {
      dialogViewId =  "/demos/cancelledDialog.jspx";
    }


    // Launch a new, success dialog with a different width and height;
    // this shows how to do so by queueing a LaunchEvent.
    // (Here, we queue it to a dummy UIXCommand just so we don't
    // get in a fun infinite loop of ReturnEvents!)
    FacesContext context = FacesContext.getCurrentInstance();
    UIViewRoot root = context.getApplication().getViewHandler().createView(
                                    context, dialogViewId);
    LaunchEvent launchEvent = new LaunchEvent(getDummyCommand(), root);
    launchEvent.getWindowProperties().put("width", "200");
    launchEvent.getWindowProperties().put("height", "100");
    addParameter(launchEvent);
    launchEvent.queue();
  }


  public void poll(PollEvent event)
  {
    FacesContext context = FacesContext.getCurrentInstance();
    UIViewRoot root = context.getApplication().getViewHandler().createView(
                           context, "/demos/simpleDialog.jspx");

    // Launch a dialog with a call to RequestContext
    RequestContext afContext = RequestContext.getCurrentInstance();
    afContext.launchDialog(root, null, event.getComponent(), true, null);
    // Stop the poll from running
    event.getComponent().setRendered(false);
  }

  public UIXInput getTableInput()
  {
    return _tableInput;
  }

  public void setTableInput(UIXInput tableInput)
  {
    _tableInput = tableInput;
  }

  public void tableReturned(ReturnEvent event)
  {
    if (event.getReturnValue() != null)
    {
      getTableInput().setValue(event.getReturnValue());
      RequestContext afContext = RequestContext.getCurrentInstance();
      afContext.addPartialTarget(getTableInput());
    }
  }


  public UIXCommand getDummyCommand()
  {
    return _dummyCommand;
  }

  public void setDummyCommand(UIXCommand dummyCommand)
  {
    _dummyCommand = dummyCommand;
  }

  private UIXInput _input;
  private UIXInput _tableInput;
  private UIXCommand _dummyCommand;
}
