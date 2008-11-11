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

import java.io.IOException;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.event.PollEvent;
import org.apache.myfaces.trinidad.model.DefaultBoundedRangeModel;

/**
 * Bean for progress steps demos.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-demo/src/main/java/oracle/adfdemo/view/faces/ProgressStepsBean.java#1 $) $Date: 16-aug-2005.15:12:28 $
 */
public class ProgressStepsBean extends ProgressBean 
{
  public List<String> getProgressSteps()
  {
    return _PROGRESS_STEPS;
  }
  
  public void onPoll(PollEvent event)
  {
    if ( __model != null && (__model.getMaximum() <= __model.getValue()) )
    {
      //pu: This means the background task is complete.
      //  End the task and navigate off to a different page.
      endProcess();
      try
      {
        ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
        ec.redirect("../components/progressEnd.jspx?taskStatus=completed");
      }
      catch(IOException ioe)
      {
        _LOG.log(Level.WARNING, "Could not redirect", ioe);
      }
      catch (RuntimeException re)
      {
        _LOG.log(Level.SEVERE, "Could not redirect", re);
        throw re;
      }
    }
  }
  
  @Override
  protected void prepare()
  {
    __model = new DefaultBoundedRangeModel(-1, 6);
    //pu: simulate asynchronous model updates on a different thread
    __processThread = new ProcessThread(1500, 1);
    __processThread.start();
  }
  
  static private List<String> _PROGRESS_STEPS;
  
  static private final Logger _LOG = Logger.getLogger(
    ProgressStepsBean.class.getName());
  
  static
  {
    _PROGRESS_STEPS = new ArrayList<String>();
    _PROGRESS_STEPS.add("Checking for latest version");
    _PROGRESS_STEPS.add("Checking available disk space");
    _PROGRESS_STEPS.add("Copying files");
    _PROGRESS_STEPS.add("Analyzing dependencies");
    _PROGRESS_STEPS.add("Install in progress");
    _PROGRESS_STEPS.add("Building icons and shortcuts");
  }

}
