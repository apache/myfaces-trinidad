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

import javax.faces.event.ActionEvent;
import org.apache.myfaces.trinidad.event.PollEvent;

/**
 * Bean for poll component demos.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-demo/src/main/java/oracle/adfdemo/view/faces/PollBean.java#1 $) $Date: 16-aug-2005.15:12:28 $
 */

public class PollBean implements java.io.Serializable
{
  public PollBean()
  {
    _POLL_COUNT = 0;
  }
  
  public void onPoll(PollEvent event)
  {
    ++_POLL_COUNT;
  }
  
  public int getPollCount()
  {
    return _POLL_COUNT;
  }
  
  public void resetPoll(ActionEvent event)
  {
    _POLL_COUNT = 0;
  }
  
  private static int _POLL_COUNT;
}
