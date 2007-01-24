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
package org.apache.myfaces.trinidaddemo.survey;

public class SurveyPage implements java.io.Serializable
{
  public SurveyPage()
  {
  }
  
  public SurveyPage(String viewId, String label)
  {
    setViewId(viewId);
    setLabel(label);
    setDisabled(false);
  }

  public void setViewId(String viewId)
  {
    _viewId = viewId;
  }


  public String getViewId()
  {
    return _viewId;
  }

  public void setOutcome(String outcome)
  {
    _outcome = outcome;
  }


  public String getOutcome()
  {
    return _outcome;
  }

  public void setLabel(String label)
  {
    _label = label;
  }


  public String getLabel()
  {
    return _label;
  } 


  public void setDisabled(boolean disabled)
  {
    _disabled = disabled;
  }


  public boolean isDisabled()
  {
    return _disabled;
  } 
  
  private String _viewId;
  private String _outcome;
  private String _label;
  private boolean _disabled;


}
