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
package org.apache.myfaces.trinidaddemo.nav;

import java.io.Serializable;
import java.util.List;


public class DemoNavigationItem implements Serializable
{
  public DemoNavigationItem()
  {}


  public void setLabel(String label)
  {
    _label = label;
  }

  public String getLabel()
  {
    return _label;
  }

  public void setOutcome(String outcome)
  {
    _outcome = outcome;
  }

  public String getOutcome()
  {
    return _outcome;
  }

  public void setViewId(String viewId)
  {
    _viewId = viewId;
  }

  public String getViewId()
  {
    return _viewId;
  }

  // calling this 'ico' instead of 'icon' due to tree bug
  public void setIco(String icon)
  {
    _icon = icon;
  }

  // calling this 'ico' instead of 'icon' due to tree bug
  public String getIco()
  {
    return _icon;
  }


  public void setDestination(String destination)
  {
    _destination = destination;
  }

  public String getDestination()
  {
    return _destination;
  }

  public List<?> getChildren()
  {
    return _children;
  }

  public void setChildren(List<?> children)
  {
    _children = children;
  }




  private String  _label       = null;
  private String  _outcome     = null;
  private String  _viewId      = null;
  private String  _destination = null;
  private String  _icon        = null;
  private List<?> _children    = null;



}
