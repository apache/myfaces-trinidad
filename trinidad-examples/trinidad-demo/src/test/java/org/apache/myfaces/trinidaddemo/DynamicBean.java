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

import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.component.core.layout.CorePanelBox;
import org.apache.myfaces.trinidad.component.core.nav.CoreCommandLink;

public class DynamicBean
{
  public DynamicBean()
  {
  }

  public void doSomething(ActionEvent event)
  {
    System.out.println("Received " + event);
  }

  @SuppressWarnings("unchecked")
  public CorePanelBox getPanel()
  {
    if (_panel == null)
    {
      FacesContext context = FacesContext.getCurrentInstance();

      _panel = new CorePanelBox();
      CoreCommandLink link = new CoreCommandLink();
      link.setText("Dynamic Link");

      MethodBinding actionListenerMethod = context.getApplication().
          createMethodBinding("#{dynamic.doSomething}",
                              new Class[]{ActionEvent.class}); 
      link.setActionListener(actionListenerMethod); 
      link.setId("TheLinkId");
      _panel.getChildren().add(link);
    }

    return _panel;
  }

  public void setPanel(CorePanelBox panel)
  {
    _panel = panel;
  }

  private CorePanelBox _panel;
}

