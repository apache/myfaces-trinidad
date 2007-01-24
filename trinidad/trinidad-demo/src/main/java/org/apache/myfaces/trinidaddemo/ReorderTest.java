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

import java.util.List;
import javax.faces.component.UIComponent;
import javax.faces.event.ActionEvent;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputText;

public class ReorderTest
{
  public void setPanel(UIComponent panel)
  {
    _panel = panel;
  }

  public UIComponent getPanel()
  {
    return _panel;
  }

  @SuppressWarnings("unchecked")
  public void add(ActionEvent event)
  {
    List<UIComponent> children = _panel.getChildren();
    CoreOutputText output = new CoreOutputText();
    output.setValue("Item " + (children.size() + 1));
    children.add(0, output);
  }

  @SuppressWarnings("unchecked")
  public void remove(ActionEvent event)
  {
    List<UIComponent> children = _panel.getChildren();
    children.remove(children.size() - 1);
  }

  @SuppressWarnings("unchecked")
  public void removeFirst(ActionEvent event)
  {
    List<UIComponent> children = _panel.getChildren();
    children.remove(0);
  }

  @SuppressWarnings("unchecked")
  public void rotate(ActionEvent event)
  {
    List<UIComponent> children = _panel.getChildren();
    UIComponent o = children.get(0);
    children.remove(0);
    children.add(o);
  }

  public void removeSeparator(ActionEvent event)
  {
    _panel.getFacets().remove("separator");
  }

  @SuppressWarnings("unchecked")
  public void setSeparator(ActionEvent event)
  {
    CoreOutputText output = new CoreOutputText();
    output.setValue("New Separator");
    _panel.getFacets().put("separator", output);
  }
  
  private UIComponent _panel;
}
