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

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXCommand;
import org.apache.myfaces.trinidad.component.UIXNavigationHierarchy;
import org.apache.myfaces.trinidad.context.RequestContext;

public class DemoCommandNavigationItemBean
{
  /**
   * Changes the selected state of all of the navigation items in the
   * parent component so that the clicked navigation item becomes
   * selected and the others become deselected.
   * @param event the ActionEvent associated with the action
   */
  @SuppressWarnings("unchecked")
  public void navigationItemAction(ActionEvent event)
  {
    UIComponent actionItem = event.getComponent();
    UIComponent parent = actionItem.getParent();
    while (! (parent instanceof UIXNavigationHierarchy) )
    {
      parent = parent.getParent();
      if (parent == null)
      {
        System.err.println(
          "Unexpected component hierarchy, no UIXNavigationHierarchy found.");
        return;
      }
    }

    List<UIComponent> children = parent.getChildren();
    for (UIComponent child : children)
    {
      FacesBean childFacesBean = ((UIXCommand) child).getFacesBean();
      FacesBean.Type type = childFacesBean.getType();
      PropertyKey selectedKey = type.findKey("selected");
      if (selectedKey != null)
      {
        childFacesBean.setProperty(selectedKey, (child == actionItem));
      }
    }

    RequestContext adfContext = RequestContext.getCurrentInstance();
    adfContext.addPartialTarget(parent);
  }
}
