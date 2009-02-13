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

import javax.faces.application.ViewHandler;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.faces.event.ValueChangeEvent;

import org.apache.myfaces.trinidad.component.UIXOutput;
import org.apache.myfaces.trinidad.context.RequestContext;

public class PartialDemoUtilBean
{
  public void action(ActionEvent action)
  {
    // Just update the string which says when the last update was.
    _status.setLinkUpdate();
  }

  // This is called for the resetButton
  public void reset(ActionEvent action)
  {
    _status.reset();
    _resetList();
  }

  public void valueChanged(ValueChangeEvent vce)
  {
    Object newValue = vce.getNewValue();
    UIComponent component = vce.getComponent();

    String rendererType = component.getRendererType();

    // For these first components the listeners have registered themselves
    // by setting the partialTriggers attribute. So we just update the model.
    if (rendererType.equals("org.apache.myfaces.trinidad.Checkbox"))
    {
      _status.setChecked((Boolean) newValue);
      _status.incrementCheckBoxUpdateCount();
    }
    else if (rendererType.equals("org.apache.myfaces.trinidad.Radio"))
    {
      if (Boolean.TRUE.equals(newValue))
      {
        String text = (String) component.getAttributes().get("text");
        _status.setSelectBooleanState(text);
      }
      else if (newValue instanceof String)
        _status.setSelectOneState((String) newValue);
    }
    else if (rendererType.equals("org.apache.myfaces.trinidad.Text"))
    {
      if (newValue instanceof String)
        _status.setTextValue((String) newValue);
    }
    else if (rendererType.equals("org.apache.myfaces.trinidad.Choice"))
    {
      if (newValue instanceof String)
        _status.setChoiceInt((String) newValue);
    }

    // This component illustrates a method of dynamically adding a
    // partialTarget (i.e. without setting the partialTriggers attribute). It
    // updates a component binding and adds the updated component directly to
    // the list of partial targets.
    else if (rendererType.equals("org.apache.myfaces.trinidad.Listbox"))
    {
      _listUpdate.setValue(component.getAttributes().get("value"));
      _addTarget(_listUpdate);
    }
  }

  public UIXOutput getListUpdate()
  {
    return _listUpdate;
  }

  public void setListUpdate(UIXOutput listUpdate)
  {
    _listUpdate = listUpdate;
  }

  public PartialDemoStatusBean getStatus()
  {
    return _status;
  }

  public void setStatus(PartialDemoStatusBean status)
  {
    _status = status;
  }

  public void navigate(ValueChangeEvent vce)
  {
    Object newValue = vce.getNewValue();
    if ((newValue != null) && !"".equals(newValue))
    {
      FacesContext fContext = FacesContext.getCurrentInstance(); 
      ViewHandler vh = fContext.getApplication().getViewHandler(); 
      UIViewRoot root = vh.createView(fContext, newValue.toString()); 
      fContext.setViewRoot(root); 
    }
  }

  private void _resetList()
  {
    _listUpdate.setValue("nothing yet.");
    _addTarget(_listUpdate);
  }

  private void _addTarget(UIComponent target)
  {
    RequestContext adfContext = RequestContext.getCurrentInstance();
    adfContext.addPartialTarget(target);
  }

  private PartialDemoStatusBean _status;
  private UIXOutput _listUpdate;
}
