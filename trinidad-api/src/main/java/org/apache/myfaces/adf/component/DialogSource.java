/*
 * Copyright  2003-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.adf.component;

import javax.faces.component.ActionSource;

import javax.faces.el.MethodBinding;

import org.apache.myfaces.adf.event.ReturnListener;
import org.apache.myfaces.adf.event.LaunchListener;


/**
 * Extension of ActionSource for components that fire
 * dialogs, and need to be notified when the dialog
 * begins launching and completes.
 * 
 * @author The Oracle ADF Faces Team
 */
public interface DialogSource extends ActionSource
{
  public void addReturnListener(ReturnListener listener);
  public void removeReturnListener(ReturnListener listener);
  public ReturnListener[] getReturnListeners();
  public void setReturnListener(MethodBinding returnListener);
  public MethodBinding getReturnListener();

  public void addLaunchListener(LaunchListener listener);
  public void removeLaunchListener(LaunchListener listener);
  public LaunchListener[] getLaunchListeners();
  public void setLaunchListener(MethodBinding launchListener);
  public MethodBinding getLaunchListener();
}
