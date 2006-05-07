/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.taglib.listener;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.FacesBeanImpl;
import org.apache.myfaces.adf.bean.PropertyKey;

/**
 * 
 * @todo Look at moving to org.apache.myfaces.adf.event
 * @todo Extending FacesBean is very lame if we make this
 *   class part of our public API, but the FacesBean API
 *   would otherwise require a private subclass of FacesBeanImpl.
 *   We need a better way out.
 * @author The Oracle ADF Faces Team
 */
public class SetActionListener extends FacesBeanImpl
  implements ActionListener, StateHolder
{
  static public final FacesBean.Type TYPE = new FacesBean.Type();
  static public final PropertyKey FROM_KEY =
    TYPE.registerKey("from");
  // Must be a ValueBinding
  static public final PropertyKey TO_KEY =
    TYPE.registerKey("to");

  static
  {
    TYPE.lock();
  }

  public SetActionListener()
  {
  }

  public void processAction(ActionEvent event)
  {
    ValueBinding to = getValueBinding(TO_KEY);
    if (to != null)
    {
      to.setValue(FacesContext.getCurrentInstance(),
                  getFrom());
    }
  }

  public Object getFrom()
  {
    return getProperty(FROM_KEY);
  }

  public void setFrom(Object from)
  {
    setProperty(FROM_KEY, from);
  }

  public Type getType()
  {
    return TYPE;
  }

  public boolean isTransient()
  {
    return false;
  }

  public void setTransient(boolean newTransientValue)
  {
    throw new UnsupportedOperationException();
  }

  // saveState() and restoreState() come from FacesBeanImpl
}
