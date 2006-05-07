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
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.FacesBeanImpl;
import org.apache.myfaces.adf.bean.PropertyKey;
import org.apache.myfaces.adf.context.AdfFacesContext;

/**
 * This action listener returns a value from a dialog or process.
 * @author The Oracle ADF Faces Team
 */
public class ReturnActionListener extends FacesBeanImpl
  implements ActionListener, StateHolder
{
  static public final FacesBean.Type TYPE = new FacesBean.Type();
  static public final PropertyKey VALUE_KEY =
    TYPE.registerKey("value");
  static
  {
    TYPE.lock();
  }

  public ReturnActionListener()
  {
  }

  public void processAction(ActionEvent event)
  {
    Object value = getValue();
    AdfFacesContext adf = AdfFacesContext.getCurrentInstance();
    adf.returnFromDialog(value, null);
  }

  public Object getValue()
  {
    return getProperty(VALUE_KEY);
  }

  public void setValue(Object value)
  {
    setProperty(VALUE_KEY, value);
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

}
