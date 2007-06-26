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
package org.apache.myfaces.trinidad.event;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.FacesBeanImpl;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * ActionListener that supports getting a value from
 * one binding and setting it on another.
 */
public class SetActionListener implements ActionListener, StateHolder
{
  
  /**
   * Creates a SetActionListener.
   */
  public SetActionListener()
  {
    _bean = new Bean();
  }

  /**
   * Gets the value from the "from" property and sets it on 
   * the ValueBinding for the "to" property
   */
  public void processAction(ActionEvent event)
  {
    ValueBinding to = _bean.getValueBinding(Bean.TO_KEY);
    if (to != null)
    {
      Object from = getFrom();
      try
      {
        to.setValue(FacesContext.getCurrentInstance(), from);
      }
      catch (RuntimeException e)
      {
        if (_LOG.isWarning())
        {
          ValueBinding fromBinding = _bean.getValueBinding(Bean.FROM_KEY);
          String mes = "Error setting:'"+to.getExpressionString() +
            "' to value:"+from;
          if (fromBinding != null)
            mes += " from:'"+fromBinding.getExpressionString()+"'";
            
          _LOG.warning(mes, e);
        }
        throw e;
      }
    }
  }

  public ValueBinding getValueBinding(String name)
  {
    PropertyKey key = Bean.TYPE.findKey(name);
    if (key == null)
      return null;

    return _bean.getValueBinding(key);
  }

  public void setValueBinding(String name, ValueBinding binding)
  {
    PropertyKey key = Bean.TYPE.findKey(name);
    if (key == null)
      throw new IllegalArgumentException();
    _bean.setValueBinding(key, binding);
  }

  public Object getFrom()
  {
    return _bean.getProperty(Bean.FROM_KEY);
  }

  public void setFrom(Object from)
  {
    _bean.setProperty(Bean.FROM_KEY, from);
  }

  public Object saveState(FacesContext context)
  {
    return _bean.saveState(context);
  }

  public void restoreState(FacesContext context, Object state)
  {
    _bean.restoreState(context, state);
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
  static private class Bean extends FacesBeanImpl
  {
    static public final FacesBean.Type TYPE = new FacesBean.Type();
    static public final PropertyKey FROM_KEY =
      TYPE.registerKey("from");
    // Must be a ValueBinding
    static public final PropertyKey TO_KEY =
      TYPE.registerKey("to");


    @Override
    public Type getType()
    {
      return TYPE;
    }

    static
    {
      TYPE.lock();
    }
  }

  private Bean _bean;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SetActionListener.class);
}
