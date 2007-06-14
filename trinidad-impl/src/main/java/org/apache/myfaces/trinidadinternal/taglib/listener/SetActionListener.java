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
package org.apache.myfaces.trinidadinternal.taglib.listener;

import javax.el.ValueExpression;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.FacesBeanImpl;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * JavaServer Faces version 1.2 a <code>setPropertyActionListener</code>, which provides the 
 * same functionality. In JSF 1.2 days this class should be <code>deprecated</code>.
 *
 * @todo Look at moving to org.apache.myfaces.trinidad.event
 * @todo Extending FacesBean is very lame if we make this
 *   class part of our public API, but the FacesBean API
 *   would otherwise require a private subclass of FacesBeanImpl.
 *   We need a better way out.
 */
public class SetActionListener extends FacesBeanImpl
  implements ActionListener, StateHolder
{
  static public final FacesBean.Type TYPE = new FacesBean.Type();
  static public final PropertyKey FROM_KEY =
    TYPE.registerKey("from");
  // Must be a ValueExpression
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
    ValueExpression to = getValueExpression(TO_KEY);
    if (to != null)
    {
      Object from = getProperty(FROM_KEY);
      try
      {
        to.setValue(FacesContext.getCurrentInstance().getELContext(), from);
      }
      catch (RuntimeException e)
      {
        if (_LOG.isWarning())
        {
          ValueExpression fromExpression = getValueExpression(FROM_KEY);
          String mes = "Error setting:'"+to.getExpressionString() +
            "' to value:"+from;
          if (fromExpression != null)
            mes += " from:'"+fromExpression.getExpressionString()+"'";
            
          _LOG.warning(mes, e);
        }
        throw e;
      }
    }
  }

  public void setFrom(ValueExpression from)
  {
    if (from.isLiteralText())
      setProperty(FROM_KEY, from.getValue(null));
    else
      setValueExpression(FROM_KEY, from);
  }

  public void setTo(ValueExpression to)
  {
    if (to.isLiteralText())
      throw new IllegalArgumentException("setActionListener's 'to' must be an EL expression");
    
    setValueExpression(TO_KEY, to);
  }

  @Override
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
  
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SetActionListener.class);
}
