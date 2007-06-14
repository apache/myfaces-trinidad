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
package org.apache.myfaces.trinidad.bean;

import javax.el.ELException;
import javax.el.ValueExpression;

import javax.faces.context.FacesContext;
import javax.faces.el.EvaluationException;
import javax.faces.el.ValueBinding;

@Deprecated
public class ValueExpressionValueBinding
  extends ValueBinding
{
  public ValueExpressionValueBinding(ValueExpression ve)
  {
    _ve = ve;
  }

  public ValueExpression getValueExpression()
  {
    return _ve;
  }
  
  public Object getValue(FacesContext facesContext)
  {
    try
    {
      return _ve.getValue(facesContext.getELContext());
    }
    // Convert EL exceptions into EvaluationExceptions
    catch (ELException ee)
    {
      throw new EvaluationException(ee.getMessage(), ee.getCause());
    }    
  }

  public void setValue(FacesContext facesContext, Object object)
  {
    try
    {
      _ve.setValue(facesContext.getELContext(), object);
    }
    // Convert EL exceptions into EvaluationExceptions
    catch (ELException ee)
    {
      throw new EvaluationException(ee.getMessage(), ee.getCause());
    }    
  }

  public boolean isReadOnly(FacesContext facesContext)
  {
    try
    {
      return _ve.isReadOnly(facesContext.getELContext());
    }
    // Convert EL exceptions into EvaluationExceptions
    catch (ELException ee)
    {
      throw new EvaluationException(ee.getMessage(), ee.getCause());
    }    
  }

  public Class getType(FacesContext facesContext)
  {
    try
    {
      return _ve.getType(facesContext.getELContext());
    }
    // Convert EL exceptions into EvaluationExceptions
    catch (ELException ee)
    {
      throw new EvaluationException(ee.getMessage(), ee.getCause());
    }
  }
  
  private final ValueExpression _ve;
}
