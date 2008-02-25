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
package org.apache.myfaces.trinidadinternal.config;

import javax.faces.FactoryFinder;
import javax.faces.application.Application;
import javax.faces.application.ApplicationFactory;
import javax.faces.context.FacesContext;
import javax.faces.el.ReferenceSyntaxException;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * A ValueBinding class that lazily parses the underlying EL expression
 * (in case the Application object is not yet available).  Unfortunately,
 * this implementation means that errors in the syntax of the EL
 * expression won't get detected until use.
 *
 */
public class LazyValueBinding extends ValueBinding
{
  /**
   * Create a ValueBinding;  if the Application is not yet
   * available, delay actual parsing until later.
   */
  static public ValueBinding createValueBinding(String expression)
  {
    ApplicationFactory factory = (ApplicationFactory)
      FactoryFinder.getFactory(FactoryFinder.APPLICATION_FACTORY);
    if (factory != null)
    {
      Application application = factory.getApplication();
      if (application != null)
      {
        try
        {
          return application.createValueBinding(expression);
        }
        catch (NullPointerException npe)
        {
          ;
          // In the Sun RI, JSF 1.0 and 1.1, creating a ValueBinding
          // when there isn't a FacesContext results in a NullPointerException.
          // Work around this bug.
        }
      }
    }

    return new LazyValueBinding(expression);
  }

  private LazyValueBinding(String expression)
  {
    _expression = expression;
  }

  @Override
  public Object getValue(FacesContext context)
  {
    return _getValueBinding().getValue(context);
  }

  @Override
  public void setValue(FacesContext context, Object value)
  {
    _getValueBinding().setValue(context, value);
  }

  @Override
  public boolean isReadOnly(FacesContext context)
  {
    return _getValueBinding().isReadOnly(context);
  }

  @Override
  public Class<?> getType(FacesContext context)
  {
    return _getValueBinding().getType(context);
  }

  @Override
  public String getExpressionString()
  {
    return _expression;
  }

  private ValueBinding _getValueBinding()
  {
    if (_binding == null)
    {
      ApplicationFactory factory = (ApplicationFactory)
        FactoryFinder.getFactory(FactoryFinder.APPLICATION_FACTORY);
      Application application = factory.getApplication();
      try
      {
        _binding = application.createValueBinding(_expression);
      }
      catch (ReferenceSyntaxException rse)
      {
        _LOG.warning(rse);
      }
    }

    return _binding;
  }

  private final String       _expression;
  private       ValueBinding _binding;

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(LazyValueBinding.class);
}
